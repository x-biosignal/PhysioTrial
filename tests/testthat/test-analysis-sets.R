make_analysis_participants <- function() {
  data.frame(
    id = as.character(seq_len(12L)),
    arm = c(rep(c("A", "B"), 5L), NA, NA),
    randomized = c(rep(TRUE, 10L), FALSE, FALSE),
    adherent = c(TRUE, TRUE, TRUE, FALSE, FALSE, rep(TRUE, 5L), NA, NA),
    completed = c(rep(TRUE, 5L), FALSE, rep(TRUE, 4L), NA, NA),
    outcome = c(seq_len(9L), NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

make_analysis_deviations <- function() {
  data.frame(
    participant_id = c("2", "3", "4", "4", "4"),
    major = c(FALSE, TRUE, TRUE, TRUE, TRUE),
    code = c("minor", "eligibility", "visit", "dose", "dose"),
    stringsAsFactors = FALSE
  )
}

test_that("ITT and PP partition the complete source population", {
  trial <- Trial("sets", c("A", "B"))
  participants <- make_analysis_participants()
  itt <- intentionToTreat(trial, participants)
  pp <- perProtocol(
    trial,
    participants,
    deviations = make_analysis_deviations(),
    adherent_col = "adherent",
    completed_col = "completed"
  )

  expect_s3_class(itt, "analysis_set")
  expect_identical(itt$members$participant_id, as.character(1:10))
  expect_identical(itt$excluded$participant_id, c("11", "12"))
  expect_identical(itt$excluded$reason, rep("not_randomized", 2L))
  expect_true(is.na(participants$outcome[[10L]]))
  expect_true("10" %in% itt$members$participant_id)

  expect_identical(
    pp$members$participant_id,
    c("1", "2", "7", "8", "9", "10")
  )
  reasons <- stats::setNames(pp$excluded$reason, pp$excluded$participant_id)
  expect_identical(reasons[["3"]], "major_deviation:eligibility")
  expect_identical(
    reasons[["4"]],
    "major_deviation:dose;major_deviation:visit;nonadherent"
  )
  expect_identical(reasons[["5"]], "nonadherent")
  expect_identical(reasons[["6"]], "incomplete")
  expect_identical(reasons[["11"]], "not_randomized")

  for (set in list(itt, pp)) {
    included <- set$members$participant_id
    excluded <- set$excluded$participant_id
    expect_length(intersect(included, excluded), 0L)
    expect_setequal(c(included, excluded), set$source_ids)
  }
  expect_identical(analysisParticipants(pp), pp$participants)
  expect_identical(analysisExclusions(pp), pp$excluded)
})

test_that("PP reasons are deterministic while included rows follow source order", {
  trial <- Trial("sets-order", c("A", "B"))
  participants <- make_analysis_participants()
  deviations <- make_analysis_deviations()
  reference <- perProtocol(
    trial,
    participants,
    deviations,
    adherent_col = "adherent",
    completed_col = "completed"
  )
  set.seed(19)
  participants <- participants[sample(nrow(participants)), , drop = FALSE]
  deviations <- deviations[sample(nrow(deviations)), , drop = FALSE]
  shuffled <- perProtocol(
    trial,
    participants,
    deviations,
    adherent_col = "adherent",
    completed_col = "completed"
  )
  reason <- function(x) {
    x <- x$excluded[order(x$excluded$participant_id), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  expect_identical(reason(reference), reason(shuffled))
  expect_identical(
    shuffled$participants$id,
    participants$id[participants$id %in% shuffled$members$participant_id]
  )
})

test_that("analysis-set validation rejects ambiguous source state", {
  trial <- Trial("sets-errors", c("A", "B"))
  participants <- make_analysis_participants()

  duplicated <- participants
  duplicated$id[[2L]] <- duplicated$id[[1L]]
  expect_error(intentionToTreat(trial, duplicated), "unique")

  missing_randomization <- participants
  missing_randomization$randomized[[1L]] <- NA
  expect_error(
    intentionToTreat(trial, missing_randomization),
    "non-missing logical"
  )

  bad_arm <- participants
  bad_arm$arm[[1L]] <- "unknown"
  expect_error(intentionToTreat(trial, bad_arm), "known trial arm")

  screen_arm <- participants
  screen_arm$arm[[11L]] <- "A"
  expect_error(intentionToTreat(trial, screen_arm), "arm = NA")

  deviations <- make_analysis_deviations()
  deviations$participant_id[[1L]] <- "unknown"
  expect_error(
    perProtocol(trial, participants, deviations),
    "absent from participants"
  )

  invalid_flag <- participants
  invalid_flag$adherent[[1L]] <- NA
  expect_error(
    perProtocol(trial, invalid_flag, adherent_col = "adherent"),
    "non-missing"
  )
  expect_error(analysisParticipants(list()), "analysis_set")
})

test_that("factor inputs and an empty PP set remain typed", {
  trial <- Trial("sets-factor", c("A", "B"))
  participants <- data.frame(
    id = factor(c("p1", "p2"), levels = c("p1", "p2", "unused")),
    arm = factor(c("A", "B"), levels = c("A", "B", "unused")),
    randomized = TRUE,
    adherent = FALSE
  )
  itt <- intentionToTreat(trial, participants)
  pp <- perProtocol(trial, participants, adherent_col = "adherent")
  expect_identical(itt$members$participant_id, c("p1", "p2"))
  expect_identical(itt$members$arm, c("A", "B"))
  expect_equal(nrow(pp$members), 0L)
  expect_equal(nrow(pp$participants), 0L)
  expect_identical(pp$excluded$reason, rep("nonadherent", 2L))
})

test_that("LOCF never fills backward or crosses participants", {
  trial <- Trial("locf", c("A", "B"))
  participants <- data.frame(
    id = c("p1", "p2", "screen"),
    arm = c("A", "B", NA),
    randomized = c(TRUE, TRUE, FALSE)
  )
  set <- intentionToTreat(trial, participants)
  outcomes <- data.frame(
    id = c("p1", "p1", "p1", "p2", "p2", "screen"),
    time = c(0, 1, 2, 0, 1, 0),
    value = c(NA, 2, NA, 9, NA, 100)
  )
  result <- analysisData(
    set,
    outcomes,
    time_col = "time",
    value_cols = "value",
    missing = "locf"
  )
  expect_equal(result$value, c(NA, 2, 2, 9, 9))
  expect_identical(attr(result, "analysis_set"), "ITT")
  expect_identical(attr(result, "excluded_ids"), "screen")

  shuffled <- outcomes[c(3, 1, 5, 2, 4, 6), , drop = FALSE]
  shuffled_result <- analysisData(
    set,
    shuffled,
    time_col = "time",
    value_cols = "value",
    missing = "locf"
  )
  expected <- c(2, NA, 9, 2, 9)
  expect_equal(shuffled_result$value, expected)
})

test_that("LOCF uses factor level order and preserves all-missing values", {
  trial <- Trial("locf-factor", c("A", "B"))
  participants <- data.frame(
    id = c("p1", "p2"),
    arm = c("A", "B"),
    randomized = TRUE
  )
  set <- intentionToTreat(trial, participants)
  outcomes <- data.frame(
    id = c("p1", "p1", "p2", "p2"),
    time = factor(
      c("a_followup", "z_baseline", "a_followup", "z_baseline"),
      levels = c("z_baseline", "a_followup")
    ),
    value = c(NA, 7, NA, NA)
  )
  result <- analysisData(
    set,
    outcomes,
    time_col = "time",
    missing = "locf"
  )
  expect_equal(result$value, c(7, 7, NA, NA))

  duplicate <- rbind(outcomes, outcomes[1L, ])
  expect_error(
    analysisData(set, duplicate, time_col = "time", missing = "locf"),
    "unique participant/time"
  )
  unknown <- outcomes
  unknown$id[[1L]] <- "unknown"
  expect_error(analysisData(set, unknown), "absent from the analysis source")
})

test_that("multiple-imputation callbacks must preserve row keys", {
  trial <- Trial("mi", c("A", "B"))
  participants <- data.frame(
    id = c("p1", "p2"),
    arm = c("A", "B"),
    randomized = TRUE
  )
  set <- intentionToTreat(trial, participants)
  outcomes <- data.frame(
    id = c("p1", "p1", "p2"),
    time = c(0L, 1L, 0L),
    value = c(NA, 2, 9)
  )
  imputer <- function(data, offset) {
    first <- data
    second <- data
    first$value[is.na(first$value)] <- offset
    second$value[is.na(second$value)] <- offset + 1
    list(first, second)
  }
  completed <- analysisData(
    set,
    outcomes,
    time_col = "time",
    missing = "multiple",
    imputer = imputer,
    offset = 4
  )
  expect_length(completed, 2L)
  expect_equal(completed[[1L]]$value, c(4, 2, 9))
  expect_equal(completed[[2L]]$value, c(5, 2, 9))
  expect_identical(attr(completed[[1L]], "included_ids"), c("p1", "p2"))

  expect_error(
    analysisData(
      set,
      outcomes,
      time_col = "time",
      missing = "multiple",
      imputer = function(x) x[c(3, 1, 2), ]
    ),
    "preserve participant IDs"
  )
  expect_error(
    analysisData(
      set,
      outcomes,
      time_col = "time",
      missing = "multiple",
      imputer = function(x) {
        x$time[[1L]] <- 99L
        x
      }
    ),
    "preserve time keys"
  )
  expect_error(
    analysisData(set, outcomes, missing = "multiple"),
    "imputer function"
  )
})
