make_spirit_fixture <- function() {
  timepoint <- list(
    screen = c("Screening", -1L),
    baseline = c("Baseline", 0L),
    week6 = c("Week 6", 1L),
    week12 = c("Week 12", 2L)
  )
  cell <- function(id, category, activity, arm = NA_character_,
                   scheduled = TRUE, marker = "X") {
    data.frame(
      timepoint_id = id,
      timepoint_label = timepoint[[id]][[1L]],
      timepoint_order = as.integer(timepoint[[id]][[2L]]),
      category = category,
      activity = activity,
      arm = arm,
      scheduled = scheduled,
      marker = marker,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, list(
    cell("screen", "enrolment", "Eligibility"),
    cell("baseline", "enrolment", "Allocation"),
    cell("baseline", "intervention", "Assigned intervention", "A"),
    cell("week6", "intervention", "Assigned intervention", "A"),
    cell("week12", "intervention", "Assigned intervention", "A"),
    cell("baseline", "intervention", "Assigned intervention", "B"),
    cell("week6", "intervention", "Assigned intervention", "B"),
    cell("week12", "intervention", "Assigned intervention", "B"),
    cell("baseline", "assessment", "Primary endpoint"),
    cell("week12", "assessment", "Primary endpoint"),
    cell("week6", "assessment", "Adverse events"),
    cell("week12", "assessment", "Adverse events")
  ))
}

test_that("SPIRIT schedule has stable ordered cells", {
  trial <- Trial("SPIRIT", c("A", "B"))
  events <- make_spirit_fixture()
  schedule <- spiritSchedule(trial, events)
  table <- as.data.frame(schedule)

  expect_identical(
    names(table),
    c(
      "category", "activity", "arm",
      "screen", "baseline", "week6", "week12"
    )
  )
  expect_identical(
    schedule$timepoints$timepoint_order,
    c(-1L, 0L, 1L, 2L)
  )
  eligibility <- table$activity == "Eligibility"
  expect_identical(
    unname(unlist(table[eligibility, 4:7])),
    c("X", "", "", "")
  )
  arm_a <- table$activity == "Assigned intervention" & table$arm == "A"
  expect_identical(
    unname(unlist(table[arm_a, 4:7])),
    c("", "X", "X", "X")
  )
  endpoint <- table$activity == "Primary endpoint"
  expect_identical(
    unname(unlist(table[endpoint, 4:7])),
    c("", "X", "", "X")
  )

  set.seed(17)
  shuffled <- spiritSchedule(
    trial,
    events[sample(nrow(events)), ]
  )
  expect_identical(schedule$table, shuffled$table)
  expect_identical(schedule$timepoints, shuffled$timepoints)
})

test_that("SPIRIT validates timepoints, cells, and arms", {
  trial <- Trial("SPIRIT-errors", c("A", "B"))
  events <- make_spirit_fixture()

  expect_error(
    spiritSchedule(trial, rbind(events, events[1L, ])),
    "duplicate"
  )

  bad <- events
  duplicate <- bad[1L, ]
  duplicate$activity <- "Consent"
  duplicate$timepoint_label <- "Wrong label"
  expect_error(
    spiritSchedule(trial, rbind(bad, duplicate)),
    "exactly one label"
  )

  bad <- events
  bad$timepoint_order[bad$timepoint_id == "week12"] <- 1L
  expect_error(spiritSchedule(trial, bad), "must be unique")

  bad <- events
  bad$arm[[3L]] <- "unknown"
  expect_error(spiritSchedule(trial, bad), "unknown trial arm")

  bad <- events
  bad$scheduled[[1L]] <- NA
  expect_error(spiritSchedule(trial, bad), "scheduled")

  bad <- events
  bad$marker[[1L]] <- ""
  expect_error(spiritSchedule(trial, bad), "non-empty marker")
})

test_that("SPIRIT checklist covers all top-level items and tracks evidence", {
  checklist <- spiritChecklist(list(
    `1` = "Title",
    `2` = "Registry",
    `5` = TRUE,
    `12` = c("Primary", "Secondary"),
    `33` = "Consent form"
  ))
  expect_s3_class(checklist, "spirit_checklist")
  expect_setequal(unique(checklist$top_level_item), seq_len(33L))
  expect_equal(sum(checklist$complete), 5L)
  expect_equal(
    checklist$evidence[checklist$item_id == "12"],
    "Primary; Secondary"
  )
  expect_error(
    spiritChecklist(list(unknown = "value")),
    "Unknown SPIRIT"
  )

  incomplete <- data.frame(
    item_id = "1",
    top_level_item = 1L,
    section = "section",
    short_label = "label"
  )
  expect_error(
    spiritChecklist(list(`1` = "x"), checklist = incomplete),
    "all 33"
  )

  malformed <- data.frame(
    item_id = as.character(seq_len(33L)),
    top_level_item = c(1.5, 2:33),
    section = "section",
    short_label = "label"
  )
  expect_error(
    spiritChecklist(list(`1` = "x"), checklist = malformed),
    "finite integers"
  )
})
