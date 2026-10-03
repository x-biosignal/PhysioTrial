make_consort_fixture <- function() {
  randomized <- c(rep(TRUE, 12), FALSE, FALSE)
  arm <- c(rep("A", 6), rep("B", 6), NA, NA)
  received <- c(
    FALSE, rep(TRUE, 5),
    rep(TRUE, 6),
    NA, NA
  )
  followup <- c(
    FALSE, FALSE, rep(TRUE, 4),
    FALSE, rep(TRUE, 5),
    NA, NA
  )
  analysed <- c(
    FALSE, rep(TRUE, 5),
    FALSE, FALSE, rep(TRUE, 4),
    NA, NA
  )
  data.frame(
    participant_id = paste0("p", seq_len(14)),
    eligible = c(rep(TRUE, 12), FALSE, TRUE),
    randomized = randomized,
    arm = arm,
    received_allocated = received,
    followup_complete = followup,
    analysed = analysed,
    pre_exclusion_reason = c(
      rep(NA_character_, 12), "ineligible", "declined"
    ),
    not_received_reason = c(
      "supply", rep(NA_character_, 13)
    ),
    followup_reason = c(
      "withdrew", "lost", rep(NA_character_, 4),
      "lost", rep(NA_character_, 7)
    ),
    analysis_exclusion_reason = c(
      "no outcome", rep(NA_character_, 5),
      "no outcome", "no outcome", rep(NA_character_, 6)
    ),
    stringsAsFactors = FALSE
  )
}

test_that("CONSORT nodes and reasons reconcile exactly", {
  trial <- Trial("CONSORT", c("A", "B"))
  diagram <- consortDiagram(
    trial,
    make_consort_fixture(),
    render = FALSE
  )
  counts <- stats::setNames(diagram$nodes$n, diagram$nodes$node_id)
  expect_equal(
    counts[c("assessed", "excluded", "randomized")],
    c(assessed = 14L, excluded = 2L, randomized = 12L)
  )
  expect_equal(
    counts[c(
      "arm1_allocated", "arm1_received", "arm1_not_received",
      "arm1_followup", "arm1_followup_incomplete",
      "arm1_analysed", "arm1_excluded_analysis"
    )],
    c(
      arm1_allocated = 6L,
      arm1_received = 5L,
      arm1_not_received = 1L,
      arm1_followup = 4L,
      arm1_followup_incomplete = 2L,
      arm1_analysed = 5L,
      arm1_excluded_analysis = 1L
    )
  )
  expect_equal(
    counts[c(
      "arm2_allocated", "arm2_received", "arm2_not_received",
      "arm2_followup", "arm2_followup_incomplete",
      "arm2_analysed", "arm2_excluded_analysis"
    )],
    c(
      arm2_allocated = 6L,
      arm2_received = 6L,
      arm2_not_received = 0L,
      arm2_followup = 5L,
      arm2_followup_incomplete = 1L,
      arm2_analysed = 4L,
      arm2_excluded_analysis = 2L
    )
  )
  expect_equal(
    sum(diagram$reasons$n[
      diagram$reasons$stage == "excluded_before_randomization"
    ]),
    2L
  )
  expect_identical(consortCounts(diagram), diagram$nodes)
  expect_identical(as.data.frame(diagram), diagram$nodes)
  expect_identical(format(diagram), diagram$dot)
})

test_that("CONSORT output is invariant to participant row order", {
  trial <- Trial("CONSORT-order", c("A", "B"))
  flow <- make_consort_fixture()
  first <- consortDiagram(trial, flow, render = FALSE)
  set.seed(41)
  second <- consortDiagram(
    trial,
    flow[sample(nrow(flow)), ],
    render = FALSE
  )
  expect_identical(first$nodes, second$nodes)
  expect_identical(first$edges, second$edges)
  expect_identical(first$reasons, second$reasons)
  expect_identical(first$dot, second$dot)
})

test_that("CONSORT escapes user reason text in DOT", {
  trial <- Trial("CONSORT-dot", c("A", "B"))
  flow <- make_consort_fixture()
  flow$pre_exclusion_reason[[13L]] <- "lost\"; evil -> node"
  diagram <- consortDiagram(trial, flow, render = FALSE)
  escaped <- paste0("lost", "\\", "\"; evil -> node")
  expect_true(grepl(escaped, diagram$dot, fixed = TRUE))
  expect_false(grepl('lost"; evil -> node', diagram$dot, fixed = TRUE))
})

test_that("CONSORT rejects inconsistent flow states", {
  trial <- Trial("CONSORT-errors", c("A", "B"))
  flow <- make_consort_fixture()

  bad <- flow
  bad$arm[[1L]] <- NA_character_
  expect_error(consortDiagram(trial, bad, FALSE), "known trial arm")

  bad <- flow
  bad$arm[[13L]] <- "A"
  expect_error(consortDiagram(trial, bad, FALSE), "must not have an arm")

  bad <- flow
  bad$pre_exclusion_reason[[13L]] <- NA_character_
  expect_error(consortDiagram(trial, bad, FALSE), "pre_exclusion_reason")

  bad <- flow
  bad$arm[[1L]] <- "unknown"
  expect_error(consortDiagram(trial, bad, FALSE), "known trial arm")

  bad <- flow
  bad$randomized[[13L]] <- TRUE
  expect_error(consortDiagram(trial, bad, FALSE), "eligible")

  bad <- flow
  bad$participant_id[[2L]] <- bad$participant_id[[1L]]
  expect_error(consortDiagram(trial, bad, FALSE), "unique")
  expect_error(consortCounts(list()), "consort_diagram")
})

test_that("CONSORT DiagrammeR rendering is capability-gated", {
  trial <- Trial("CONSORT-render", c("A", "B"))
  flow <- make_consort_fixture()
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    diagram <- consortDiagram(trial, flow, render = TRUE)
    expect_s3_class(diagram$widget, "htmlwidget")
  } else {
    expect_error(
      consortDiagram(trial, flow, render = TRUE),
      "DiagrammeR is required"
    )
  }
})
