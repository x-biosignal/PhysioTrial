make_ae_fixture <- function() {
  date <- as.Date("2026-01-01")
  list(
    adverseEvent("p1", "Nausea", date, ctcae_grade = 1),
    adverseEvent(
      "p1", "Infusion reaction", date + 1,
      ctcae_grade = 2, serious = TRUE,
      meddra_pt = "Infusion related reaction",
      meddra_code = "10051792", meddra_version = "28.0"
    ),
    adverseEvent(
      "p2", "Fatigue", date + 2,
      ctcae_grade = 3, serious = TRUE
    ),
    adverseEvent("p5", "Nausea", date, ctcae_grade = 2),
    adverseEvent("p6", "Nausea", date + 1, ctcae_grade = 2),
    adverseEvent(
      "p6", "Hypotension", date + 2,
      ctcae_grade = 4, serious = TRUE
    )
  )
}

make_ae_allocation <- function() {
  data.frame(
    participant_id = paste0("p", 1:8),
    arm = rep(c("A", "B"), each = 4),
    stringsAsFactors = FALSE
  )
}

test_that("adverseEvent preserves typed clinical and coding fields", {
  metadata <- list(source = "site", form = 2L)
  event <- adverseEvent(
    "p1",
    "Nausea",
    as.Date("2026-01-01"),
    end_date = as.Date("2026-01-02"),
    ctcae_grade = 2,
    serious = TRUE,
    causality = "possible",
    action = "dose interrupted",
    outcome = "recovered",
    meddra_pt = "Nausea",
    meddra_code = "10028813",
    meddra_version = "28.0",
    metadata = metadata
  )
  expect_s3_class(event, "adverse_event")
  expect_s3_class(event$onset_date, "Date")
  expect_identical(event$ctcae_grade, 2L)
  expect_identical(event$metadata[[1L]], metadata)
})

test_that("aeSummary separates events, participants, severity, and SAE", {
  summary <- aeSummary(
    make_ae_fixture(),
    make_ae_allocation(),
    arms = c("A", "B")
  )
  expect_s3_class(summary, "ae_summary")
  expect_equal(
    summary$by_arm[c(
      "arm", "n_randomized", "n_ae", "n_participants_ae",
      "n_sae", "n_participants_sae", "ae_risk"
    )],
    data.frame(
      arm = c("A", "B"),
      n_randomized = c(4L, 4L),
      n_ae = c(3L, 3L),
      n_participants_ae = c(2L, 2L),
      n_sae = c(2L, 1L),
      n_participants_sae = c(2L, 1L),
      ae_risk = c(0.5, 0.5)
    )
  )
  severity <- summary$by_severity
  expect_equal(nrow(severity), 10L)
  expect_equal(
    severity$n_ae[severity$arm == "A"],
    c(1L, 1L, 1L, 0L, 0L)
  )
  expect_equal(
    severity$n_ae[severity$arm == "B"],
    c(0L, 2L, 0L, 1L, 0L)
  )
  expect_equal(
    severity$n_participants[
      severity$arm == "B" & severity$ctcae_grade == 2L
    ],
    2L
  )
})

test_that("aeSummary retains zero-event arms and recurrent incidence", {
  allocation <- make_ae_allocation()
  empty <- aeSummary(list(), allocation, arms = c("A", "B", "C"))
  expect_equal(empty$by_arm$n_ae, c(0L, 0L, 0L))
  expect_equal(empty$by_arm$n_randomized, c(4L, 4L, 0L))
  expect_true(is.na(empty$by_arm$ae_risk[[3L]]))
  expect_equal(nrow(empty$by_severity), 15L)
  expect_true(all(empty$by_severity$n_ae == 0L))
  expect_equal(nrow(empty$by_term), 0L)

  repeated <- aeSummary(
    make_ae_fixture()[1:2],
    allocation,
    arms = c("A", "B")
  )
  expect_equal(repeated$by_arm$n_ae, c(2L, 0L))
  expect_equal(repeated$by_arm$n_participants_ae, c(1L, 0L))
})

test_that("AE validation catches inconsistent dates and fatal status", {
  date <- as.Date("2026-01-02")
  expect_error(
    adverseEvent(
      "p1", "Nausea", date,
      end_date = date - 1,
      ctcae_grade = 1
    ),
    "must not precede"
  )
  expect_error(
    adverseEvent(
      "p1", "Death", date,
      ctcae_grade = 5,
      serious = FALSE
    ),
    "grade 5"
  )
  expect_error(
    adverseEvent(
      "p1", "Death", date,
      ctcae_grade = 4,
      serious = TRUE,
      outcome = "fatal"
    ),
    "Fatal events"
  )
  expect_error(
    adverseEvent("p1", "Nausea", date, ctcae_grade = NaN),
    "integer"
  )
  expect_error(
    adverseEvent("p1", "Nausea", "not-a-date", ctcae_grade = 1),
    "valid Date"
  )
  expect_error(
    adverseEvent("p1", "Nausea", structure(Inf, class = "Date"),
                 ctcae_grade = 1),
    "valid Date"
  )
  expect_silent(
    adverseEvent(
      "p1", "Nausea", date, end_date = date,
      ctcae_grade = 1
    )
  )
})

test_that("aeSummary rejects unknown IDs and ambiguous allocations", {
  allocation <- make_ae_allocation()
  unknown <- adverseEvent(
    "unknown", "Nausea", as.Date("2026-01-01"),
    ctcae_grade = 1
  )
  expect_error(
    aeSummary(list(unknown), allocation),
    "absent from allocation"
  )

  duplicated <- rbind(allocation, allocation[1L, ])
  expect_error(
    aeSummary(make_ae_fixture(), duplicated),
    "unique IDs"
  )

  expect_error(
    aeSummary(make_ae_fixture(), allocation, arms = "A"),
    "not listed"
  )
})
