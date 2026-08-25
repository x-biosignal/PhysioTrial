test_that("Arm and Participant constructors preserve typed trial metadata", {
  arm <- Arm(
    "active",
    ratio = 2,
    description = "Active rehabilitation",
    treatment_code = "masked"
  )
  expect_s4_class(arm, "Arm")
  expect_identical(arm@ratio, 2L)

  participant <- Participant(
    "p1",
    strata = c(sex = "F", site = "north")
  )
  expect_s4_class(participant, "Participant")
  expect_identical(
    participant@strata,
    c(sex = "F", site = "north")
  )
  expect_true(is.na(participant@arm))
})

test_that("Trial accepts labels or Arm objects and exposes ordered accessors", {
  trial <- Trial(
    "T1",
    arms = c("active", "control"),
    allocation_ratio = c(control = 1, active = 2),
    strata = list(site = c("north", "south"))
  )
  expect_s4_class(trial, "Trial")
  expect_identical(arms(trial), c("active", "control"))
  expect_identical(
    allocationRatio(trial),
    c(active = 2L, control = 1L)
  )
  expect_identical(strata(trial), list(site = c("north", "south")))

  from_arms <- Trial(
    "T2",
    list(Arm("A", 3), Arm("B", 1, is_control = TRUE))
  )
  expect_identical(allocationRatio(from_arms), c(A = 3L, B = 1L))
})

test_that("Trial model rejects malformed definitions", {
  expect_error(Arm("", 1), "label")
  expect_error(Arm("A", 0), "positive integer")
  expect_error(Participant("", c(site = "north")), "id")
  expect_error(Participant("p1", "north"), "strata")
  expect_error(Trial("T", "A"), "at least two")
  expect_error(Trial("T", c("A", "A")), "unique")
  expect_error(
    Trial("T", c("A", "B"), allocation_ratio = c(A = 1, C = 1)),
    "names must match"
  )
  expect_error(
    Trial("T", c("A", "B"), strata = list(site = character())),
    "strata"
  )
  expect_error(
    Trial("T", list(Arm("A"), Arm("B")), allocation_ratio = c(1, 1)),
    "must be omitted"
  )
})
