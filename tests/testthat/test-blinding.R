test_that("blindingIndex reproduces the Bang closed-form value", {
  guesses <- data.frame(
    arm = rep("active", 20),
    guess = c(
      rep("active", 15),
      rep("control", 3),
      rep("dont_know", 2)
    )
  )
  result <- blindingIndex(guesses, dont_know = "dont_know")
  row <- result$table[result$table$arm == "active", ]
  expect_equal(row$BI, 0.6)
  expect_equal(
    row$SE,
    sqrt((18 / 20 - 0.6^2) / 20),
    tolerance = 1e-9
  )
  z <- qnorm(0.975)
  expect_equal(row$ci_lower, 0.6 - z * row$SE, tolerance = 1e-9)
  expect_equal(row$ci_upper, 0.6 + z * row$SE, tolerance = 1e-9)
})

test_that("blindingIndex handles neutral and boundary estimates", {
  balanced <- data.frame(
    arm = rep("a", 10),
    guess = c(rep("a", 5), rep("b", 5))
  )
  unknown <- data.frame(
    arm = rep("a", 10),
    guess = rep("dont_know", 10)
  )
  correct <- data.frame(arm = rep("a", 4), guess = rep("a", 4))
  incorrect <- data.frame(arm = rep("a", 4), guess = rep("b", 4))
  missing_guess <- data.frame(arm = "a", guess = NA_character_)

  expect_equal(blindingIndex(balanced)$table$BI, 0)
  expect_equal(blindingIndex(unknown)$table$BI, 0)
  expect_equal(
    unname(unlist(blindingIndex(correct)$table[c("BI", "SE")])),
    c(1, 0)
  )
  expect_equal(
    unname(unlist(blindingIndex(incorrect)$table[c("BI", "SE")])),
    c(-1, 0)
  )
  expect_equal(blindingIndex(missing_guess)$table$BI, -1)
})

test_that("generated and supplied treatment codes are validated", {
  trial <- Trial("codes", c("A", "B", "C"))
  first <- blindingManager(trial, seed = 42)
  second <- blindingManager(trial, seed = 42)
  expect_identical(blindingCodes(first), blindingCodes(second))
  expect_identical(trialFingerprint(first), trialFingerprint(second))
  expect_setequal(unname(blindingCodes(first)), sprintf("Kit-%03d", 1:3))

  supplied <- blindingManager(
    trial,
    codes = c(C = "masked-3", A = "masked-1", B = "masked-2")
  )
  expect_identical(
    blindingCodes(supplied),
    c(A = "masked-1", B = "masked-2", C = "masked-3")
  )
  expect_error(
    blindingManager(trial, codes = c(A = "x", B = "x", C = "z")),
    "unique"
  )
})

test_that("unblind appends an audit row and changes the fingerprint", {
  trial <- Trial("T7", arms = c("A", "B"))
  participants <- lapply(1:6, function(i) Participant(paste0("p", i)))
  sequence <- randomize(
    trial,
    "permuted_block",
    participants = participants,
    block_sizes = 2L,
    seed = 1
  )
  manager <- blindingManager(trial, sequence = sequence, seed = 2)
  initial_fingerprint <- trialFingerprint(manager)
  expect_equal(nrow(unblindingLog(manager)), 0L)

  manager <- unblind(
    manager,
    "p3",
    requester = "PI",
    reason = "SAE"
  )
  log <- unblindingLog(manager)
  expect_equal(nrow(log), 1L)
  expect_equal(log$participant_id, "p3")
  expect_true(log$arm %in% c("A", "B"))
  expect_false(identical(trialFingerprint(manager), initial_fingerprint))
  expect_error(unblind(manager, "unknown"), "not present")
})

test_that("blinding inputs reject ambiguous or malformed values", {
  trial <- Trial("blind-errors", c("A", "B"))
  expect_error(
    blindingIndex(data.frame(arm = "A"), guess_col = "guess"),
    "missing columns"
  )
  expect_error(
    blindingIndex(data.frame(arm = NA_character_, guess = "A")),
    "Actual arm"
  )
  expect_error(
    blindingIndex(data.frame(arm = "dont_know", guess = "dont_know")),
    "must not equal"
  )
  expect_error(
    blindingIndex(data.frame(arm = "A", guess = "A"), conf_level = 1),
    "strictly"
  )
  expect_error(
    blindingManager(
      trial,
      codes = c(A = "one", wrong = "two")
    ),
    "one code per arm"
  )
})
