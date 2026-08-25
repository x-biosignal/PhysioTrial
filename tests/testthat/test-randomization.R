test_that("permuted_block gives exact 1:1 balance in every completed block", {
  trial <- Trial("T1", arms = c("A", "B"))
  sequence <- randomize(
    trial,
    "permuted_block",
    n = 12,
    block_sizes = 4L,
    seed = 42
  )
  allocation <- PhysioTrial:::.sealed_assignments(sequence)
  expect_equal(nrow(allocation), 12L)
  for (block in split(allocation$arm, allocation$block_id)) {
    expect_equal(
      sort(table(block)),
      c(A = 2L, B = 2L),
      ignore_attr = TRUE
    )
  }
  expect_equal(as.integer(table(allocation$arm)), c(6L, 6L))
})

test_that("permuted_block honours unequal ratios and a partial final block", {
  trial <- Trial(
    "T2",
    arms = c("A", "B"),
    allocation_ratio = c(A = 2L, B = 1L)
  )
  complete <- randomize(
    trial,
    "permuted_block",
    n = 12,
    block_sizes = 6L,
    seed = 7
  )
  allocation <- PhysioTrial:::.sealed_assignments(complete)
  for (block in split(allocation$arm, allocation$block_id)) {
    expect_equal(c(sum(block == "A"), sum(block == "B")), c(4L, 2L))
  }
  expect_equal(
    c(sum(allocation$arm == "A"), sum(allocation$arm == "B")),
    c(8L, 4L)
  )

  partial <- randomize(
    Trial("T2p", c("A", "B")),
    "permuted_block",
    n = 6,
    block_sizes = 4L,
    seed = 7
  )
  partial_table <- PhysioTrial:::.sealed_assignments(partial)
  expect_equal(nrow(partial_table), 6L)
  expect_equal(as.integer(table(partial_table$block_id)), c(4L, 2L))
})

test_that("stratified_block is exactly balanced within every stratum", {
  trial <- Trial(
    "T3",
    arms = c("A", "B"),
    strata = list(sex = c("M", "F"))
  )
  participants <- c(
    lapply(1:8, function(i) Participant(paste0("m", i), c(sex = "M"))),
    lapply(1:8, function(i) Participant(paste0("f", i), c(sex = "F")))
  )
  sequence <- randomize(
    trial,
    "stratified_block",
    participants = participants,
    block_sizes = 4L,
    seed = 11
  )
  allocation <- PhysioTrial:::.sealed_assignments(sequence)
  by_stratum <- tapply(allocation$arm, allocation$stratum, function(x) {
    as.integer(table(factor(x, c("A", "B"))))
  })
  expect_equal(by_stratum[["sex=M"]], c(4L, 4L))
  expect_equal(by_stratum[["sex=F"]], c(4L, 4L))
  expect_equal(
    length(unique(split(allocation$block_id, allocation$stratum))),
    2L
  )
})

test_that("stratified streams are independent of cross-stratum row order", {
  trial <- Trial(
    "T3-order",
    arms = c("A", "B"),
    strata = list(site = c("x", "y"))
  )
  grouped <- data.frame(
    id = c(paste0("x", 1:8), paste0("y", 1:8)),
    site = rep(c("x", "y"), each = 8)
  )
  interleaved <- grouped[c(rbind(1:8, 9:16)), ]
  first <- PhysioTrial:::.sealed_assignments(randomize(
    trial, "stratified_block", participants = grouped,
    block_sizes = 4L, seed = 501
  ))
  second <- PhysioTrial:::.sealed_assignments(randomize(
    trial, "stratified_block", participants = interleaved,
    block_sizes = 4L, seed = 501
  ))
  expect_identical(
    first$arm[match(grouped$id, first$participant_id)],
    second$arm[match(grouped$id, second$participant_id)]
  )
})

test_that("fixed seed reproduces sealed content and public access stays masked", {
  trial <- Trial("T4", arms = c("A", "B"))
  s1 <- randomize(
    trial, "permuted_block", n = 20, block_sizes = 4L, seed = 99
  )
  s2 <- randomize(
    trial, "permuted_block", n = 20, block_sizes = 4L, seed = 99
  )
  s3 <- randomize(
    trial, "permuted_block", n = 20, block_sizes = 4L, seed = 100
  )
  expect_identical(
    PhysioTrial:::.sealed_assignments(s1)$arm,
    PhysioTrial:::.sealed_assignments(s2)$arm
  )
  expect_false(identical(
    PhysioTrial:::.sealed_assignments(s1)$arm,
    PhysioTrial:::.sealed_assignments(s3)$arm
  ))
  expect_true(all(is.na(assignments(s1)$arm)))
  expect_equal(allocationTable(s1)$n, c(0L, 0L))

  first <- nextAllocation(s1, requester = "site")
  expect_equal(first$participant_id, "slot_1")
  expect_true(first$arm %in% c("A", "B"))
  expect_false(is.na(assignments(first$sequence)$arm[[1L]]))
  expect_true(all(is.na(assignments(first$sequence)$arm[-1L])))
  counts <- allocationTable(first$sequence)
  expect_equal(sum(counts$n[counts$stratum == "__overall__"]), 1L)
  expect_equal(randomSeed(s1), 99L)
  expect_true("seal" %in% auditLog(s1)$event)
  expect_identical(trialFingerprint(s1), trialFingerprint(s2))
  expect_false(identical(
    trialFingerprint(s1),
    trialFingerprint(first$sequence)
  ))
})

test_that("generated seed is captured and participant covariates are recorded", {
  trial <- Trial(
    "T-simple",
    c("A", "B"),
    strata = list(site = c("north", "south"))
  )
  participants <- data.frame(
    id = c("p1", "p2", "p3"),
    site = c("north", "south", "north")
  )
  sequence <- randomize(
    trial,
    "simple",
    participants = participants,
    seed = NULL
  )
  expect_type(randomSeed(sequence), "integer")
  expect_gt(randomSeed(sequence), 0L)
  expect_identical(
    PhysioTrial:::.sealed_assignments(sequence)$stratum,
    c("site=north", "site=south", "site=north")
  )
})

test_that("randomize validates method-specific and numeric inputs", {
  unstratified <- Trial("TU", c("A", "B"))
  stratified <- Trial(
    "TS", c("A", "B"), strata = list(site = c("x", "y"))
  )
  participants <- data.frame(id = "p1", site = "x")

  expect_error(randomize(unstratified, n = 2, participants = list()), "exactly")
  expect_error(randomize(unstratified, n = 0), "positive")
  expect_error(randomize(unstratified, n = 2, seed = 0), "seed")
  expect_error(
    randomize(unstratified, "permuted_block", n = 2, block_sizes = 3),
    "multiples"
  )
  expect_error(randomize(stratified, "simple", n = 2), "participants")
  expect_error(
    randomize(stratified, "stratified_block", participants = NULL, n = NULL),
    "exactly"
  )
  expect_error(
    randomize(stratified, "minimization", participants, p_bias = NaN),
    "p_bias"
  )
  expect_error(
    randomize(
      stratified, "simple",
      participants = data.frame(id = "p1", site = "unknown")
    ),
    "Invalid level"
  )
})
