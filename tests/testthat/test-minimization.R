test_that(".ps_imbalance matches hand arithmetic for every discrepancy", {
  state <- list(
    sex = list(M = c(A = 3, B = 1)),
    age = list(old = c(A = 2, B = 2))
  )
  levels <- c(sex = "M", age = "old")
  ratio <- c(A = 1L, B = 1L)
  score <- function(arm, measure) {
    PhysioTrial:::.ps_imbalance(
      state,
      levels,
      arm,
      c("A", "B"),
      ratio,
      weights = c(sex = 1, age = 1),
      measure = measure
    )
  }
  expect_equal(score("A", "range"), 4)
  expect_equal(score("B", "range"), 2)
  expect_equal(score("A", "variance"), 2.5)
  expect_equal(score("B", "variance"), 0.5)
  expect_equal(score("A", "sd"), 2)
  expect_equal(score("B", "sd"), 1)
})

test_that("single-factor pure minimization keeps each level within one", {
  trial <- Trial(
    "T5",
    arms = c("A", "B"),
    strata = list(site = c("s1", "s2", "s3"))
  )
  set.seed(3)
  sites <- sample(c("s1", "s2", "s3"), 60, replace = TRUE)
  participants <- Map(
    function(i, site) Participant(paste0("p", i), c(site = site)),
    seq_along(sites),
    sites
  )
  sequence <- randomize(
    trial,
    "minimization",
    participants = participants,
    p_bias = 1,
    imbalance = "range",
    seed = 5
  )
  allocation <- PhysioTrial:::.sealed_assignments(sequence)
  imbalance <- tapply(allocation$arm, allocation$stratum, function(x) {
    abs(sum(x == "A") - sum(x == "B"))
  })
  expect_true(all(imbalance <= 1))
})

test_that("minimization out-balances simple across fixed replicates", {
  make_participants <- function(seed, n = 200) {
    set.seed(seed)
    lapply(seq_len(n), function(i) {
      Participant(
        paste0("p", i),
        c(
          sex = sample(c("M", "F"), 1),
          age = sample(c("y", "o"), 1)
        )
      )
    })
  }
  worst <- function(sequence) {
    allocation <- PhysioTrial:::.sealed_assignments(sequence)
    tokens <- strsplit(allocation$stratum, ";", fixed = TRUE)
    max(vapply(c("sex=M", "sex=F", "age=y", "age=o"), function(level) {
      selected <- vapply(tokens, function(x) level %in% x, logical(1))
      abs(
        sum(allocation$arm[selected] == "A") -
          sum(allocation$arm[selected] == "B")
      )
    }, numeric(1)))
  }

  trial <- Trial(
    "T6",
    arms = c("A", "B"),
    strata = list(sex = c("M", "F"), age = c("y", "o"))
  )
  repetitions <- 30L
  minimized <- simple <- numeric(repetitions)
  for (replicate in seq_len(repetitions)) {
    participants <- make_participants(1000 + replicate)
    minimized[[replicate]] <- worst(randomize(
      trial,
      "minimization",
      participants = participants,
      p_bias = 0.9,
      imbalance = "range",
      seed = replicate
    ))
    simple[[replicate]] <- worst(randomize(
      trial,
      "simple",
      participants = participants,
      seed = replicate
    ))
  }
  expect_lt(mean(minimized), mean(simple))
})

test_that("unequal allocation ratios normalize minimization scores", {
  state <- list(site = list(x = c(A = 4, B = 1)))
  levels <- c(site = "x")
  ratio <- c(A = 2L, B = 1L)
  score_a <- PhysioTrial:::.ps_imbalance(
    state, levels, "A", c("A", "B"), ratio,
    weights = c(site = 1), measure = "range"
  )
  score_b <- PhysioTrial:::.ps_imbalance(
    state, levels, "B", c("A", "B"), ratio,
    weights = c(site = 1), measure = "range"
  )
  expect_equal(score_a, 1.5)
  expect_equal(score_b, 0)
})
