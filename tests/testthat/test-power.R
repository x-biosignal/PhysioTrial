library(testthat)

test_that("continuous sample size matches base stats::power.t.test", {
  for (d in c(0.3, 0.5, 0.8, 1.0)) {
    base_n <- ceiling(stats::power.t.test(delta = d, sd = 1, power = 0.8,
                                          sig.level = 0.05)$n)
    ours <- sampleSizeContinuous(delta = d, sd = 1)$n1
    expect_lte(abs(base_n - ours), 1L)                 # within rounding
  }
  # classic d = 0.5 -> ~64 per arm
  expect_equal(sampleSizeContinuous(delta = 5, sd = 10)$n1, 64L)
})

test_that("binary sample size matches base stats::power.prop.test", {
  base_n <- ceiling(stats::power.prop.test(p1 = 0.3, p2 = 0.5, power = 0.8,
                                           sig.level = 0.05)$n)
  expect_lte(abs(sampleSizeBinary(0.3, 0.5)$n1 - base_n), 2L)
})

test_that("powerContinuous inverts sampleSizeContinuous", {
  n <- sampleSizeContinuous(delta = 5, sd = 10, power = 0.8)$n1
  expect_gte(powerContinuous(n, delta = 5, sd = 10), 0.80)
  expect_lt(powerContinuous(n - 5, delta = 5, sd = 10), 0.80)
})

test_that("ANCOVA and change-score shrink n via the baseline correlation", {
  cont <- sampleSizeContinuous(5, 10)$n1
  anc  <- sampleSizeANCOVA(5, 10, baseline_cor = 0.6)$n1
  expect_lt(anc, cont)
  expect_equal(anc / cont, 1 - 0.6^2, tolerance = 0.05)   # (1 - rho^2)
  # change-score beats unadjusted only when rho > 0.5
  expect_lt(sampleSizeChangeScore(5, 10, 0.7)$n1, cont)
  expect_gt(sampleSizeChangeScore(5, 10, 0.3)$n1, cont)
  # ANCOVA is never worse than change-score
  expect_lte(sampleSizeANCOVA(5, 10, 0.7)$n1, sampleSizeChangeScore(5, 10, 0.7)$n1)
})

test_that("repeated-measures uses the compound-symmetry variance factor", {
  cont <- sampleSizeContinuous(5, 10)$n1
  rm3  <- sampleSizeRepeatedMeasures(5, 10, n_measurements = 3, correlation = 0.5)$n1
  expect_lt(rm3, cont)                                   # averaging reduces variance
  # more visits help more at lower correlation
  lo <- sampleSizeRepeatedMeasures(5, 10, 3, 0.2)$n1
  hi <- sampleSizeRepeatedMeasures(5, 10, 3, 0.8)$n1
  expect_lt(lo, hi)
})

test_that("cluster design effect and dropout inflation are applied correctly", {
  base <- sampleSizeContinuous(5, 10)
  cl <- sampleSizeContinuous(5, 10, cluster_size = 10, icc = 0.05)
  expect_equal(cl$design_effect, 1 + 9 * 0.05)          # 1.45
  expect_equal(cl$n1, ceiling(base$n1 * 1.45))
  expect_false(is.null(cl$clusters))
  dr <- sampleSizeContinuous(5, 10, dropout = 0.2)
  expect_equal(dr$n_enrolled, ceiling(dr$n_total / 0.8))
})

test_that("allocation ratio and one-sided reduce control-arm n appropriately", {
  expect_lt(sampleSizeContinuous(5, 10, sides = 1)$n1,
            sampleSizeContinuous(5, 10, sides = 2)$n1)
  # 2:1 allocation: control arm smaller than balanced total is larger
  r21 <- sampleSizeContinuous(5, 10, ratio = 2)
  expect_true(r21$n2 > r21$n1)
})

test_that("powerCurve is monotonic in n and returns the right shape", {
  pc <- powerCurve(delta = 5, sd = 10, n1 = seq(20, 120, 20))
  expect_s3_class(pc, "data.frame")
  expect_equal(nrow(pc), 6L)
  expect_true(all(diff(pc$power) > 0))                  # increasing with n
})

test_that("estimateBaselineCorrelation works from vectors and data frames", {
  set.seed(1); b <- rnorm(50); f <- 0.6 * b + rnorm(50, sd = sqrt(1 - 0.36))
  rho <- estimateBaselineCorrelation(b, f)
  expect_equal(rho, 0.6, tolerance = 0.2)
  df <- data.frame(base = b, post = f)
  expect_equal(estimateBaselineCorrelation(df, cols = c("base", "post")), rho)
})

test_that("input validation errors", {
  expect_error(sampleSizeContinuous(5, -1), "sd")
  expect_error(sampleSizeContinuous(0, 10), "non-zero")
  expect_error(sampleSizeBinary(0.5, 0.5), "p1 != p2")
  expect_error(sampleSizeANCOVA(5, 10, 1.2), "baseline_cor")
})
