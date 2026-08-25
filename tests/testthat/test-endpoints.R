make_ancova_fixture <- function() {
  baseline <- rep(0:7, 2L)
  residual <- rep(c(-1.5, -0.5, 0.5, 1.5, -1, 0, 1, 0), 2L)
  arm <- rep(c("control", "active"), each = 8L)
  change <- 5 + residual + ifelse(arm == "active", 2, 0)
  data.frame(
    id = paste0("p", seq_along(arm)),
    arm = arm,
    baseline = baseline,
    followup = baseline + change,
    covariate = rep(c(-1, 1), 8L),
    stringsAsFactors = FALSE
  )
}

test_that("ANCOVA matches direct lm and PhysioCore effect sizes", {
  data <- make_ancova_fixture()
  result <- endpointANCOVA(
    data,
    "followup",
    "baseline",
    "arm",
    control = "control"
  )
  reference_data <- data
  reference_data$change <- reference_data$followup -
    reference_data$baseline
  reference_data$arm <- stats::relevel(
    factor(reference_data$arm),
    ref = "control"
  )
  reference <- stats::lm(
    change ~ arm + baseline,
    data = reference_data
  )
  coefficient <- summary(reference)$coefficients["armactive", ]
  critical <- stats::qt(0.975, reference$df.residual)
  estimate <- result$estimates[1L, ]

  expect_s3_class(result, "endpoint_analysis")
  expect_identical(result$method, "ancova_change")
  expect_equal(estimate$estimate, 2, tolerance = 1e-12)
  expect_equal(estimate$estimate, coefficient[["Estimate"]],
               tolerance = 1e-12)
  expect_equal(estimate$std_error, coefficient[["Std. Error"]],
               tolerance = 1e-12)
  expect_equal(estimate$df, reference$df.residual, tolerance = 1e-12)
  expect_equal(estimate$statistic, coefficient[["t value"]],
               tolerance = 1e-12)
  expect_equal(estimate$p_value, coefficient[["Pr(>|t|)"]],
               tolerance = 1e-12)
  expect_equal(
    c(estimate$ci_lower, estimate$ci_upper),
    coefficient[["Estimate"]] +
      c(-1, 1) * critical * coefficient[["Std. Error"]],
    tolerance = 1e-12
  )

  direct_d <- PhysioCore::cohensD(
    reference_data$change[reference_data$arm == "active"],
    reference_data$change[reference_data$arm == "control"]
  )
  expect_equal(
    c(estimate$cohens_d, estimate$d_ci_lower, estimate$d_ci_upper),
    c(direct_d$d, direct_d$ci_lower, direct_d$ci_upper),
    tolerance = 1e-12
  )
})

test_that("ANCOVA keeps explicit control direction and missingness counts", {
  data <- make_ancova_fixture()
  data$arm <- factor(data$arm, levels = c("active", "control"))
  reversed <- endpointANCOVA(
    data[nrow(data):1L, ],
    "followup",
    "baseline",
    "arm",
    control = "control"
  )
  expect_equal(reversed$estimates$estimate, 2, tolerance = 1e-12)
  expect_identical(reversed$estimates$arm, "active")
  expect_identical(reversed$estimates$control, "control")

  data$followup[[1L]] <- NA_real_
  missing <- endpointANCOVA(
    data,
    "followup",
    "baseline",
    "arm",
    control = "control",
    response_scale = "followup"
  )
  expect_identical(missing$method, "ancova_followup")
  expect_equal(missing$n_input, 16L)
  expect_equal(missing$n_used, 15L)
  expect_equal(missing$n_missing, 1L)
})

test_that("ANCOVA rejects unsupported numeric and design states", {
  data <- make_ancova_fixture()
  nonfinite <- data
  nonfinite$followup[[1L]] <- Inf
  expect_error(
    endpointANCOVA(nonfinite, "followup", "baseline", "arm"),
    "finite"
  )

  rank_deficient <- data
  rank_deficient$duplicate_baseline <- 2 * rank_deficient$baseline
  expect_error(
    endpointANCOVA(
      rank_deficient,
      "followup",
      "baseline",
      "arm",
      covariates = "duplicate_baseline"
    ),
    "rank deficient"
  )

  one_active <- data[c(seq_len(8L), 9L), ]
  expect_error(
    endpointANCOVA(one_active, "followup", "baseline", "arm"),
    "at least two"
  )
})

make_mmrm_fixture <- function() {
  set.seed(331)
  ids <- paste0("p", seq_len(24L))
  visit <- factor(c("week2", "week6", "week12"),
                  levels = c("week2", "week6", "week12"))
  data <- expand.grid(
    id = ids,
    visit = visit,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  data$visit <- factor(
    data$visit,
    levels = c("week2", "week6", "week12")
  )
  subject_index <- match(data$id, ids)
  arm_by_subject <- rep(c("control", "active"), each = 12L)
  baseline_by_subject <- stats::rnorm(24L, 50, 5)
  subject_effect <- stats::rnorm(24L, 0, 2)
  visit_number <- as.integer(data$visit)
  data$arm <- factor(
    arm_by_subject[subject_index],
    levels = c("control", "active")
  )
  data$baseline <- baseline_by_subject[subject_index]
  data$response <- 20 +
    0.4 * data$baseline +
    subject_effect[subject_index] +
    visit_number +
    1.5 * (data$arm == "active") * visit_number +
    stats::rnorm(nrow(data), 0, 1)
  data
}

test_that("MMRM wrapper matches the existing inference engine", {
  skip_if_not_installed("PhysioClinStats")
  skip_if_not_installed("mmrm")
  skip_if_not_installed("emmeans")
  data <- make_mmrm_fixture()
  result <- endpointMMRM(
    data,
    "response",
    "arm",
    "visit",
    "id",
    baseline = "baseline",
    control = "control",
    target_time = "week12",
    covariance = "unstructured",
    df = "kenward-roger"
  )
  direct_fit <- PhysioClinStats::fitMMRM(
    data,
    "response",
    "arm",
    "visit",
    "id",
    covariates = "baseline",
    covariance = "unstructured",
    df = "kenward-roger"
  )
  direct_emm <- PhysioClinStats::estimatedMarginalMeans(
    direct_fit,
    ~ arm | visit,
    contrasts = "trt.vs.ctrl",
    level = 0.95
  )
  direct <- PhysioCore::resultValue(direct_emm)$contrasts
  direct <- direct[as.character(direct$visit) == "week12", ]
  estimate <- result$estimates[1L, ]

  expect_s3_class(result, "endpoint_analysis")
  expect_identical(result$method, "mmrm")
  expect_identical(estimate$time, "week12")
  expect_equal(estimate$estimate, direct$estimate, tolerance = 1e-10)
  expect_equal(estimate$std_error, direct$SE, tolerance = 1e-10)
  expect_equal(estimate$df, direct$df, tolerance = 1e-10)
  expect_equal(estimate$statistic, direct$t.ratio, tolerance = 1e-10)
  expect_equal(estimate$p_value, direct$p.value, tolerance = 1e-10)

  selected <- data$visit == "week12"
  score <- data$response - data$baseline
  direct_d <- PhysioCore::cohensD(
    score[selected & data$arm == "active"],
    score[selected & data$arm == "control"]
  )
  expect_equal(
    c(estimate$cohens_d, estimate$d_ci_lower, estimate$d_ci_upper),
    c(direct_d$d, direct_d$ci_lower, direct_d$ci_upper),
    tolerance = 1e-12
  )
  expect_identical(result$effect_size_scale, "observed_change")
})

test_that("MMRM validates longitudinal identity and treatment state", {
  skip_if_not_installed("PhysioClinStats")
  skip_if_not_installed("mmrm")
  skip_if_not_installed("emmeans")
  data <- make_mmrm_fixture()

  duplicated <- rbind(data, data[1L, ])
  expect_error(
    endpointMMRM(
      duplicated, "response", "arm", "visit", "id",
      baseline = "baseline"
    ),
    "one row per subject/time"
  )

  switching <- data
  switching$arm[[1L]] <- "active"
  expect_error(
    endpointMMRM(
      switching, "response", "arm", "visit", "id",
      baseline = "baseline"
    ),
    "constant within each subject"
  )

  inconsistent_baseline <- data
  inconsistent_baseline$baseline[[1L]] <-
    inconsistent_baseline$baseline[[1L]] + 1
  expect_error(
    endpointMMRM(
      inconsistent_baseline, "response", "arm", "visit", "id",
      baseline = "baseline"
    ),
    "exactly one value per subject"
  )

  expect_error(
    endpointMMRM(
      data, "response", "arm", "visit", "id",
      baseline = "baseline", target_time = "unknown"
    ),
    "target_time"
  )
})

make_responder_fixture <- function() {
  arm_a <- c(8, 9, 10, 11, 12, 13, 5, 7, 0, 4, NA)
  arm_b <- c(8, 9, 10, 5, 6, 7, 0, 1, 2, 4, NA)
  data.frame(
    arm = factor(
      rep(c("A", "B"), each = 11L),
      levels = c("A", "B")
    ),
    baseline = 100,
    followup = 100 + c(arm_a, arm_b)
  )
}

test_that("responder analysis delegates classification and Wilson arithmetic", {
  skip_if_not_installed("PhysioClinical")
  data <- make_responder_fixture()
  rownames(data) <- paste0("subject-", seq_len(nrow(data)))
  result <- responderAnalysis(
    data,
    "baseline",
    "followup",
    "arm",
    mdc = 5,
    mcid = 8
  )
  direct <- PhysioClinical::classifyResponder(
    data$baseline,
    data$followup,
    mdc = 5,
    mcid = 8
  )
  expect_s3_class(result, "responder_analysis")
  expect_identical(result$subjects$row_id, rownames(data))
  expect_identical(result$subjects$classification, direct$classification)
  expect_equal(result$by_arm$n, c(11L, 11L))
  expect_equal(result$by_arm$n_evaluable, c(10L, 10L))
  expect_equal(result$by_arm$n_missing, c(1L, 1L))
  expect_equal(result$by_arm$n_true_responder, c(6L, 3L))
  expect_equal(result$by_arm$responder_rate, c(0.6, 0.3))

  wilson <- function(x, n, level = 0.95) {
    z <- stats::qnorm(1 - (1 - level) / 2)
    p <- x / n
    den <- 1 + z^2 / n
    centre <- (p + z^2 / (2 * n)) / den
    half <- z / den * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
    c(centre - half, centre + half)
  }
  expect_equal(
    unname(as.matrix(result$by_arm[c("ci_lower", "ci_upper")])),
    rbind(wilson(6, 10), wilson(3, 10)),
    tolerance = 1e-12
  )
})

test_that("responder thresholds, direction, and zero evaluable arms are explicit", {
  skip_if_not_installed("PhysioClinical")
  data <- data.frame(
    arm = factor(c("B", "A", "A"), levels = c("A", "B", "C")),
    baseline = c(10, 10, NA),
    followup = c(5, 2, NA)
  )
  result <- responderAnalysis(
    data,
    "baseline",
    "followup",
    "arm",
    mdc = 5,
    mcid = 8,
    direction = "decrease",
    control = "B"
  )
  expect_identical(result$by_arm$arm, c("B", "A", "C"))
  expect_identical(
    as.character(result$subjects$classification[1:2]),
    c("subclinical_change", "true_responder")
  )
  expect_true(is.na(result$by_arm$responder_rate[[3L]]))
  expect_true(is.na(result$by_arm$ci_lower[[3L]]))

  equality <- responderAnalysis(
    data.frame(arm = "A", baseline = 0, followup = 8),
    "baseline",
    "followup",
    "arm",
    mdc = 5,
    mcid = 8
  )
  expect_identical(
    as.character(equality$subjects$classification),
    "true_responder"
  )
  expect_error(
    responderAnalysis(
      data, "baseline", "followup", "arm",
      mdc = 0, mcid = 8
    ),
    "finite positive"
  )
})
