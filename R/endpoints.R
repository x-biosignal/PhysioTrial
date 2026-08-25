.validate_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be one finite number between zero and one.",
         call. = FALSE)
  }
  conf_level
}

.endpoint_columns <- function(data, columns, argument, allow_null = TRUE) {
  if (is.null(columns) && allow_null) {
    return(character(0))
  }
  if (!is.character(columns) || (!length(columns) && !allow_null) ||
      anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop(argument, " must contain unique, non-empty column names.",
         call. = FALSE)
  }
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      argument,
      " contains columns absent from data: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  columns
}

.endpoint_numeric <- function(x, name, allow_na = TRUE) {
  if (!is.numeric(x) ||
      any(!is.na(x) & !is.finite(x)) ||
      (!allow_na && anyNA(x))) {
    stop(
      name,
      " must be numeric with finite non-missing values.",
      call. = FALSE
    )
  }
  as.numeric(x)
}

.endpoint_treatment <- function(x, control = NULL) {
  value <- as.character(x)
  observed <- unique(value[!is.na(value)])
  if (any(!is.na(value) & !nzchar(value)) || length(observed) < 2L) {
    stop("treatment must contain at least two non-empty observed arms.",
         call. = FALSE)
  }
  if (is.null(control)) {
    control <- observed[[1L]]
  } else if (!.is_scalar_string(control) || !control %in% observed) {
    stop("control must identify one observed treatment arm.", call. = FALSE)
  }
  levels <- c(control, setdiff(observed, control))
  list(
    factor = factor(value, levels = levels),
    control = control,
    levels = levels
  )
}

.cohens_d_row <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2L || length(y) < 2L) {
    return(c(d = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  effect <- PhysioCore::cohensD(x, y, pooled = TRUE)
  c(
    d = as.numeric(effect$d),
    lower = as.numeric(effect$ci_lower),
    upper = as.numeric(effect$ci_upper)
  )
}

#' ANCOVA Endpoint Analysis
#'
#' Fits change-from-baseline or follow-up ANCOVA with treatment referenced to
#' an explicit control. Reported Cohen d values are unadjusted change-score
#' effects delegated to [PhysioCore::cohensD()] and are separate from the
#' adjusted ANCOVA estimate.
#'
#' @param data Participant-level endpoint data.
#' @param followup,baseline,treatment Scalar column names.
#' @param covariates Optional additional model columns.
#' @param control Control-arm label; defaults to the first observed arm.
#' @param response_scale `"change"` or `"followup"`.
#' @param conf_level Confidence level for adjusted treatment estimates.
#' @return An `endpoint_analysis` with standardized estimates, the `lm` fit,
#'   complete analysis data, and missingness counts.
#' @examples
#' d <- data.frame(
#'   arm = rep(c("control", "active"), each = 4),
#'   baseline = rep(0:3, 2),
#'   followup = c(1, 2, 4, 5, 3, 4, 6, 7)
#' )
#' endpointANCOVA(d, "followup", "baseline", "arm")
#' @export
endpointANCOVA <- function(
  data,
  followup,
  baseline,
  treatment,
  covariates = NULL,
  control = NULL,
  response_scale = c("change", "followup"),
  conf_level = 0.95
) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("data must be a non-empty data.frame.", call. = FALSE)
  }
  response_scale <- match.arg(response_scale)
  conf_level <- .validate_conf_level(conf_level)
  followup <- .analysis_column(data, followup, "followup")
  baseline <- .analysis_column(data, baseline, "baseline")
  treatment <- .analysis_column(data, treatment, "treatment")
  covariates <- .endpoint_columns(data, covariates, "covariates")
  if (anyDuplicated(c(followup, baseline, treatment, covariates))) {
    stop("Endpoint and covariate columns must be distinct.", call. = FALSE)
  }
  followup_value <- .endpoint_numeric(data[[followup]], "followup")
  baseline_value <- .endpoint_numeric(data[[baseline]], "baseline")
  model_columns <- c(followup, baseline, treatment, covariates)
  complete <- stats::complete.cases(data[model_columns])
  used <- data[complete, , drop = FALSE]
  if (!nrow(used)) {
    stop("No complete endpoint observations are available.", call. = FALSE)
  }
  treatment_info <- .endpoint_treatment(used[[treatment]], control)
  arm_counts <- table(treatment_info$factor)
  if (any(arm_counts < 2L)) {
    stop("ANCOVA requires at least two complete observations per arm.",
         call. = FALSE)
  }
  change <- followup_value[complete] - baseline_value[complete]
  response <- if (response_scale == "change") {
    change
  } else {
    followup_value[complete]
  }
  model_data <- data.frame(
    .response = response,
    .treatment = treatment_info$factor,
    .baseline = baseline_value[complete],
    check.names = FALSE
  )
  model_covariates <- character(0)
  for (index in seq_along(covariates)) {
    name <- paste0(".covariate", index)
    value <- used[[covariates[[index]]]]
    if (is.numeric(value) && any(!is.finite(value))) {
      stop("Numeric covariates must be finite in complete rows.",
           call. = FALSE)
    }
    model_data[[name]] <- value
    model_covariates <- c(model_covariates, name)
  }
  formula <- stats::reformulate(
    c(".treatment", ".baseline", model_covariates),
    response = ".response"
  )
  fit <- stats::lm(formula, data = model_data)
  design <- stats::model.matrix(fit)
  if (fit$rank < ncol(design) || anyNA(stats::coef(fit))) {
    stop("ANCOVA design is rank deficient.", call. = FALSE)
  }
  coefficients <- summary(fit)$coefficients
  df_residual <- fit$df.residual
  critical <- stats::qt(
    1 - (1 - conf_level) / 2,
    df = df_residual
  )
  rows <- lapply(treatment_info$levels[-1L], function(arm) {
    term <- paste0(".treatment", arm)
    if (!term %in% rownames(coefficients)) {
      stop("Could not resolve the treatment contrast for arm '", arm, "'.",
           call. = FALSE)
    }
    coefficient <- coefficients[term, , drop = TRUE]
    active_change <- change[treatment_info$factor == arm]
    control_change <- change[
      treatment_info$factor == treatment_info$control
    ]
    effect <- .cohens_d_row(active_change, control_change)
    data.frame(
      arm = arm,
      control = treatment_info$control,
      estimate = unname(coefficient[["Estimate"]]),
      std_error = unname(coefficient[["Std. Error"]]),
      df = as.numeric(df_residual),
      statistic = unname(coefficient[["t value"]]),
      p_value = unname(coefficient[["Pr(>|t|)"]]),
      ci_lower = unname(coefficient[["Estimate"]] -
        critical * coefficient[["Std. Error"]]),
      ci_upper = unname(coefficient[["Estimate"]] +
        critical * coefficient[["Std. Error"]]),
      cohens_d = unname(effect[["d"]]),
      d_ci_lower = unname(effect[["lower"]]),
      d_ci_upper = unname(effect[["upper"]]),
      n_arm = as.integer(sum(treatment_info$factor == arm)),
      n_control = as.integer(sum(
        treatment_info$factor == treatment_info$control
      )),
      stringsAsFactors = FALSE
    )
  })
  used$change <- change
  rownames(used) <- NULL
  structure(
    list(
      method = paste0("ancova_", response_scale),
      estimates = do.call(rbind, rows),
      model = fit,
      data = used,
      n_input = as.integer(nrow(data)),
      n_used = as.integer(nrow(used)),
      n_missing = as.integer(nrow(data) - nrow(used)),
      effect_size_scale = "unadjusted_change"
    ),
    class = "endpoint_analysis"
  )
}

.validate_mmrm_input <- function(
  data,
  response,
  treatment,
  time,
  subject,
  baseline,
  covariates,
  control
) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("data must be a non-empty data.frame.", call. = FALSE)
  }
  response <- .analysis_column(data, response, "response")
  treatment <- .analysis_column(data, treatment, "treatment")
  time <- .analysis_column(data, time, "time")
  subject <- .analysis_column(data, subject, "subject")
  baseline <- .endpoint_columns(data, baseline, "baseline")
  if (length(baseline) > 1L) {
    stop("baseline must be NULL or one column name.", call. = FALSE)
  }
  covariates <- .endpoint_columns(data, covariates, "covariates")
  columns <- c(response, treatment, time, subject, baseline, covariates)
  if (anyDuplicated(columns)) {
    stop("MMRM role columns must be distinct.", call. = FALSE)
  }
  response_value <- .endpoint_numeric(data[[response]], "response")
  subject_value <- as.character(data[[subject]])
  time_value <- data[[time]]
  if (anyNA(subject_value) || any(!nzchar(subject_value)) ||
      anyNA(time_value) ||
      (is.numeric(time_value) && any(!is.finite(time_value)))) {
    stop("MMRM subject and time values must be non-empty/non-missing.",
         call. = FALSE)
  }
  if (anyDuplicated(data[c(subject, time)])) {
    stop("MMRM requires one row per subject/time key.", call. = FALSE)
  }
  treatment_value <- as.character(data[[treatment]])
  if (anyNA(treatment_value) || any(!nzchar(treatment_value))) {
    stop("MMRM treatment assignments must be non-empty and non-missing.",
         call. = FALSE)
  }
  treatment_by_subject <- split(treatment_value, subject_value)
  if (any(vapply(treatment_by_subject, function(x) {
    length(unique(x)) != 1L
  }, logical(1)))) {
    stop("Treatment must remain constant within each subject.",
         call. = FALSE)
  }
  treatment_info <- .endpoint_treatment(treatment_value, control)
  baseline_value <- NULL
  if (length(baseline)) {
    baseline_value <- .endpoint_numeric(
      data[[baseline]],
      "baseline",
      allow_na = FALSE
    )
    baseline_by_subject <- split(baseline_value, subject_value)
    if (any(vapply(baseline_by_subject, function(x) {
      length(unique(x)) != 1L
    }, logical(1)))) {
      stop("baseline must have exactly one value per subject.",
           call. = FALSE)
    }
  }
  list(
    response = response,
    treatment = treatment,
    time = time,
    subject = subject,
    baseline = baseline,
    covariates = covariates,
    response_value = response_value,
    subject_value = subject_value,
    time_value = time_value,
    treatment_info = treatment_info,
    baseline_value = baseline_value
  )
}

.mmrm_standard_data <- function(data, validated) {
  time <- validated$time_value
  time_levels <- if (is.factor(time)) {
    levels(droplevels(time))
  } else {
    unique(as.character(time))
  }
  output <- data.frame(
    .response = validated$response_value,
    .treatment = validated$treatment_info$factor,
    .time = factor(as.character(time), levels = time_levels),
    .subject = factor(validated$subject_value),
    stringsAsFactors = FALSE
  )
  model_covariates <- character(0)
  if (length(validated$baseline)) {
    output$.baseline <- validated$baseline_value
    model_covariates <- ".baseline"
  }
  for (index in seq_along(validated$covariates)) {
    name <- paste0(".covariate", index)
    value <- data[[validated$covariates[[index]]]]
    if (is.numeric(value) && any(!is.na(value) & !is.finite(value))) {
      stop("Numeric MMRM covariates must be finite when non-missing.",
           call. = FALSE)
    }
    output[[name]] <- value
    model_covariates <- c(model_covariates, name)
  }
  list(data = output, covariates = model_covariates)
}

.mmrm_effect_size <- function(
  data,
  arm,
  control,
  time,
  use_change
) {
  selected_time <- data$.time == time
  score <- if (use_change) {
    data$.response - data$.baseline
  } else {
    data$.response
  }
  .cohens_d_row(
    score[selected_time & data$.treatment == arm],
    score[selected_time & data$.treatment == control]
  )
}

#' MMRM Endpoint Analysis
#'
#' Delegates longitudinal modelling and estimated marginal means to
#' `PhysioClinStats`. No alternate MMRM or complete-case ANCOVA fallback is
#' implemented here.
#'
#' @param data Longitudinal endpoint data.
#' @param response,treatment,time,subject Scalar column names.
#' @param baseline Optional repeated subject-level baseline column.
#' @param covariates Optional additional fixed-effect columns.
#' @param control Control-arm label.
#' @param target_time Optional one requested time level.
#' @param covariance MMRM covariance structure.
#' @param df Denominator degrees-of-freedom method.
#' @param conf_level Confidence level.
#' @return An `endpoint_analysis` carrying standardized treatment contrasts and
#'   the underlying `PhysioClinStats` result objects.
#' @export
endpointMMRM <- function(
  data,
  response,
  treatment,
  time,
  subject,
  baseline = NULL,
  covariates = NULL,
  control = NULL,
  target_time = NULL,
  covariance = c(
    "unstructured", "ar1", "compound-symmetry", "toeplitz"
  ),
  df = c("kenward-roger", "satterthwaite"),
  conf_level = 0.95
) {
  covariance <- match.arg(covariance)
  df <- match.arg(df)
  conf_level <- .validate_conf_level(conf_level)
  if (!requireNamespace("PhysioClinStats", quietly = TRUE)) {
    stop("PhysioClinStats is required for endpointMMRM().", call. = FALSE)
  }
  validated <- .validate_mmrm_input(
    data,
    response,
    treatment,
    time,
    subject,
    baseline,
    covariates,
    control
  )
  standardized <- .mmrm_standard_data(data, validated)
  model_data <- standardized$data
  fit <- PhysioClinStats::fitMMRM(
    model_data,
    ".response",
    ".treatment",
    ".time",
    ".subject",
    covariates = standardized$covariates,
    covariance = covariance,
    df = df
  )
  marginal <- PhysioClinStats::estimatedMarginalMeans(
    fit,
    specs = ~ .treatment | .time,
    contrasts = "trt.vs.ctrl",
    level = conf_level
  )
  contrasts <- PhysioCore::resultValue(marginal)$contrasts
  required <- c(
    "contrast", ".time", "estimate", "SE", "df", "t.ratio", "p.value"
  )
  if (!is.data.frame(contrasts) ||
      any(!required %in% names(contrasts))) {
    stop("PhysioClinStats returned an unsupported contrast schema.",
         call. = FALSE)
  }
  contrasts$.time <- as.character(contrasts$.time)
  if (!is.null(target_time)) {
    if (length(target_time) != 1L || is.na(target_time)) {
      stop("target_time must identify exactly one time level.",
           call. = FALSE)
    }
    target_time <- as.character(target_time)
    if (sum(unique(contrasts$.time) == target_time) != 1L) {
      stop("target_time must match exactly one observed time level.",
           call. = FALSE)
    }
    contrasts <- contrasts[
      contrasts$.time == target_time,
      ,
      drop = FALSE
    ]
  }
  control <- validated$treatment_info$control
  contrast_map <- stats::setNames(
    validated$treatment_info$levels[-1L],
    paste0(validated$treatment_info$levels[-1L], " - ", control)
  )
  arm <- unname(contrast_map[as.character(contrasts$contrast)])
  if (anyNA(arm)) {
    stop("Could not map a PhysioClinStats treatment contrast to an arm.",
         call. = FALSE)
  }
  critical <- stats::qt(
    1 - (1 - conf_level) / 2,
    df = contrasts$df
  )
  effects <- t(vapply(seq_len(nrow(contrasts)), function(i) {
    .mmrm_effect_size(
      model_data,
      arm[[i]],
      control,
      contrasts$.time[[i]],
      use_change = length(validated$baseline) > 0L
    )
  }, numeric(3)))
  estimates <- data.frame(
    time = contrasts$.time,
    arm = arm,
    control = control,
    estimate = as.numeric(contrasts$estimate),
    std_error = as.numeric(contrasts$SE),
    df = as.numeric(contrasts$df),
    statistic = as.numeric(contrasts$t.ratio),
    p_value = as.numeric(contrasts$p.value),
    ci_lower = as.numeric(contrasts$estimate - critical * contrasts$SE),
    ci_upper = as.numeric(contrasts$estimate + critical * contrasts$SE),
    cohens_d = effects[, "d"],
    d_ci_lower = effects[, "lower"],
    d_ci_upper = effects[, "upper"],
    stringsAsFactors = FALSE
  )
  rownames(estimates) <- NULL
  structure(
    list(
      method = "mmrm",
      estimates = estimates,
      model = fit,
      marginal_means = marginal,
      data = model_data,
      n_input = as.integer(nrow(data)),
      n_used = as.integer(sum(stats::complete.cases(
        model_data[c(".response", standardized$covariates)]
      ))),
      n_missing = as.integer(sum(!stats::complete.cases(
        model_data[c(".response", standardized$covariates)]
      ))),
      effect_size_scale = if (length(validated$baseline)) {
        "observed_change"
      } else {
        "observed_response"
      }
    ),
    class = "endpoint_analysis"
  )
}

#' @export
print.endpoint_analysis <- function(x, ...) {
  cat(
    "<endpoint_analysis> ", x$method, ": ",
    nrow(x$estimates), " contrast(s), ",
    x$n_used, "/", x$n_input, " observations used\n",
    sep = ""
  )
  print(x$estimates, row.names = FALSE, ...)
  invisible(x)
}

.responder_arm_levels <- function(treatment, control) {
  value <- as.character(treatment)
  if (anyNA(value) || any(!nzchar(value))) {
    stop("treatment must be non-empty and non-missing.", call. = FALSE)
  }
  levels <- if (is.factor(treatment)) {
    levels(treatment)
  } else {
    unique(value)
  }
  if (!length(levels) || anyNA(levels) || any(!nzchar(levels))) {
    stop("treatment must define at least one valid arm.", call. = FALSE)
  }
  if (!is.null(control)) {
    if (!.is_scalar_string(control) || !control %in% levels) {
      stop("control must identify one treatment arm.", call. = FALSE)
    }
    levels <- c(control, setdiff(levels, control))
  }
  levels
}

.wilson_interval <- function(x, n, conf_level) {
  if (!n) {
    return(c(rate = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p <- x / n
  denominator <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denominator
  half <- z / denominator * sqrt(
    p * (1 - p) / n + z^2 / (4 * n^2)
  )
  c(rate = p, lower = centre - half, upper = centre + half)
}

#' MDC/MCID Responder Analysis
#'
#' Delegates each subject's dual-threshold classification to
#' [PhysioClinical::classifyResponder()] and summarizes true responders with
#' Wilson score intervals. Missing outcomes remain explicit and are excluded
#' from the rate denominator.
#'
#' @param data Participant-level data.
#' @param baseline,followup,treatment Scalar column names.
#' @param mdc,mcid Positive MDC and MCID thresholds.
#' @param direction Whether improvement is an increase or decrease.
#' @param control Optional control arm placed first.
#' @param conf_level Wilson interval confidence level.
#' @return A `responder_analysis` with subject classifications and arm
#'   summaries.
#' @export
responderAnalysis <- function(
  data,
  baseline,
  followup,
  treatment,
  mdc,
  mcid,
  direction = c("increase", "decrease"),
  control = NULL,
  conf_level = 0.95
) {
  direction <- match.arg(direction)
  conf_level <- .validate_conf_level(conf_level)
  if (!requireNamespace("PhysioClinical", quietly = TRUE)) {
    stop("PhysioClinical is required for responderAnalysis().",
         call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  baseline <- .analysis_column(data, baseline, "baseline")
  followup <- .analysis_column(data, followup, "followup")
  treatment <- .analysis_column(data, treatment, "treatment")
  if (anyDuplicated(c(baseline, followup, treatment))) {
    stop("baseline, followup, and treatment columns must be distinct.",
         call. = FALSE)
  }
  if (!is.numeric(mdc) || length(mdc) != 1L || !is.finite(mdc) ||
      mdc <= 0 ||
      !is.numeric(mcid) || length(mcid) != 1L || !is.finite(mcid) ||
      mcid <= 0) {
    stop("mdc and mcid must each be one finite positive number.",
         call. = FALSE)
  }
  baseline_value <- .endpoint_numeric(data[[baseline]], "baseline")
  followup_value <- .endpoint_numeric(data[[followup]], "followup")
  arm_levels <- .responder_arm_levels(data[[treatment]], control)
  classification <- PhysioClinical::classifyResponder(
    baseline_value,
    followup_value,
    mdc = mdc,
    mcid = mcid,
    direction = direction
  )
  arm <- as.character(data[[treatment]])
  subjects <- data.frame(
    row_id = rownames(data),
    arm = factor(arm, levels = arm_levels),
    change = classification$change,
    improvement = classification$improvement,
    exceeds_mdc = classification$exceeds_mdc,
    exceeds_mcid = classification$exceeds_mcid,
    classification = classification$classification,
    stringsAsFactors = FALSE
  )
  by_arm <- do.call(rbind, lapply(arm_levels, function(arm_name) {
    selected <- arm == arm_name
    evaluable <- selected & !is.na(classification$classification)
    responder <- evaluable &
      as.character(classification$classification) == "true_responder"
    n_evaluable <- sum(evaluable)
    interval <- .wilson_interval(
      sum(responder),
      n_evaluable,
      conf_level
    )
    data.frame(
      arm = arm_name,
      n = as.integer(sum(selected)),
      n_evaluable = as.integer(n_evaluable),
      n_missing = as.integer(sum(selected) - n_evaluable),
      n_true_responder = as.integer(sum(responder)),
      responder_rate = unname(interval[["rate"]]),
      ci_lower = unname(interval[["lower"]]),
      ci_upper = unname(interval[["upper"]]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_arm) <- NULL
  structure(
    list(
      subjects = subjects,
      by_arm = by_arm,
      thresholds = list(
        mdc = as.numeric(mdc),
        mcid = as.numeric(mcid),
        direction = direction
      )
    ),
    class = "responder_analysis"
  )
}

#' @export
print.responder_analysis <- function(x, ...) {
  cat(
    "<responder_analysis> MDC=", x$thresholds$mdc,
    ", MCID=", x$thresholds$mcid,
    ", direction=", x$thresholds$direction, "\n",
    sep = ""
  )
  print(x$by_arm, row.names = FALSE, ...)
  invisible(x)
}
