# Power / sample-size for rehabilitation trial designs.
#
# The vanilla two-sample t and two-proportion cases are computed from the
# noncentral t / normal approximation and cross-validated against base R's
# stats::power.t.test / power.prop.test. On top of these are the rehab-specific
# extensions base R lacks: ANCOVA baseline adjustment (the recommended pre-post
# analysis), change-from-baseline, repeated-measures (compound symmetry),
# cluster-randomization design effect, dropout inflation, unequal allocation, and
# estimation of the baseline-outcome correlation / ICC from a PhysioCohort.

# power of a two-sample t at (n1, n2=ratio*n1) for standardized effect d
.pt_power <- function(n1, ratio, d, sig_level, sides) {
  n2 <- ratio * n1
  ncp <- abs(d) / sqrt(1 / n1 + 1 / n2)
  df <- n1 + n2 - 2
  crit <- stats::qt(1 - sig_level / sides, df)
  up <- stats::pt(crit, df, ncp, lower.tail = FALSE)
  if (sides == 2L) up + stats::pt(-crit, df, ncp, lower.tail = TRUE) else up
}

# smallest control-arm n achieving target power (n2 = ratio * n1)
.solve_n1 <- function(d, power, sig_level, sides, ratio) {
  if (!is.finite(d) || d == 0) stop("effect size must be non-zero", call. = FALSE)
  n1 <- 2
  while (.pt_power(n1, ratio, d, sig_level, sides) < power) {
    n1 <- n1 + 1
    if (n1 > 1e7) stop("required n exceeds 1e7 - check inputs", call. = FALSE)
  }
  n1
}

# apply cluster design effect + dropout inflation + assemble the result object
.finalize_power <- function(n1, ratio, power, sig_level, sides, method, effect,
                            dropout = 0, cluster_size = NULL, icc = NULL) {
  n2 <- ceiling(ratio * n1); n1 <- ceiling(n1)
  de <- 1
  clusters <- NULL
  if (!is.null(cluster_size) && !is.null(icc)) {
    de <- 1 + (cluster_size - 1) * icc
    n1 <- ceiling(n1 * de); n2 <- ceiling(n2 * de)
    clusters <- c(arm1 = ceiling(n1 / cluster_size), arm2 = ceiling(n2 / cluster_size))
  }
  n_total <- n1 + n2
  n_enroll <- if (dropout > 0) ceiling(n_total / (1 - dropout)) else n_total
  structure(list(
    method = method, effect = effect, n1 = n1, n2 = n2, n_total = n_total,
    n_enrolled = n_enroll, power = power, sig_level = sig_level, sides = sides,
    allocation_ratio = ratio, dropout = dropout, design_effect = de,
    cluster_size = cluster_size, icc = icc, clusters = clusters),
    class = "trial_power")
}

#' Sample size for a two-arm continuous endpoint
#'
#' Two-sample comparison of means. Delegates the balanced case's core to the
#' noncentral-t (matching \code{stats::power.t.test}) and adds unequal
#' allocation, cluster design effect, and dropout inflation.
#'
#' @param delta Target difference in means (treatment - control).
#' @param sd Common (residual) standard deviation of the endpoint.
#' @param power Target power (default 0.8).
#' @param sig_level Two-sided (or one-sided, see \code{sides}) alpha (default 0.05).
#' @param ratio Allocation ratio n2/n1 (treatment/control; default 1).
#' @param sides 1 or 2 (default 2).
#' @param dropout Expected proportion lost to follow-up; inflates enrolment
#'   (default 0).
#' @param cluster_size Average subjects per cluster for a cluster-randomized
#'   design (\code{NULL} = individually randomized).
#' @param icc Intra-cluster correlation (required with \code{cluster_size}).
#' @return A \code{trial_power} object (\code{n1}, \code{n2}, \code{n_total},
#'   \code{n_enrolled}, design effect, clusters, ...).
#' @seealso [sampleSizeANCOVA()], [sampleSizeBinary()], [powerContinuous()]
#' @export
#' @examples
#' sampleSizeContinuous(delta = 5, sd = 10)          # d = 0.5 -> 64/arm
#' sampleSizeContinuous(delta = 5, sd = 10, dropout = 0.2)
sampleSizeContinuous <- function(delta, sd, power = 0.8, sig_level = 0.05,
                                 ratio = 1, sides = 2L, dropout = 0,
                                 cluster_size = NULL, icc = NULL) {
  stopifnot(is.numeric(delta), is.numeric(sd), sd > 0, power > 0, power < 1,
            sig_level > 0, sig_level < 1, ratio > 0, sides %in% c(1L, 2L),
            dropout >= 0, dropout < 1)
  d <- delta / sd
  n1 <- .solve_n1(d, power, sig_level, as.integer(sides), ratio)
  .finalize_power(n1, ratio, power, sig_level, as.integer(sides),
                  "two-sample continuous", c(delta = delta, sd = sd, d = d),
                  dropout, cluster_size, icc)
}

#' Sample size for a baseline-adjusted (ANCOVA) continuous endpoint
#'
#' The recommended pre-post rehabilitation analysis: adjusting the follow-up
#' outcome for its baseline reduces the residual variance by a factor
#' \eqn{(1-\rho^2)}, where \eqn{\rho} is the baseline-outcome correlation -
#' shrinking the required sample versus an unadjusted comparison.
#'
#' @param delta,sd,power,sig_level,ratio,sides,dropout,cluster_size,icc As in
#'   [sampleSizeContinuous()]. \code{sd} is the raw (unadjusted) outcome SD.
#' @param baseline_cor Correlation \eqn{\rho} between baseline and follow-up
#'   outcome (0-1); the variance is scaled by \eqn{(1-\rho^2)}.
#' @return A \code{trial_power} object.
#' @references Frison & Pocock (1992), Stat Med; Borm et al. (2007), J Clin Epi.
#' @seealso [sampleSizeChangeScore()], [estimateBaselineCorrelation()]
#' @export
#' @examples
#' sampleSizeANCOVA(delta = 5, sd = 10, baseline_cor = 0.6)   # < 64/arm
sampleSizeANCOVA <- function(delta, sd, baseline_cor, power = 0.8,
                             sig_level = 0.05, ratio = 1, sides = 2L,
                             dropout = 0, cluster_size = NULL, icc = NULL) {
  stopifnot(is.numeric(baseline_cor), baseline_cor >= 0, baseline_cor < 1)
  sd_eff <- sd * sqrt(1 - baseline_cor^2)
  res <- sampleSizeContinuous(delta, sd_eff, power, sig_level, ratio, sides,
                              dropout, cluster_size, icc)
  res$method <- "ANCOVA (baseline-adjusted)"
  res$effect <- c(delta = delta, sd = sd, baseline_cor = baseline_cor,
                  d_adjusted = delta / sd_eff)
  res
}

#' Sample size for a change-from-baseline continuous endpoint
#'
#' Analysis of the change score (follow-up minus baseline). The change SD is
#' \eqn{\sigma\sqrt{2(1-\rho)}}; change-score analysis beats an unadjusted
#' follow-up comparison only when \eqn{\rho > 0.5}, and is never better than
#' ANCOVA.
#'
#' @param delta,sd,baseline_cor,power,sig_level,ratio,sides,dropout,cluster_size,icc
#'   As in [sampleSizeANCOVA()].
#' @return A \code{trial_power} object.
#' @seealso [sampleSizeANCOVA()]
#' @export
sampleSizeChangeScore <- function(delta, sd, baseline_cor, power = 0.8,
                                  sig_level = 0.05, ratio = 1, sides = 2L,
                                  dropout = 0, cluster_size = NULL, icc = NULL) {
  stopifnot(is.numeric(baseline_cor), baseline_cor >= 0, baseline_cor < 1)
  sd_eff <- sd * sqrt(2 * (1 - baseline_cor))
  res <- sampleSizeContinuous(delta, sd_eff, power, sig_level, ratio, sides,
                              dropout, cluster_size, icc)
  res$method <- "change-from-baseline"
  res$effect <- c(delta = delta, sd = sd, baseline_cor = baseline_cor,
                  sd_change = sd_eff)
  res
}

#' Sample size for a repeated-measures continuous endpoint
#'
#' Comparison of the mean of \code{n_measurements} post-baseline measurements
#' under a compound-symmetry correlation \code{correlation} between them. The
#' effective variance is \eqn{\sigma^2 [1 + (r-1)\rho]/r}, so more visits help
#' most when the within-subject correlation is low.
#'
#' @param delta,sd,power,sig_level,ratio,sides,dropout,cluster_size,icc As in
#'   [sampleSizeContinuous()].
#' @param n_measurements Number of post-baseline measurements per subject.
#' @param correlation Compound-symmetry correlation between measurements (0-1).
#' @return A \code{trial_power} object.
#' @references Frison & Pocock (1992), Stat Med.
#' @seealso [sampleSizeANCOVA()]
#' @export
sampleSizeRepeatedMeasures <- function(delta, sd, n_measurements, correlation,
                                       power = 0.8, sig_level = 0.05, ratio = 1,
                                       sides = 2L, dropout = 0,
                                       cluster_size = NULL, icc = NULL) {
  stopifnot(is.numeric(n_measurements), n_measurements >= 1,
            is.numeric(correlation), correlation >= 0, correlation < 1)
  r <- n_measurements
  sd_eff <- sd * sqrt((1 + (r - 1) * correlation) / r)
  res <- sampleSizeContinuous(delta, sd_eff, power, sig_level, ratio, sides,
                              dropout, cluster_size, icc)
  res$method <- sprintf("repeated measures (%d visits, rho=%.2f)", as.integer(r),
                        correlation)
  res$effect <- c(delta = delta, sd = sd, n_measurements = r,
                  correlation = correlation, sd_effective = sd_eff)
  res
}

#' Sample size for a two-arm binary / responder endpoint
#'
#' Comparison of two proportions (e.g. responder rates), via the normal
#' approximation with unequal allocation; matches \code{stats::power.prop.test}
#' in the balanced case.
#'
#' @param p1 Control-arm proportion.
#' @param p2 Treatment-arm proportion.
#' @param power,sig_level,ratio,sides,dropout,cluster_size,icc As in
#'   [sampleSizeContinuous()].
#' @return A \code{trial_power} object.
#' @seealso [sampleSizeContinuous()], [powerBinary()]
#' @export
#' @examples
#' sampleSizeBinary(p1 = 0.30, p2 = 0.50)      # responder-rate 30% vs 50%
sampleSizeBinary <- function(p1, p2, power = 0.8, sig_level = 0.05, ratio = 1,
                             sides = 2L, dropout = 0, cluster_size = NULL,
                             icc = NULL) {
  stopifnot(p1 > 0, p1 < 1, p2 > 0, p2 < 1, p1 != p2, power > 0, power < 1,
            ratio > 0, sides %in% c(1L, 2L))
  za <- stats::qnorm(1 - sig_level / sides); zb <- stats::qnorm(power)
  pbar <- (p1 + ratio * p2) / (1 + ratio)
  num <- (za * sqrt((1 + 1 / ratio) * pbar * (1 - pbar)) +
            zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / ratio))^2
  n1 <- num / (p1 - p2)^2
  res <- .finalize_power(n1, ratio, power, sig_level, as.integer(sides),
                         "two-proportion", c(p1 = p1, p2 = p2),
                         dropout, cluster_size, icc)
  res
}

#' Power of a two-arm continuous endpoint at a given sample size
#'
#' @param n1 Control-arm sample size.
#' @param delta,sd,sig_level,ratio,sides As in [sampleSizeContinuous()].
#' @return The achieved power (numeric in 0-1).
#' @seealso [sampleSizeContinuous()], [powerCurve()]
#' @export
#' @examples
#' powerContinuous(64, delta = 5, sd = 10)     # ~0.80
powerContinuous <- function(n1, delta, sd, sig_level = 0.05, ratio = 1,
                            sides = 2L) {
  stopifnot(n1 >= 2, sd > 0)
  .pt_power(n1, ratio, delta / sd, sig_level, as.integer(sides))
}

#' Power of a two-arm binary endpoint at a given sample size
#' @param n1 Control-arm sample size.
#' @param p1,p2,sig_level,ratio,sides As in [sampleSizeBinary()].
#' @return The achieved power.
#' @seealso [sampleSizeBinary()]
#' @export
powerBinary <- function(n1, p1, p2, sig_level = 0.05, ratio = 1, sides = 2L) {
  stopifnot(n1 >= 2, p1 > 0, p1 < 1, p2 > 0, p2 < 1)
  n2 <- ratio * n1
  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  z <- abs(p1 - p2) / se - stats::qnorm(1 - sig_level / sides)
  stats::pnorm(z)
}

#' Power curve over a range of sample sizes or effect sizes
#'
#' @param delta,sd,sig_level,ratio,sides As in [sampleSizeContinuous()].
#' @param n1 A vector of control-arm sizes to evaluate (vary n at fixed effect),
#'   or \code{NULL} to vary the effect instead.
#' @param deltas A vector of effect differences to evaluate at fixed \code{n1}
#'   (used when \code{n1} is a single value).
#' @return A \code{data.frame} with columns \code{n1}, \code{delta}, \code{power}.
#' @seealso [powerContinuous()]
#' @export
#' @examples
#' powerCurve(delta = 5, sd = 10, n1 = seq(20, 120, 10))
powerCurve <- function(delta, sd, n1 = NULL, deltas = NULL, sig_level = 0.05,
                       ratio = 1, sides = 2L) {
  if (!is.null(n1) && length(n1) > 1L) {
    data.frame(n1 = n1, delta = delta,
               power = vapply(n1, powerContinuous, numeric(1), delta, sd,
                              sig_level, ratio, sides))
  } else if (!is.null(deltas)) {
    nn <- if (is.null(n1)) stop("supply n1 when varying deltas", call. = FALSE) else n1[1]
    data.frame(n1 = nn, delta = deltas,
               power = vapply(deltas, function(dd) powerContinuous(nn, dd, sd,
                              sig_level, ratio, sides), numeric(1)))
  } else stop("supply a vector 'n1' or a vector 'deltas'", call. = FALSE)
}

#' Estimate the baseline-outcome correlation (for ANCOVA planning)
#'
#' From pilot data - a pair of numeric vectors, a \code{data.frame} with a
#' baseline and follow-up column, or a \code{PhysioCohort} \code{cohortDesign()}
#' plus column names - estimate \eqn{\rho} to feed [sampleSizeANCOVA()].
#'
#' @param baseline A numeric vector of baseline values, a \code{data.frame}, or a
#'   \code{PhysioCohort}.
#' @param followup A numeric vector of follow-up values (when \code{baseline} is
#'   a vector).
#' @param cols Length-2 character vector naming the baseline and follow-up
#'   columns (when \code{baseline} is a \code{data.frame} / cohort design).
#' @param method Correlation method (default \code{"pearson"}).
#' @return The estimated correlation (numeric).
#' @seealso [sampleSizeANCOVA()]
#' @export
estimateBaselineCorrelation <- function(baseline, followup = NULL, cols = NULL,
                                        method = "pearson") {
  if (is.numeric(baseline) && is.numeric(followup)) {
    return(stats::cor(baseline, followup, use = "complete.obs", method = method))
  }
  df <- if (methods::is(baseline, "PhysioCohort")) PhysioCore::cohortData(baseline)
        else baseline
  df <- as.data.frame(df)
  if (is.null(cols) || length(cols) != 2L)
    stop("supply cols = c('baseline_col', 'followup_col')", call. = FALSE)
  stats::cor(df[[cols[1]]], df[[cols[2]]], use = "complete.obs", method = method)
}

#' @export
print.trial_power <- function(x, ...) {
  cat(sprintf("<trial_power> %s\n", x$method))
  eff <- paste(sprintf("%s=%.4g", names(x$effect), x$effect), collapse = ", ")
  cat(sprintf("  effect: %s\n", eff))
  cat(sprintf("  power %.2f | alpha %.3g (%d-sided) | allocation %.2g:1\n",
              x$power, x$sig_level, x$sides, x$allocation_ratio))
  cat(sprintf("  n per arm: control %d, treatment %d  (total %d)\n",
              x$n1, x$n2, x$n_total))
  if (!is.null(x$clusters))
    cat(sprintf("  cluster design: DE %.2f (ICC %.3g, size %g) -> clusters %d + %d\n",
                x$design_effect, x$icc, x$cluster_size, x$clusters[1], x$clusters[2]))
  if (x$dropout > 0)
    cat(sprintf("  enrol %d to allow %.0f%% dropout\n", x$n_enrolled, 100 * x$dropout))
  invisible(x)
}
