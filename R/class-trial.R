.is_scalar_string <- function(x, allow_na = FALSE) {
  is.character(x) && length(x) == 1L &&
    ((allow_na && is.na(x)) || (!is.na(x) && nzchar(x)))
}

.whole_number <- function(x) {
  is.numeric(x) && all(is.finite(x)) &&
    all(x == floor(x)) && all(x <= .Machine$integer.max)
}

.validate_ratio <- function(x, labels, name = "allocation_ratio") {
  if (!is.numeric(x) || length(x) != length(labels) ||
      !.whole_number(x) || any(x < 1)) {
    stop(name, " must contain one positive integer per arm.", call. = FALSE)
  }
  if (!is.null(names(x))) {
    if (anyNA(names(x)) || any(!nzchar(names(x))) ||
        !setequal(names(x), labels)) {
      stop(name, " names must match the arm labels.", call. = FALSE)
    }
    x <- x[labels]
  }
  stats::setNames(as.integer(x), labels)
}

.valid_arm <- function(object) {
  if (!.is_scalar_string(object@label)) {
    return("label must be one non-empty string")
  }
  if (length(object@ratio) != 1L || is.na(object@ratio) ||
      object@ratio < 1L) {
    return("ratio must be one positive integer")
  }
  if (!.is_scalar_string(object@description, allow_na = TRUE)) {
    return("description must be one string or NA")
  }
  if (length(object@is_control) != 1L || is.na(object@is_control)) {
    return("is_control must be one non-missing logical")
  }
  if (!.is_scalar_string(object@treatment_code, allow_na = TRUE)) {
    return("treatment_code must be one string or NA")
  }
  TRUE
}

#' Trial Arm
#'
#' @slot label Arm label.
#' @slot ratio Positive integer allocation weight.
#' @slot description Optional description.
#' @slot is_control Whether this is a control arm.
#' @slot treatment_code Optional masked treatment code.
#' @exportClass Arm
methods::setClass(
  "Arm",
  slots = c(
    label = "character",
    ratio = "integer",
    description = "character",
    is_control = "logical",
    treatment_code = "character"
  ),
  validity = .valid_arm
)

#' Construct a Trial Arm
#'
#' @param label Non-empty arm label.
#' @param ratio Positive integer allocation weight.
#' @param description Optional description.
#' @param is_control Whether this is a control arm.
#' @param treatment_code Optional masked treatment code.
#' @return An [Arm] object.
#' @examples
#' Arm("active", ratio = 2L, is_control = FALSE)
#' @export
Arm <- function(label, ratio = 1L, description = NA_character_,
                is_control = FALSE, treatment_code = NA_character_) {
  if (!.whole_number(ratio) || length(ratio) != 1L || ratio < 1) {
    stop("ratio must be one positive integer.", call. = FALSE)
  }
  methods::new(
    "Arm",
    label = as.character(label),
    ratio = as.integer(ratio),
    description = as.character(description),
    is_control = as.logical(is_control),
    treatment_code = as.character(treatment_code)
  )
}

.valid_participant <- function(object) {
  if (!.is_scalar_string(object@id)) {
    return("id must be one non-empty string")
  }
  if (length(object@strata)) {
    nm <- names(object@strata)
    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) ||
        anyDuplicated(nm)) {
      return("strata must have unique, non-empty names")
    }
    if (anyNA(object@strata) || any(!nzchar(object@strata))) {
      return("strata values must be non-empty and non-missing")
    }
  }
  if (!.is_scalar_string(object@arm, allow_na = TRUE)) {
    return("arm must be one string or NA")
  }
  if (!.is_scalar_string(object@enrolled_at, allow_na = TRUE)) {
    return("enrolled_at must be one string or NA")
  }
  TRUE
}

#' Trial Participant
#'
#' @slot id Participant identifier.
#' @slot strata Named character vector of prognostic-factor levels.
#' @slot arm Allocated arm, or `NA` before allocation.
#' @slot enrolled_at Optional enrollment time representation.
#' @exportClass Participant
methods::setClass(
  "Participant",
  slots = c(
    id = "character",
    strata = "character",
    arm = "character",
    enrolled_at = "character"
  ),
  validity = .valid_participant
)

#' Construct a Trial Participant
#'
#' @param id Non-empty participant identifier.
#' @param strata Named character vector mapping prognostic factors to levels.
#' @param arm Allocated arm, or `NA` before allocation.
#' @param enrolled_at Optional enrollment time representation.
#' @return A [Participant] object.
#' @examples
#' Participant("P001", strata = c(site = "north"))
#' @export
Participant <- function(id, strata = character(0), arm = NA_character_,
                        enrolled_at = NA_character_) {
  strata_values <- as.character(strata)
  names(strata_values) <- names(strata)
  methods::new(
    "Participant",
    id = as.character(id),
    strata = strata_values,
    arm = as.character(arm),
    enrolled_at = as.character(enrolled_at)
  )
}

.valid_trial <- function(object) {
  if (!.is_scalar_string(object@id)) {
    return("id must be one non-empty string")
  }
  if (length(object@arms) < 2L || anyNA(object@arms) ||
      any(!nzchar(object@arms)) || anyDuplicated(object@arms)) {
    return("arms must contain at least two unique, non-empty labels")
  }
  if (length(object@arm_specs) != length(object@arms) ||
      !all(vapply(object@arm_specs, methods::is, logical(1), "Arm")) ||
      !identical(vapply(
        object@arm_specs, methods::slot, character(1), "label"
      ),
                 object@arms)) {
    return("arm_specs must contain one matching Arm object per arm")
  }
  if (!identical(names(object@allocation_ratio), object@arms) ||
      length(object@allocation_ratio) != length(object@arms) ||
      anyNA(object@allocation_ratio) || any(object@allocation_ratio < 1L)) {
    return("allocation_ratio must be a named positive-integer vector in arm order")
  }
  if (length(object@strata)) {
    nm <- names(object@strata)
    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) ||
        anyDuplicated(nm)) {
      return("strata must be a uniquely named list")
    }
    valid_levels <- vapply(object@strata, function(x) {
      is.character(x) && length(x) > 0L && !anyNA(x) &&
        all(nzchar(x)) && !anyDuplicated(x)
    }, logical(1))
    if (!all(valid_levels)) {
      return("each strata entry must contain unique, non-empty character levels")
    }
  }
  TRUE
}

#' Randomized Trial Definition
#'
#' @slot id Trial identifier.
#' @slot arms Ordered arm labels.
#' @slot arm_specs Arm definitions.
#' @slot allocation_ratio Named integer allocation weights.
#' @slot strata Named list of prognostic factors and admissible levels.
#' @slot endpoints Endpoint definitions carried by the trial.
#' @slot metadata Free-form trial metadata.
#' @exportClass Trial
methods::setClass(
  "Trial",
  slots = c(
    id = "character",
    arms = "character",
    arm_specs = "list",
    allocation_ratio = "integer",
    strata = "list",
    endpoints = "list",
    metadata = "list"
  ),
  validity = .valid_trial
)

#' Construct a Randomized Trial
#'
#' @param id Non-empty trial identifier.
#' @param arms Character arm labels or a list of [Arm] objects.
#' @param allocation_ratio Optional named or positional allocation weights.
#' @param strata Named list mapping prognostic factors to admissible levels.
#' @param endpoints Named endpoint definitions.
#' @param metadata Free-form trial metadata.
#' @return A [Trial] object.
#' @examples
#' Trial(
#'   "rehab-01",
#'   arms = c("active", "control"),
#'   strata = list(site = c("north", "south"))
#' )
#' @export
Trial <- function(id, arms, allocation_ratio = NULL, strata = list(),
                  endpoints = list(), metadata = list()) {
  if (is.list(arms) && !is.character(arms)) {
    if (!length(arms) || !all(vapply(arms, methods::is, logical(1), "Arm"))) {
      stop("arms must be character labels or a list of Arm objects.",
           call. = FALSE)
    }
    if (!is.null(allocation_ratio)) {
      stop("allocation_ratio must be omitted when arms are Arm objects.",
           call. = FALSE)
    }
    arm_specs <- unname(arms)
    labels <- vapply(arm_specs, methods::slot, character(1), "label")
    ratio <- stats::setNames(
      vapply(arm_specs, methods::slot, integer(1), "ratio"),
      labels
    )
  } else {
    labels <- as.character(arms)
    ratio <- .validate_ratio(
      allocation_ratio %||% rep.int(1L, length(labels)),
      labels
    )
    arm_specs <- Map(function(label, weight) Arm(label, ratio = weight),
                     labels, unname(ratio))
  }
  methods::new(
    "Trial",
    id = as.character(id),
    arms = labels,
    arm_specs = unname(arm_specs),
    allocation_ratio = stats::setNames(as.integer(ratio), labels),
    strata = strata,
    endpoints = endpoints,
    metadata = metadata
  )
}

#' Arm Labels
#'
#' @param x A [Trial] object.
#' @return Character vector of arm labels in allocation order.
#' @examples
#' arms(Trial("T1", c("active", "control")))
#' @export
methods::setGeneric("arms", function(x) standardGeneric("arms"))

#' @rdname arms
#' @export
methods::setMethod("arms", "Trial", function(x) x@arms)

#' Allocation Ratio
#'
#' @param x A [Trial] object.
#' @return Named integer vector of allocation weights.
#' @examples
#' allocationRatio(Trial("T1", c("active", "control")))
#' @export
methods::setGeneric(
  "allocationRatio",
  function(x) standardGeneric("allocationRatio")
)

#' @rdname allocationRatio
#' @export
methods::setMethod(
  "allocationRatio", "Trial",
  function(x) x@allocation_ratio
)

#' Trial Strata
#'
#' @param x A [Trial] object.
#' @return Named list of prognostic factors and admissible levels.
#' @examples
#' strata(Trial(
#'   "T1", c("active", "control"),
#'   strata = list(site = c("north", "south"))
#' ))
#' @export
methods::setGeneric("strata", function(x) standardGeneric("strata"))

#' @rdname strata
#' @export
methods::setMethod("strata", "Trial", function(x) x@strata)

.valid_randomization_sequence <- function(object) {
  if (!.is_scalar_string(object@trial_id)) {
    return("trial_id must be one non-empty string")
  }
  allowed <- c("simple", "permuted_block", "stratified_block", "minimization")
  if (length(object@method) != 1L || !object@method %in% allowed) {
    return("method is not supported")
  }
  if (length(object@arms) < 2L || anyNA(object@arms) ||
      any(!nzchar(object@arms)) || anyDuplicated(object@arms)) {
    return("arms must contain unique, non-empty labels")
  }
  if (!identical(names(object@ratio), object@arms) ||
      anyNA(object@ratio) || any(object@ratio < 1L)) {
    return("ratio must be a named positive-integer vector in arm order")
  }
  if (length(object@seed) != 1L || is.na(object@seed) ||
      object@seed < 1L) {
    return("seed must be one positive integer")
  }
  ratio_sum <- sum(as.double(object@ratio))
  if (!length(object@block_sizes) || anyNA(object@block_sizes) ||
      any(object@block_sizes < 1L) ||
      any(object@block_sizes %% ratio_sum != 0)) {
    return("block_sizes must be positive multiples of the ratio sum")
  }
  if (length(object@p_bias) != 1L || !is.finite(object@p_bias) ||
      object@p_bias < 0 || object@p_bias > 1) {
    return("p_bias must be between zero and one")
  }
  if (length(object@weights) && (!is.numeric(object@weights) ||
      any(!is.finite(object@weights)) || any(object@weights <= 0))) {
    return("weights must be finite and positive")
  }
  if (length(object@weights) &&
      !identical(names(object@weights), names(object@strata))) {
    return("weights must be named in strata order")
  }
  if (length(object@imbalance) != 1L ||
      !object@imbalance %in% c("range", "variance", "sd")) {
    return("imbalance is not supported")
  }
  table_columns <- c("order", "participant_id", "stratum", "block_id", "arm")
  if (!identical(names(object@table), table_columns) ||
      !is.integer(object@table$order) ||
      !is.character(object@table$participant_id) ||
      !is.character(object@table$stratum) ||
      !is.integer(object@table$block_id) ||
      !is.character(object@table$arm) ||
      anyNA(object@table$participant_id) ||
      anyDuplicated(object@table$participant_id) ||
      !identical(object@table$order, seq_len(nrow(object@table))) ||
      anyNA(object@table$arm) ||
      any(!object@table$arm %in% object@arms)) {
    return("table does not satisfy the sealed assignment schema")
  }
  audit_columns <- c("event", "time", "seed", "n_revealed", "agent")
  if (!identical(names(object@audit), audit_columns) ||
      !is.character(object@audit$event) ||
      !inherits(object@audit$time, "POSIXct") ||
      !is.integer(object@audit$seed) ||
      !is.integer(object@audit$n_revealed) ||
      !is.character(object@audit$agent)) {
    return("audit does not satisfy the audit schema")
  }
  if (length(object@revealed) != 1L || is.na(object@revealed) ||
      object@revealed < 0L || object@revealed > nrow(object@table)) {
    return("revealed must point within the assignment table")
  }
  if (!.is_scalar_string(object@fingerprint)) {
    return("fingerprint must be one non-empty string")
  }
  TRUE
}

#' Concealed Randomization Sequence
#'
#' The full allocation table is stored internally. Public accessors mask all
#' rows after the reveal pointer.
#'
#' @slot trial_id Trial identifier.
#' @slot method Allocation method.
#' @slot arms Ordered arm labels.
#' @slot ratio Named allocation weights.
#' @slot seed Captured random seed.
#' @slot strata Trial stratification definition.
#' @slot block_sizes Admissible block lengths.
#' @slot p_bias Minimization biased-coin probability.
#' @slot weights Named minimization factor weights.
#' @slot imbalance Minimization discrepancy measure.
#' @slot table Sealed allocation table.
#' @slot audit Append-only access audit.
#' @slot fingerprint Deterministic semantic-content hash.
#' @slot revealed Number of revealed rows.
#' @exportClass RandomizationSequence
methods::setClass(
  "RandomizationSequence",
  slots = c(
    trial_id = "character",
    method = "character",
    arms = "character",
    ratio = "integer",
    seed = "integer",
    strata = "list",
    block_sizes = "integer",
    p_bias = "numeric",
    weights = "numeric",
    imbalance = "character",
    table = "data.frame",
    audit = "data.frame",
    fingerprint = "character",
    revealed = "integer"
  ),
  validity = .valid_randomization_sequence
)

.valid_blinding_manager <- function(object) {
  if (!.is_scalar_string(object@trial_id)) {
    return("trial_id must be one non-empty string")
  }
  if (length(object@arms) < 2L || anyNA(object@arms) ||
      any(!nzchar(object@arms)) || anyDuplicated(object@arms)) {
    return("arms must contain unique, non-empty labels")
  }
  if (!identical(names(object@code_map), object@arms) ||
      length(object@code_map) != length(object@arms) ||
      anyNA(object@code_map) || any(!nzchar(object@code_map)) ||
      anyDuplicated(object@code_map)) {
    return("code_map must contain one unique, non-empty code per arm")
  }
  kit_columns <- c("participant_id", "kit_code", "arm")
  if (!identical(names(object@kit_codes), kit_columns) ||
      !all(vapply(object@kit_codes, is.character, logical(1))) ||
      anyNA(object@kit_codes) ||
      anyDuplicated(object@kit_codes$participant_id) ||
      any(!object@kit_codes$arm %in% object@arms)) {
    return("kit_codes does not satisfy the concealed kit schema")
  }
  log_columns <- c("participant_id", "arm", "requester", "reason", "time")
  if (!identical(names(object@unblinding_log), log_columns) ||
      !all(vapply(object@unblinding_log[setdiff(log_columns, "time")],
                  is.character, logical(1))) ||
      !inherits(object@unblinding_log$time, "POSIXct")) {
    return("unblinding_log does not satisfy the audit schema")
  }
  if (!.is_scalar_string(object@fingerprint)) {
    return("fingerprint must be one non-empty string")
  }
  TRUE
}

#' Blinding and Treatment-Code Manager
#'
#' @slot trial_id Trial identifier.
#' @slot arms Ordered arm labels.
#' @slot code_map Named arm-to-code mapping.
#' @slot kit_codes Concealed participant-level kit assignments.
#' @slot unblinding_log Append-only unblinding log.
#' @slot fingerprint Deterministic semantic-content hash.
#' @exportClass BlindingManager
methods::setClass(
  "BlindingManager",
  slots = c(
    trial_id = "character",
    arms = "character",
    code_map = "character",
    kit_codes = "data.frame",
    unblinding_log = "data.frame",
    fingerprint = "character"
  ),
  validity = .valid_blinding_manager
)
