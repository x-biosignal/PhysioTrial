.empty_kit_codes <- function() {
  data.frame(
    participant_id = character(0),
    kit_code = character(0),
    arm = character(0),
    stringsAsFactors = FALSE
  )
}

.empty_unblinding_log <- function() {
  data.frame(
    participant_id = character(0),
    arm = character(0),
    requester = character(0),
    reason = character(0),
    time = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )
}

.validate_blinding_codes <- function(codes, arm_labels) {
  if (!is.character(codes) || length(codes) != length(arm_labels) ||
      is.null(names(codes)) || anyNA(names(codes)) ||
      any(!nzchar(names(codes))) || !setequal(names(codes), arm_labels)) {
    stop("codes must be a named character vector with one code per arm.",
         call. = FALSE)
  }
  codes <- codes[arm_labels]
  if (anyNA(codes) || any(!nzchar(codes)) || anyDuplicated(codes)) {
    stop("Treatment codes must be unique, non-empty, and non-missing.",
         call. = FALSE)
  }
  codes
}

.manager_fingerprint <- function(manager) {
  log_columns <- setdiff(names(manager@unblinding_log), "time")
  semantic <- list(
    trial_id = manager@trial_id,
    arms = manager@arms,
    code_map = manager@code_map,
    kit_codes = as.list(manager@kit_codes),
    unblinding_log = as.list(manager@unblinding_log[log_columns])
  )
  digest::digest(as.list(semantic), algo = "xxhash64")
}

#' Construct a Blinding Manager
#'
#' @param trial A [Trial] object.
#' @param sequence Optional [RandomizationSequence-class] object from the same
#'   trial.
#' @param codes Optional named character vector mapping arms to masked treatment
#'   codes.
#' @param seed Optional positive integer used to scramble generated codes.
#' @return A [BlindingManager-class] object.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' blindingManager(trial, seed = 1)
#' @export
blindingManager <- function(trial, sequence = NULL, codes = NULL, seed = NULL) {
  if (!methods::is(trial, "Trial")) {
    stop("trial must be a Trial object.", call. = FALSE)
  }
  if (is.null(codes)) {
    seed <- .resolve_seed(seed)
    set.seed(seed)
    generated <- sprintf("Kit-%03d", seq_along(trial@arms))
    codes <- stats::setNames(sample(generated), trial@arms)
  } else {
    codes <- .validate_blinding_codes(codes, trial@arms)
  }
  codes <- .validate_blinding_codes(codes, trial@arms)

  kit_codes <- .empty_kit_codes()
  if (!is.null(sequence)) {
    if (!methods::is(sequence, "RandomizationSequence")) {
      stop("sequence must be a RandomizationSequence.", call. = FALSE)
    }
    if (!identical(sequence@trial_id, trial@id) ||
        !identical(sequence@arms, trial@arms)) {
      stop("sequence does not belong to this trial.", call. = FALSE)
    }
    sealed <- .sealed_assignments(sequence)
    kit_codes <- data.frame(
      participant_id = sealed$participant_id,
      kit_code = unname(codes[sealed$arm]),
      arm = sealed$arm,
      stringsAsFactors = FALSE
    )
  }

  manager <- methods::new(
    "BlindingManager",
    trial_id = trial@id,
    arms = trial@arms,
    code_map = codes,
    kit_codes = kit_codes,
    unblinding_log = .empty_unblinding_log(),
    fingerprint = "<pending>"
  )
  manager@fingerprint <- .manager_fingerprint(manager)
  methods::validObject(manager)
  manager
}

#' Record an Unblinding Event
#'
#' @param manager A [BlindingManager-class] object.
#' @param participant_id Participant to unblind.
#' @param requester Optional identity requesting unblinding.
#' @param reason Optional reason for unblinding.
#' @return The modified [BlindingManager-class] object.
#' @examples
#' trial <- Trial("T1", c("A", "B"))
#' sequence <- randomize(trial, n = 4, seed = 1)
#' manager <- blindingManager(trial, sequence, seed = 2)
#' unblind(manager, "slot_1", requester = "PI", reason = "safety")
#' @export
unblind <- function(manager, participant_id, requester = NA_character_,
                    reason = NA_character_) {
  if (!methods::is(manager, "BlindingManager")) {
    stop("manager must be a BlindingManager.", call. = FALSE)
  }
  if (!.is_scalar_string(participant_id) ||
      !.is_scalar_string(requester, allow_na = TRUE) ||
      !.is_scalar_string(reason, allow_na = TRUE)) {
    stop("participant_id must be non-empty; requester and reason must be ",
         "one string or NA.", call. = FALSE)
  }
  index <- match(participant_id, manager@kit_codes$participant_id)
  if (is.na(index)) {
    stop("participant_id is not present in the concealed kit assignments.",
         call. = FALSE)
  }
  event <- data.frame(
    participant_id = participant_id,
    arm = manager@kit_codes$arm[[index]],
    requester = requester,
    reason = reason,
    time = as.POSIXct(Sys.time()),
    stringsAsFactors = FALSE
  )
  manager@unblinding_log <- rbind(manager@unblinding_log, event)
  manager@fingerprint <- .manager_fingerprint(manager)
  methods::validObject(manager)
  manager
}

#' Unblinding Log
#'
#' @param manager A [BlindingManager-class] object.
#' @return Append-only unblinding data frame.
#' @examples
#' manager <- blindingManager(Trial("T1", c("A", "B")), seed = 1)
#' unblindingLog(manager)
#' @export
unblindingLog <- function(manager) {
  if (!methods::is(manager, "BlindingManager")) {
    stop("manager must be a BlindingManager.", call. = FALSE)
  }
  manager@unblinding_log
}

#' Masked Treatment Codes
#'
#' @param manager A [BlindingManager-class] object.
#' @return Named character vector mapping arms to masked codes.
#' @examples
#' manager <- blindingManager(Trial("T1", c("A", "B")), seed = 1)
#' blindingCodes(manager)
#' @export
blindingCodes <- function(manager) {
  if (!methods::is(manager, "BlindingManager")) {
    stop("manager must be a BlindingManager.", call. = FALSE)
  }
  manager@code_map
}

#' @rdname trialFingerprint
#' @export
methods::setMethod(
  "trialFingerprint", "BlindingManager",
  function(x) x@fingerprint
)

methods::setMethod("show", "BlindingManager", function(object) {
  cat(
    "BlindingManager:", object@trial_id, "\n",
    "  arms:", length(object@arms), "\n",
    "  coded participants:", nrow(object@kit_codes), "\n",
    "  unblinding events:", nrow(object@unblinding_log), "\n",
    sep = ""
  )
})

#' Bang Blinding Index
#'
#' Computes Bang's arm-specific blinding index and normal-approximation
#' confidence interval. A guess matching the actual arm is correct;
#' `dont_know` is neutral; any other value, including an unknown arm label or
#' missing guess, is incorrect.
#'
#' @param guesses Data frame containing actual assignments and guesses.
#' @param arm_col Name of the actual-assignment column.
#' @param guess_col Name of the guess column.
#' @param dont_know Sentinel used for a "don't know" response.
#' @param conf_level Confidence level strictly between zero and one.
#' @return A `blinding_index` object.
#' @references Bang H, Ni L, Davis CE (2004). Assessment of blinding in
#'   clinical trials. *Controlled Clinical Trials*, 25, 143-156.
#' @examples
#' guesses <- data.frame(
#'   arm = c("active", "active", "control", "control"),
#'   guess = c("active", "dont_know", "active", "control")
#' )
#' blindingIndex(guesses)
#' @export
blindingIndex <- function(guesses, arm_col = "arm", guess_col = "guess",
                          dont_know = "dont_know", conf_level = 0.95) {
  if (!is.data.frame(guesses)) {
    stop("guesses must be a data.frame.", call. = FALSE)
  }
  for (column in list(arm_col = arm_col, guess_col = guess_col)) {
    if (!.is_scalar_string(column)) {
      stop("arm_col and guess_col must be non-empty column names.",
           call. = FALSE)
    }
  }
  missing_columns <- setdiff(c(arm_col, guess_col), names(guesses))
  if (length(missing_columns)) {
    stop("guesses is missing columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (!.is_scalar_string(dont_know)) {
    stop("dont_know must be one non-empty string.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be strictly between zero and one.",
         call. = FALSE)
  }
  actual <- as.character(guesses[[arm_col]])
  guess <- as.character(guesses[[guess_col]])
  if (!length(actual) || anyNA(actual) || any(!nzchar(actual))) {
    stop("Actual arm labels must be non-empty and non-missing.",
         call. = FALSE)
  }
  arm_labels <- unique(actual)
  if (dont_know %in% arm_labels) {
    stop("dont_know must not equal an actual arm label.", call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  rows <- lapply(arm_labels, function(arm) {
    index <- actual == arm
    arm_guess <- guess[index]
    n <- length(arm_guess)
    n_correct <- sum(!is.na(arm_guess) & arm_guess == arm)
    n_dontknow <- sum(!is.na(arm_guess) & arm_guess == dont_know)
    n_incorrect <- n - n_correct - n_dontknow
    estimate <- (n_correct - n_incorrect) / n
    variance <- ((n_correct + n_incorrect) / n - estimate^2) / n
    standard_error <- sqrt(max(variance, 0))
    data.frame(
      arm = arm,
      n = as.integer(n),
      n_correct = as.integer(n_correct),
      n_incorrect = as.integer(n_incorrect),
      n_dontknow = as.integer(n_dontknow),
      BI = as.numeric(estimate),
      SE = as.numeric(standard_error),
      ci_lower = as.numeric(estimate - z * standard_error),
      ci_upper = as.numeric(estimate + z * standard_error),
      stringsAsFactors = FALSE
    )
  })
  structure(
    list(
      table = do.call(rbind, rows),
      conf_level = as.numeric(conf_level)
    ),
    class = "blinding_index"
  )
}

#' @export
print.blinding_index <- function(x, ...) {
  cat(
    "Bang blinding index (",
    formatC(100 * x$conf_level, format = "fg"),
    "% confidence interval)\n",
    sep = ""
  )
  print(x$table, row.names = FALSE, ...)
  invisible(x)
}
