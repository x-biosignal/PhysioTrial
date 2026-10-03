.analysis_set_required <- c(
  "trial_id", "set", "participants", "members", "excluded", "source_ids"
)

.analysis_column <- function(data, value, argument) {
  if (!.is_scalar_string(value)) {
    stop(argument, " must name one column.", call. = FALSE)
  }
  if (!value %in% names(data)) {
    stop(argument, " column '", value, "' is absent.", call. = FALSE)
  }
  value
}

.analysis_participant_input <- function(
  trial,
  participants,
  id_col,
  arm_col,
  randomized_col
) {
  if (!methods::is(trial, "Trial")) {
    stop("trial must be a Trial object.", call. = FALSE)
  }
  if (!is.data.frame(participants) || !nrow(participants)) {
    stop("participants must be a non-empty data.frame.", call. = FALSE)
  }
  id_col <- .analysis_column(participants, id_col, "id_col")
  arm_col <- .analysis_column(participants, arm_col, "arm_col")
  randomized_col <- .analysis_column(
    participants,
    randomized_col,
    "randomized_col"
  )
  ids <- as.character(participants[[id_col]])
  arm <- as.character(participants[[arm_col]])
  randomized <- participants[[randomized_col]]
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Participant IDs must be unique, non-empty, and non-missing.",
         call. = FALSE)
  }
  if (!is.logical(randomized) || anyNA(randomized)) {
    stop("The randomized column must be non-missing logical.",
         call. = FALSE)
  }
  if (any(is.na(arm[randomized])) ||
      any(!nzchar(arm[randomized])) ||
      any(!arm[randomized] %in% trial@arms)) {
    stop("Every randomized participant must have a known trial arm.",
         call. = FALSE)
  }
  if (any(!is.na(arm[!randomized]))) {
    stop("Non-randomized participants must have arm = NA.", call. = FALSE)
  }
  list(
    participants = participants,
    ids = ids,
    arm = arm,
    randomized = randomized
  )
}

.new_analysis_set <- function(trial, input, set, reason) {
  included <- !nzchar(reason)
  members <- data.frame(
    participant_id = input$ids[included],
    arm = input$arm[included],
    stringsAsFactors = FALSE
  )
  excluded <- data.frame(
    participant_id = input$ids[!included],
    arm = input$arm[!included],
    reason = reason[!included],
    stringsAsFactors = FALSE
  )
  structure(
    list(
      trial_id = trial@id,
      set = set,
      arm_order = trial@arms,
      participants = input$participants[included, , drop = FALSE],
      members = members,
      excluded = excluded,
      source_ids = input$ids
    ),
    class = "analysis_set"
  )
}

.validate_analysis_set <- function(x) {
  if (!inherits(x, "analysis_set") ||
      !all(.analysis_set_required %in% names(x)) ||
      !.is_scalar_string(x$trial_id) ||
      !.is_scalar_string(x$set) ||
      !is.data.frame(x$participants) ||
      !is.data.frame(x$members) ||
      !is.data.frame(x$excluded) ||
      !all(c("participant_id", "arm") %in% names(x$members)) ||
      !all(c("participant_id", "arm", "reason") %in% names(x$excluded)) ||
      !is.character(x$source_ids) ||
      anyNA(x$source_ids) ||
      any(!nzchar(x$source_ids)) ||
      anyDuplicated(x$source_ids)) {
    stop("x must be a valid analysis_set.", call. = FALSE)
  }
  included <- as.character(x$members$participant_id)
  excluded <- as.character(x$excluded$participant_id)
  if (anyNA(included) || anyNA(excluded) ||
      anyDuplicated(included) || anyDuplicated(excluded) ||
      length(intersect(included, excluded)) ||
      !setequal(c(included, excluded), x$source_ids)) {
    stop("analysis_set membership does not partition source IDs.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Intention-to-Treat Analysis Set
#'
#' Includes every randomized participant in the originally assigned arm.
#' Missing outcomes, adherence, and post-randomization events do not alter
#' membership.
#'
#' @param trial A [Trial] object.
#' @param participants One row per source participant.
#' @param id_col,arm_col,randomized_col Column names in `participants`.
#' @return An `analysis_set` with included rows, member IDs, and explicit
#'   exclusions.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' participants <- data.frame(
#'   id = c("p1", "p2", "screen"),
#'   arm = c("active", "control", NA),
#'   randomized = c(TRUE, TRUE, FALSE)
#' )
#' intentionToTreat(trial, participants)
#' @export
intentionToTreat <- function(
  trial,
  participants,
  id_col = "id",
  arm_col = "arm",
  randomized_col = "randomized"
) {
  input <- .analysis_participant_input(
    trial,
    participants,
    id_col,
    arm_col,
    randomized_col
  )
  reason <- ifelse(input$randomized, "", "not_randomized")
  .new_analysis_set(trial, input, "ITT", reason)
}

.validate_deviations <- function(
  deviations,
  source_ids,
  deviation_id_col,
  major_col,
  code_col
) {
  if (is.null(deviations)) {
    return(data.frame(
      participant_id = character(0),
      major = logical(0),
      code = character(0),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(deviations)) {
    stop("deviations must be NULL or a data.frame.", call. = FALSE)
  }
  deviation_id_col <- .analysis_column(
    deviations,
    deviation_id_col,
    "deviation_id_col"
  )
  major_col <- .analysis_column(deviations, major_col, "major_col")
  code_col <- .analysis_column(deviations, code_col, "code_col")
  ids <- as.character(deviations[[deviation_id_col]])
  major <- deviations[[major_col]]
  code <- as.character(deviations[[code_col]])
  if (anyNA(ids) || any(!nzchar(ids))) {
    stop("Deviation participant IDs must be non-empty and non-missing.",
         call. = FALSE)
  }
  unknown <- setdiff(ids, source_ids)
  if (length(unknown)) {
    stop(
      "deviations contains participant IDs absent from participants: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.logical(major) || anyNA(major)) {
    stop("The deviation major column must be non-missing logical.",
         call. = FALSE)
  }
  if (anyNA(code) || any(!nzchar(code))) {
    stop("Deviation codes must be non-empty and non-missing.", call. = FALSE)
  }
  data.frame(
    participant_id = ids,
    major = major,
    code = code,
    stringsAsFactors = FALSE
  )
}

.optional_pp_flag <- function(
  participants,
  column,
  argument,
  randomized
) {
  if (is.null(column)) {
    return(rep.int(TRUE, nrow(participants)))
  }
  column <- .analysis_column(participants, column, argument)
  value <- participants[[column]]
  if (!is.logical(value) || anyNA(value[randomized])) {
    stop(
      argument,
      " must identify a logical column non-missing for randomized participants.",
      call. = FALSE
    )
  }
  value[is.na(value)] <- TRUE
  value
}

#' Per-Protocol Analysis Set
#'
#' Starts from ITT and excludes participants with a major protocol deviation or
#' caller-selected nonadherence/incompletion flags. Multiple exclusion reasons
#' are retained in a deterministic order.
#'
#' @inheritParams intentionToTreat
#' @param deviations Optional protocol-deviation data frame.
#' @param deviation_id_col,major_col,code_col Columns in `deviations`.
#' @param adherent_col,completed_col Optional logical columns in
#'   `participants`.
#' @return A `PP` `analysis_set`.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' participants <- data.frame(
#'   id = c("p1", "p2", "screen"),
#'   arm = c("active", "control", NA),
#'   randomized = c(TRUE, TRUE, FALSE),
#'   adherent = c(TRUE, FALSE, NA)
#' )
#' perProtocol(trial, participants, adherent_col = "adherent")
#' @export
perProtocol <- function(
  trial,
  participants,
  deviations = NULL,
  id_col = "id",
  arm_col = "arm",
  randomized_col = "randomized",
  deviation_id_col = "participant_id",
  major_col = "major",
  code_col = "code",
  adherent_col = NULL,
  completed_col = NULL
) {
  input <- .analysis_participant_input(
    trial,
    participants,
    id_col,
    arm_col,
    randomized_col
  )
  deviations <- .validate_deviations(
    deviations,
    input$ids,
    deviation_id_col,
    major_col,
    code_col
  )
  adherent <- .optional_pp_flag(
    participants,
    adherent_col,
    "adherent_col",
    input$randomized
  )
  completed <- .optional_pp_flag(
    participants,
    completed_col,
    "completed_col",
    input$randomized
  )
  major <- deviations[deviations$major, , drop = FALSE]
  major_codes <- split(major$code, major$participant_id)
  reason <- vapply(seq_along(input$ids), function(i) {
    parts <- character(0)
    if (!input$randomized[[i]]) {
      parts <- c(parts, "not_randomized")
    }
    codes <- sort(unique(major_codes[[input$ids[[i]]]]))
    if (length(codes)) {
      parts <- c(parts, paste0("major_deviation:", codes))
    }
    if (input$randomized[[i]] && !adherent[[i]]) {
      parts <- c(parts, "nonadherent")
    }
    if (input$randomized[[i]] && !completed[[i]]) {
      parts <- c(parts, "incomplete")
    }
    paste(parts, collapse = ";")
  }, character(1))
  .new_analysis_set(trial, input, "PP", reason)
}

#' Extract Analysis-Set Rows and Exclusions
#'
#' @param x An `analysis_set`.
#' @return `analysisParticipants()` returns the included original rows;
#'   `analysisExclusions()` returns one explicit exclusion row per excluded
#'   source participant.
#' @name analysisSetAccessors
#' @examples
#' trial <- Trial("T1", c("A", "B"))
#' p <- data.frame(
#'   id = c("p1", "screen"),
#'   arm = c("A", NA),
#'   randomized = c(TRUE, FALSE)
#' )
#' x <- intentionToTreat(trial, p)
#' analysisParticipants(x)
#' analysisExclusions(x)
NULL

#' @rdname analysisSetAccessors
#' @export
analysisParticipants <- function(x) {
  .validate_analysis_set(x)
  x$participants
}

#' @rdname analysisSetAccessors
#' @export
analysisExclusions <- function(x) {
  .validate_analysis_set(x)
  x$excluded
}

#' @export
print.analysis_set <- function(x, ...) {
  .validate_analysis_set(x)
  cat(
    "<analysis_set> ", x$trial_id, " ", x$set, ": ",
    nrow(x$members), " included, ", nrow(x$excluded), " excluded\n",
    sep = ""
  )
  invisible(x)
}

.analysis_data_attributes <- function(x, set) {
  attr(x, "analysis_set") <- set$set
  attr(x, "included_ids") <- set$members$participant_id
  attr(x, "excluded_ids") <- set$excluded$participant_id
  x
}

.analysis_time_order <- function(x) {
  supported <- is.numeric(x) || is.character(x) || is.factor(x) ||
    inherits(x, "Date") || inherits(x, "POSIXct")
  if (!supported || anyNA(x) ||
      (is.numeric(x) && any(!is.finite(x)))) {
    stop(
      "time_col must have non-missing, totally ordered scalar values.",
      call. = FALSE
    )
  }
  order(x, method = "radix")
}

.locf_analysis_data <- function(data, id_col, time_col, value_cols) {
  if (anyDuplicated(data[c(id_col, time_col)])) {
    stop("LOCF requires unique participant/time keys.", call. = FALSE)
  }
  if (any(vapply(data[value_cols], is.list, logical(1)))) {
    stop("LOCF value columns must be atomic vectors.", call. = FALSE)
  }
  groups <- split(
    seq_len(nrow(data)),
    as.character(data[[id_col]]),
    drop = TRUE
  )
  for (indices in groups) {
    ordered <- indices[.analysis_time_order(data[[time_col]][indices])]
    for (column in value_cols) {
      last <- NA_integer_
      for (index in ordered) {
        if (!is.na(data[[column]][[index]])) {
          last <- index
        } else if (!is.na(last)) {
          data[[column]][[index]] <- data[[column]][[last]]
        }
      }
    }
  }
  data
}

.validate_imputation <- function(
  completed,
  reference,
  id_col,
  time_col,
  value_cols
) {
  if (!is.data.frame(completed) || nrow(completed) != nrow(reference)) {
    stop("Each imputed data set must preserve row count.", call. = FALSE)
  }
  required <- unique(c(id_col, time_col, value_cols))
  if (any(!required %in% names(completed))) {
    stop("Each imputed data set must preserve ID/time/value columns.",
         call. = FALSE)
  }
  if (!identical(
    as.character(completed[[id_col]]),
    as.character(reference[[id_col]])
  )) {
    stop("Each imputed data set must preserve participant IDs and order.",
         call. = FALSE)
  }
  if (!is.null(time_col) &&
      !identical(completed[[time_col]], reference[[time_col]])) {
    stop("Each imputed data set must preserve time keys and order.",
         call. = FALSE)
  }
  completed
}

#' Apply an Analysis Set and Missing-Data Method
#'
#' Filters outcome rows to analysis-set members. LOCF is included only for
#' legacy and sensitivity workflows; it is generally unsuitable for primary
#' confirmatory inference. Multiple imputation requires an explicit callback
#' and never silently substitutes a single imputation.
#'
#' @param set An `analysis_set`.
#' @param outcomes Outcome data frame.
#' @param id_col Participant-ID column in `outcomes`.
#' @param time_col Optional time column, required for LOCF.
#' @param value_cols Columns subject to missing-data processing. Defaults to
#'   every non-ID/non-time column.
#' @param missing One of `"none"`, `"locf"`, or `"multiple"`.
#' @param imputer Explicit multiple-imputation callback.
#' @param ... Arguments forwarded to `imputer`.
#' @return A filtered data frame, or a non-empty list of completed data frames
#'   for a multiple-imputation callback.
#' @examples
#' trial <- Trial("T1", c("A", "B"))
#' p <- data.frame(
#'   id = c("p1", "p2"), arm = c("A", "B"), randomized = TRUE
#' )
#' set <- intentionToTreat(trial, p)
#' outcomes <- data.frame(id = c("p1", "p1", "p2"), time = c(0, 1, 0),
#'                        value = c(NA, 2, 9))
#' analysisData(set, outcomes, time_col = "time", missing = "locf")
#' @export
analysisData <- function(
  set,
  outcomes,
  id_col = "id",
  time_col = NULL,
  value_cols = NULL,
  missing = c("none", "locf", "multiple"),
  imputer = NULL,
  ...
) {
  .validate_analysis_set(set)
  missing <- match.arg(missing)
  if (!is.data.frame(outcomes)) {
    stop("outcomes must be a data.frame.", call. = FALSE)
  }
  id_col <- .analysis_column(outcomes, id_col, "id_col")
  ids <- as.character(outcomes[[id_col]])
  if (anyNA(ids) || any(!nzchar(ids))) {
    stop("Outcome participant IDs must be non-empty and non-missing.",
         call. = FALSE)
  }
  unknown <- setdiff(ids, set$source_ids)
  if (length(unknown)) {
    stop(
      "outcomes contains participant IDs absent from the analysis source: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(time_col)) {
    time_col <- .analysis_column(outcomes, time_col, "time_col")
  }
  if (is.null(value_cols)) {
    value_cols <- setdiff(names(outcomes), c(id_col, time_col))
  }
  if (!is.character(value_cols) || !length(value_cols) ||
      anyNA(value_cols) || any(!nzchar(value_cols)) ||
      anyDuplicated(value_cols) ||
      any(!value_cols %in% names(outcomes)) ||
      any(value_cols %in% c(id_col, time_col))) {
    stop("value_cols must name existing non-ID/non-time columns.",
         call. = FALSE)
  }
  filtered <- outcomes[ids %in% set$members$participant_id, , drop = FALSE]
  rownames(filtered) <- NULL
  if (missing == "none") {
    return(.analysis_data_attributes(filtered, set))
  }
  if (missing == "locf") {
    if (is.null(time_col)) {
      stop("LOCF requires time_col.", call. = FALSE)
    }
    completed <- .locf_analysis_data(
      filtered,
      id_col,
      time_col,
      value_cols
    )
    return(.analysis_data_attributes(completed, set))
  }
  if (!is.function(imputer)) {
    stop("multiple imputation requires an imputer function.", call. = FALSE)
  }
  completed <- imputer(filtered, ...)
  if (is.data.frame(completed)) {
    completed <- .validate_imputation(
      completed,
      filtered,
      id_col,
      time_col,
      value_cols
    )
    return(.analysis_data_attributes(completed, set))
  }
  if (!is.list(completed) || !length(completed)) {
    stop("imputer must return a data.frame or a non-empty list of data.frames.",
         call. = FALSE)
  }
  completed <- lapply(completed, function(x) {
    x <- .validate_imputation(
      x,
      filtered,
      id_col,
      time_col,
      value_cols
    )
    .analysis_data_attributes(x, set)
  })
  .analysis_data_attributes(completed, set)
}
