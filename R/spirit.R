.spirit_categories <- c(
  "enrolment", "intervention", "assessment", "other"
)

.spirit_required_columns <- c(
  "timepoint_id",
  "timepoint_label",
  "timepoint_order",
  "category",
  "activity",
  "arm",
  "scheduled"
)

.validate_spirit_events <- function(trial, events, marker) {
  if (!methods::is(trial, "Trial")) {
    stop("trial must be a Trial object.", call. = FALSE)
  }
  if (!is.data.frame(events) || !nrow(events)) {
    stop("events must be a non-empty data.frame.", call. = FALSE)
  }
  missing_columns <- setdiff(.spirit_required_columns, names(events))
  if (length(missing_columns)) {
    stop(
      "events is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  keep <- c(
    .spirit_required_columns,
    if ("marker" %in% names(events)) "marker"
  )
  events <- events[, keep, drop = FALSE]
  for (column in c(
    "timepoint_id", "timepoint_label", "category", "activity", "arm"
  )) {
    events[[column]] <- as.character(events[[column]])
  }
  for (column in c("timepoint_id", "timepoint_label", "activity")) {
    if (anyNA(events[[column]]) || any(!nzchar(events[[column]]))) {
      stop(column, " must be non-empty and non-missing.", call. = FALSE)
    }
  }
  if (!is.numeric(events$timepoint_order) ||
      any(!is.finite(events$timepoint_order)) ||
      any(events$timepoint_order != floor(events$timepoint_order)) ||
      any(abs(events$timepoint_order) > .Machine$integer.max)) {
    stop("timepoint_order must contain finite integers.", call. = FALSE)
  }
  events$timepoint_order <- as.integer(events$timepoint_order)
  if (anyNA(events$category) ||
      any(!events$category %in% .spirit_categories)) {
    stop(
      "category must be one of: ",
      paste(.spirit_categories, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.logical(events$scheduled) || anyNA(events$scheduled)) {
    stop("scheduled must be non-missing logical.", call. = FALSE)
  }
  if (any(!is.na(events$arm) & !events$arm %in% trial@arms)) {
    stop("events contains an unknown trial arm.", call. = FALSE)
  }
  events$arm[is.na(events$arm)] <- "all"

  if ("marker" %in% names(events)) {
    events$marker <- as.character(events$marker)
  } else {
    events$marker <- rep.int(marker, nrow(events))
  }
  if (any(events$scheduled &
      (is.na(events$marker) | !nzchar(events$marker)))) {
    stop("Every scheduled event must have a non-empty marker.",
         call. = FALSE)
  }
  events$marker[!events$scheduled] <- ""

  timepoint_split <- split(events, events$timepoint_id)
  consistent <- vapply(timepoint_split, function(rows) {
    length(unique(rows$timepoint_label)) == 1L &&
      length(unique(rows$timepoint_order)) == 1L
  }, logical(1))
  if (!all(consistent)) {
    stop("Each timepoint_id must have exactly one label and order.",
         call. = FALSE)
  }
  timepoints <- do.call(rbind, lapply(timepoint_split, function(rows) {
    rows[1L, c("timepoint_id", "timepoint_label", "timepoint_order")]
  }))
  if (anyDuplicated(timepoints$timepoint_order)) {
    stop("timepoint_order must be unique across timepoint IDs.",
         call. = FALSE)
  }

  cell_columns <- c("category", "activity", "arm", "timepoint_id")
  if (anyDuplicated(events[cell_columns])) {
    stop(
      "events contains a duplicate category/activity/arm/timepoint cell.",
      call. = FALSE
    )
  }
  rownames(events) <- NULL
  events
}

.spirit_timepoints <- function(events) {
  timepoints <- unique(
    events[c("timepoint_id", "timepoint_label", "timepoint_order")]
  )
  timepoints <- timepoints[
    order(timepoints$timepoint_order, timepoints$timepoint_id),
    ,
    drop = FALSE
  ]
  rownames(timepoints) <- NULL
  timepoints
}

.spirit_arm_order <- function(arm, trial_arms) {
  match(arm, c("all", trial_arms))
}

.spirit_wide_table <- function(events, timepoints, trial_arms) {
  rows <- unique(events[c("category", "activity", "arm")])
  rows$category_order <- match(rows$category, .spirit_categories)
  rows$arm_order <- .spirit_arm_order(rows$arm, trial_arms)
  rows <- rows[
    order(rows$category_order, rows$activity, rows$arm_order),
    c("category", "activity", "arm"),
    drop = FALSE
  ]
  for (timepoint in timepoints$timepoint_id) {
    rows[[timepoint]] <- vapply(seq_len(nrow(rows)), function(i) {
      selected <- events$category == rows$category[[i]] &
        events$activity == rows$activity[[i]] &
        events$arm == rows$arm[[i]] &
        events$timepoint_id == timepoint
      if (!any(selected)) "" else events$marker[selected][[1L]]
    }, character(1))
  }
  rownames(rows) <- NULL
  rows
}

#' SPIRIT Schedule of Events
#'
#' Converts a validated long schedule into a deterministic wide
#' enrolment/intervention/assessment table.
#'
#' @param trial A [Trial] object.
#' @param events Long schedule data frame using the documented event schema.
#' @param marker Default marker for scheduled cells.
#' @return A `spirit_schedule` object with validated long events, a wide table,
#'   and ordered timepoint metadata.
#' @references Chan AW et al. (2013). SPIRIT 2013 Statement.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' events <- data.frame(
#'   timepoint_id = c("screen", "week12"),
#'   timepoint_label = c("Screening", "Week 12"),
#'   timepoint_order = c(-1L, 1L),
#'   category = c("enrolment", "assessment"),
#'   activity = c("Eligibility", "Primary endpoint"),
#'   arm = NA_character_,
#'   scheduled = TRUE
#' )
#' spiritSchedule(trial, events)
#' @export
spiritSchedule <- function(trial, events, marker = "X") {
  if (!.is_scalar_string(marker)) {
    stop("marker must be one non-empty string.", call. = FALSE)
  }
  events <- .validate_spirit_events(trial, events, marker)
  timepoints <- .spirit_timepoints(events)
  table <- .spirit_wide_table(events, timepoints, trial@arms)
  structure(
    list(
      trial_id = trial@id,
      events = events,
      table = table,
      timepoints = timepoints
    ),
    class = "spirit_schedule"
  )
}

#' @export
as.data.frame.spirit_schedule <- function(x, ...) {
  x$table
}

#' @export
print.spirit_schedule <- function(x, ...) {
  cat(
    "<spirit_schedule> ", x$trial_id, ": ",
    nrow(x$table), " activities x ",
    nrow(x$timepoints), " timepoints\n",
    sep = ""
  )
  print(x$table, row.names = FALSE, ...)
  invisible(x)
}

.spirit_template <- function(checklist) {
  if (is.null(checklist)) {
    path <- system.file(
      "extdata",
      "spirit-2013-checklist.csv",
      package = "PhysioTrial"
    )
    if (!nzchar(path)) {
      stop("The bundled SPIRIT checklist could not be located.",
           call. = FALSE)
    }
    checklist <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  if (!is.data.frame(checklist)) {
    stop("checklist must be a data.frame.", call. = FALSE)
  }
  required <- c("item_id", "top_level_item", "section", "short_label")
  missing_columns <- setdiff(required, names(checklist))
  if (length(missing_columns)) {
    stop(
      "checklist is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  checklist <- checklist[, required, drop = FALSE]
  checklist$item_id <- as.character(checklist$item_id)
  checklist$section <- as.character(checklist$section)
  checklist$short_label <- as.character(checklist$short_label)
  if (anyNA(checklist$item_id) || any(!nzchar(checklist$item_id)) ||
      anyDuplicated(checklist$item_id) ||
      anyNA(checklist$section) || any(!nzchar(checklist$section)) ||
      anyNA(checklist$short_label) || any(!nzchar(checklist$short_label))) {
    stop("checklist text fields must be unique/non-empty as appropriate.",
         call. = FALSE)
  }
  top_level_value <- checklist$top_level_item
  if (is.factor(top_level_value)) {
    top_level_value <- as.character(top_level_value)
  }
  top_level_numeric <- suppressWarnings(as.numeric(top_level_value))
  if (anyNA(top_level_numeric) ||
      any(!is.finite(top_level_numeric)) ||
      any(top_level_numeric != floor(top_level_numeric)) ||
      any(abs(top_level_numeric) > .Machine$integer.max)) {
    stop("top_level_item must contain finite integers.", call. = FALSE)
  }
  top_level <- as.integer(top_level_numeric)
  if (!setequal(unique(top_level), seq_len(33L))) {
    stop("checklist must cover all 33 SPIRIT 2013 top-level items.",
         call. = FALSE)
  }
  checklist$top_level_item <- top_level
  checklist
}

.spirit_evidence <- function(value) {
  if (is.null(value) || !length(value) || all(is.na(value))) {
    return("")
  }
  if (is.logical(value)) {
    return(if (any(value, na.rm = TRUE)) "reported" else "")
  }
  value <- as.character(value)
  value <- value[!is.na(value) & nzchar(value)]
  paste(value, collapse = "; ")
}

.protocol_evidence <- function(protocol) {
  if (is.list(protocol) && !is.data.frame(protocol)) {
    item_id <- names(protocol)
    if (is.null(item_id) || anyNA(item_id) || any(!nzchar(item_id)) ||
        anyDuplicated(item_id)) {
      stop("A list protocol must have unique, non-empty item_id names.",
           call. = FALSE)
    }
    evidence <- vapply(protocol, .spirit_evidence, character(1))
  } else if (is.data.frame(protocol)) {
    missing_columns <- setdiff(c("item_id", "evidence"), names(protocol))
    if (length(missing_columns)) {
      stop("A protocol data.frame must contain item_id and evidence.",
           call. = FALSE)
    }
    item_id <- as.character(protocol$item_id)
    if (anyNA(item_id) || any(!nzchar(item_id)) || anyDuplicated(item_id)) {
      stop("protocol item_id values must be unique and non-empty.",
           call. = FALSE)
    }
    evidence <- vapply(
      as.list(protocol$evidence),
      .spirit_evidence,
      character(1)
    )
  } else {
    stop("protocol must be a named list or a data.frame.", call. = FALSE)
  }
  data.frame(
    item_id = item_id,
    evidence = evidence,
    stringsAsFactors = FALSE
  )
}

#' SPIRIT 2013 Protocol Checklist
#'
#' Marks protocol evidence against the bundled SPIRIT 2013 checklist. This is a
#' completeness aid and does not certify protocol compliance.
#'
#' @param protocol Named list (`item_id` to evidence) or data frame with
#'   `item_id` and `evidence`.
#' @param checklist Optional replacement checklist using the bundled schema.
#' @return A `spirit_checklist` data frame with completion flags and evidence.
#' @references Chan AW et al. (2013). SPIRIT 2013 Statement.
#' @examples
#' spiritChecklist(list(`1` = "Descriptive title", `2` = "Registry ID"))
#' @export
spiritChecklist <- function(protocol, checklist = NULL) {
  template <- .spirit_template(checklist)
  evidence <- .protocol_evidence(protocol)
  unknown <- setdiff(evidence$item_id, template$item_id)
  if (length(unknown)) {
    stop(
      "Unknown SPIRIT checklist item(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  matched <- match(template$item_id, evidence$item_id)
  template$evidence <- ifelse(
    is.na(matched),
    "",
    evidence$evidence[matched]
  )
  template$complete <- nzchar(template$evidence)
  class(template) <- c("spirit_checklist", "data.frame")
  template
}

#' @export
print.spirit_checklist <- function(x, ...) {
  cat(
    "<spirit_checklist> ",
    sum(x$complete), "/", nrow(x),
    " entries have evidence; completeness aid only\n",
    sep = ""
  )
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}
