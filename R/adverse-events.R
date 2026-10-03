.ae_causality_levels <- c(
  "not_assessed", "unrelated", "unlikely",
  "possible", "probable", "definite"
)

.ae_outcome_levels <- c(
  "unknown", "not_recovered", "recovering", "recovered",
  "recovered_with_sequelae", "fatal"
)

.ae_columns <- c(
  "participant_id",
  "term",
  "onset_date",
  "end_date",
  "ctcae_grade",
  "serious",
  "causality",
  "action",
  "outcome",
  "meddra_pt",
  "meddra_code",
  "meddra_version",
  "metadata"
)

.ae_optional_string <- function(x, name) {
  if (!.is_scalar_string(x, allow_na = TRUE)) {
    stop(name, " must be one string or NA.", call. = FALSE)
  }
  as.character(x)
}

.ae_date <- function(x, name, allow_na) {
  if (length(x) != 1L) {
    stop(name, " must have length one.", call. = FALSE)
  }
  value <- tryCatch(
    {
      if (inherits(x, "Date")) x else as.Date(x)
    },
    error = function(e) as.Date(NA)
  )
  if (length(value) != 1L ||
      (!is.na(value) && !is.finite(unclass(value))) ||
      (!allow_na && is.na(value))) {
    stop(name, " must be a valid Date", if (allow_na) " or NA." else ".",
         call. = FALSE)
  }
  value
}

.empty_adverse_events <- function() {
  data.frame(
    participant_id = character(0),
    term = character(0),
    onset_date = as.Date(character(0)),
    end_date = as.Date(character(0)),
    ctcae_grade = integer(0),
    serious = logical(0),
    causality = character(0),
    action = character(0),
    outcome = character(0),
    meddra_pt = character(0),
    meddra_code = character(0),
    meddra_version = character(0),
    metadata = I(list()),
    stringsAsFactors = FALSE
  )
}

.validate_ae_table <- function(events) {
  if (!is.data.frame(events)) {
    stop("events must be adverse_event objects or a data.frame.",
         call. = FALSE)
  }
  missing_columns <- setdiff(.ae_columns, names(events))
  if (length(missing_columns)) {
    stop(
      "events is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  events <- events[, .ae_columns, drop = FALSE]
  for (column in c(
    "participant_id", "term", "causality", "action", "outcome",
    "meddra_pt", "meddra_code", "meddra_version"
  )) {
    events[[column]] <- as.character(events[[column]])
  }
  if (anyNA(events$participant_id) || any(!nzchar(events$participant_id)) ||
      anyNA(events$term) || any(!nzchar(events$term))) {
    stop("AE participant_id and term must be non-empty and non-missing.",
         call. = FALSE)
  }
  if (!inherits(events$onset_date, "Date") ||
      !inherits(events$end_date, "Date") ||
      anyNA(events$onset_date)) {
    stop("AE onset_date/end_date must be Date, with onset non-missing.",
         call. = FALSE)
  }
  if (any(
    !is.na(events$end_date) & events$end_date < events$onset_date
  )) {
    stop("AE end_date must not precede onset_date.", call. = FALSE)
  }
  if (!is.numeric(events$ctcae_grade) ||
      any(!is.finite(events$ctcae_grade)) ||
      any(events$ctcae_grade != floor(events$ctcae_grade)) ||
      any(!events$ctcae_grade %in% 1:5)) {
    stop("ctcae_grade must contain integers from 1 through 5.",
         call. = FALSE)
  }
  events$ctcae_grade <- as.integer(events$ctcae_grade)
  if (!is.logical(events$serious) || anyNA(events$serious)) {
    stop("serious must be non-missing logical.", call. = FALSE)
  }
  if (any(!events$causality %in% .ae_causality_levels)) {
    stop("Unknown AE causality value.", call. = FALSE)
  }
  if (any(!events$outcome %in% .ae_outcome_levels)) {
    stop("Unknown AE outcome value.", call. = FALSE)
  }
  if (any(events$ctcae_grade == 5L & !events$serious)) {
    stop("CTCAE grade 5 events must be serious.", call. = FALSE)
  }
  fatal <- events$outcome == "fatal"
  if (any(fatal & (events$ctcae_grade != 5L | !events$serious))) {
    stop("Fatal events must be CTCAE grade 5 and serious.", call. = FALSE)
  }
  optional <- c(
    "action", "meddra_pt", "meddra_code", "meddra_version"
  )
  if (any(vapply(events[optional], function(x) {
    any(!is.na(x) & !nzchar(x))
  }, logical(1)))) {
    stop("Optional AE strings must be non-empty when supplied.",
         call. = FALSE)
  }
  if (!is.list(events$metadata)) {
    stop("AE metadata must be a list-column.", call. = FALSE)
  }
  rownames(events) <- NULL
  events
}

.as_ae_data <- function(events) {
  if (is.list(events) && !is.data.frame(events)) {
    if (!length(events)) {
      return(.empty_adverse_events())
    }
    if (!all(vapply(events, inherits, logical(1), "adverse_event"))) {
      stop("Every element of events must be an adverse_event.",
           call. = FALSE)
    }
    events <- do.call(rbind, unname(events))
    class(events) <- "data.frame"
  }
  .validate_ae_table(events)
}

#' Capture an Adverse Event
#'
#' Creates one typed adverse-event row. CTCAE severity and regulatory
#' seriousness are stored independently; a serious event is not inferred from
#' a grade threshold. MedDRA fields preserve externally assigned coding
#' metadata and are not checked against a bundled terminology.
#'
#' @param participant_id Non-empty participant identifier.
#' @param term Verbatim adverse-event term.
#' @param onset_date Event onset date.
#' @param end_date Optional event end date.
#' @param ctcae_grade Integer CTCAE grade from 1 through 5.
#' @param serious Whether the event meets a seriousness criterion.
#' @param causality Investigator causality assessment.
#' @param action Optional action taken.
#' @param outcome Controlled event outcome.
#' @param meddra_pt Optional MedDRA preferred term.
#' @param meddra_code Optional MedDRA code.
#' @param meddra_version Optional MedDRA version.
#' @param metadata Free-form list stored as a list-column.
#' @return A one-row `adverse_event` data frame.
#' @references National Cancer Institute. CTCAE version 5.0.
#' @examples
#' adverseEvent(
#'   "P001", "Nausea", as.Date("2026-01-02"),
#'   ctcae_grade = 2, causality = "possible"
#' )
#' @export
adverseEvent <- function(
  participant_id,
  term,
  onset_date,
  end_date = as.Date(NA),
  ctcae_grade,
  serious = FALSE,
  causality = c(
    "not_assessed", "unrelated", "unlikely",
    "possible", "probable", "definite"
  ),
  action = NA_character_,
  outcome = c(
    "unknown", "not_recovered", "recovering", "recovered",
    "recovered_with_sequelae", "fatal"
  ),
  meddra_pt = NA_character_,
  meddra_code = NA_character_,
  meddra_version = NA_character_,
  metadata = list()
) {
  if (!.is_scalar_string(participant_id) || !.is_scalar_string(term)) {
    stop("participant_id and term must be non-empty strings.",
         call. = FALSE)
  }
  if (!.whole_number(ctcae_grade) || length(ctcae_grade) != 1L ||
      !ctcae_grade %in% 1:5) {
    stop("ctcae_grade must be one integer from 1 through 5.",
         call. = FALSE)
  }
  if (!is.logical(serious) || length(serious) != 1L || is.na(serious)) {
    stop("serious must be one non-missing logical.", call. = FALSE)
  }
  if (!is.list(metadata)) {
    stop("metadata must be a list.", call. = FALSE)
  }
  causality <- match.arg(causality)
  outcome <- match.arg(outcome)
  onset_date <- .ae_date(onset_date, "onset_date", allow_na = FALSE)
  end_date <- .ae_date(end_date, "end_date", allow_na = TRUE)
  event <- data.frame(
    participant_id = as.character(participant_id),
    term = as.character(term),
    onset_date = onset_date,
    end_date = end_date,
    ctcae_grade = as.integer(ctcae_grade),
    serious = serious,
    causality = causality,
    action = .ae_optional_string(action, "action"),
    outcome = outcome,
    meddra_pt = .ae_optional_string(meddra_pt, "meddra_pt"),
    meddra_code = .ae_optional_string(meddra_code, "meddra_code"),
    meddra_version = .ae_optional_string(
      meddra_version,
      "meddra_version"
    ),
    metadata = I(list(metadata)),
    stringsAsFactors = FALSE
  )
  event <- .validate_ae_table(event)
  class(event) <- c("adverse_event", "data.frame")
  event
}

.validate_ae_allocation <- function(allocation, arms) {
  if (!is.data.frame(allocation)) {
    stop("allocation must be a data.frame.", call. = FALSE)
  }
  missing_columns <- setdiff(
    c("participant_id", "arm"),
    names(allocation)
  )
  if (length(missing_columns)) {
    stop("allocation must contain participant_id and arm.", call. = FALSE)
  }
  allocation <- allocation[, c("participant_id", "arm"), drop = FALSE]
  allocation$participant_id <- as.character(allocation$participant_id)
  allocation$arm <- as.character(allocation$arm)
  if (!nrow(allocation) || anyNA(allocation$participant_id) ||
      any(!nzchar(allocation$participant_id)) ||
      anyDuplicated(allocation$participant_id) ||
      anyNA(allocation$arm) || any(!nzchar(allocation$arm))) {
    stop("allocation must have unique IDs and non-missing arm labels.",
         call. = FALSE)
  }
  if (is.null(arms)) {
    arms <- unique(allocation$arm)
  } else {
    if (!is.character(arms) || !length(arms) || anyNA(arms) ||
        any(!nzchar(arms)) || anyDuplicated(arms)) {
      stop("arms must contain unique, non-empty labels.", call. = FALSE)
    }
    if (any(!allocation$arm %in% arms)) {
      stop("allocation contains an arm not listed in arms.", call. = FALSE)
    }
  }
  list(allocation = allocation, arms = arms)
}

.ae_by_arm <- function(events, allocation, arms) {
  rows <- lapply(arms, function(arm) {
    allocated_ids <- allocation$participant_id[allocation$arm == arm]
    selected <- events$arm == arm
    arm_events <- events[selected, , drop = FALSE]
    participants_ae <- unique(arm_events$participant_id)
    serious_events <- arm_events[arm_events$serious, , drop = FALSE]
    participants_sae <- unique(serious_events$participant_id)
    denominator <- length(allocated_ids)
    data.frame(
      arm = arm,
      n_randomized = as.integer(denominator),
      n_ae = as.integer(nrow(arm_events)),
      n_participants_ae = as.integer(length(participants_ae)),
      n_sae = as.integer(nrow(serious_events)),
      n_participants_sae = as.integer(length(participants_sae)),
      ae_risk = if (denominator) length(participants_ae) / denominator
        else NA_real_,
      sae_risk = if (denominator) length(participants_sae) / denominator
        else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.ae_by_severity <- function(events, arms) {
  rows <- lapply(arms, function(arm) {
    lapply(1:5, function(grade) {
      selected <- events$arm == arm & events$ctcae_grade == grade
      data.frame(
        arm = arm,
        ctcae_grade = as.integer(grade),
        n_ae = as.integer(sum(selected)),
        n_participants = as.integer(length(unique(
          events$participant_id[selected]
        ))),
        stringsAsFactors = FALSE
      )
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

.same_or_na <- function(x, value) {
  (is.na(x) & is.na(value)) | (!is.na(x) & !is.na(value) & x == value)
}

.ae_by_term <- function(events, arms) {
  if (!nrow(events)) {
    return(data.frame(
      arm = character(0),
      meddra_code = character(0),
      meddra_pt = character(0),
      term = character(0),
      n_ae = integer(0),
      n_participants = integer(0),
      n_sae = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  rows <- list()
  for (arm in arms) {
    arm_events <- events[events$arm == arm, , drop = FALSE]
    terms <- unique(
      arm_events[c("meddra_code", "meddra_pt", "term")]
    )
    terms <- terms[order(
      is.na(terms$meddra_code),
      terms$meddra_code,
      is.na(terms$meddra_pt),
      terms$meddra_pt,
      terms$term,
      na.last = TRUE
    ), , drop = FALSE]
    for (i in seq_len(nrow(terms))) {
      selected <- .same_or_na(
        arm_events$meddra_code,
        terms$meddra_code[[i]]
      ) & .same_or_na(
        arm_events$meddra_pt,
        terms$meddra_pt[[i]]
      ) & arm_events$term == terms$term[[i]]
      rows[[length(rows) + 1L]] <- data.frame(
        arm = arm,
        meddra_code = terms$meddra_code[[i]],
        meddra_pt = terms$meddra_pt[[i]],
        term = terms$term[[i]],
        n_ae = as.integer(sum(selected)),
        n_participants = as.integer(length(unique(
          arm_events$participant_id[selected]
        ))),
        n_sae = as.integer(sum(arm_events$serious[selected])),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(.ae_by_term(.empty_adverse_events(), character(0)))
  }
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}

#' Summarize Adverse Events by Trial Arm
#'
#' Reports event counts separately from participant incidence, and counts SAE
#' status from the explicit `serious` field rather than a CTCAE grade cutoff.
#'
#' @param events List of [adverseEvent()] rows or a data frame using the same
#'   schema.
#' @param allocation Data frame with unique `participant_id` and `arm`.
#' @param arms Optional complete arm order, including zero-event arms.
#' @return An `ae_summary` with arm, severity, and term tables.
#' @examples
#' allocation <- data.frame(
#'   participant_id = c("p1", "p2"),
#'   arm = c("active", "control")
#' )
#' events <- list(adverseEvent(
#'   "p1", "Nausea", as.Date("2026-01-02"),
#'   ctcae_grade = 1
#' ))
#' aeSummary(events, allocation, arms = c("active", "control"))
#' @export
aeSummary <- function(events, allocation, arms = NULL) {
  events <- .as_ae_data(events)
  validated <- .validate_ae_allocation(allocation, arms)
  allocation <- validated$allocation
  arms <- validated$arms
  unknown <- setdiff(events$participant_id, allocation$participant_id)
  if (length(unknown)) {
    stop(
      "events contains participant IDs absent from allocation: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  events$arm <- allocation$arm[
    match(events$participant_id, allocation$participant_id)
  ]
  structure(
    list(
      by_arm = .ae_by_arm(events, allocation, arms),
      by_severity = .ae_by_severity(events, arms),
      by_term = .ae_by_term(events, arms)
    ),
    class = "ae_summary"
  )
}

#' @export
print.ae_summary <- function(x, ...) {
  cat("<ae_summary>\n")
  print(x$by_arm, row.names = FALSE, ...)
  invisible(x)
}
