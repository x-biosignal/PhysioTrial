.cdisc_dm_columns <- c(
  "STUDYID", "DOMAIN", "USUBJID", "SUBJID", "SITEID", "RFSTDTC",
  "RFENDTC", "BRTHDTC", "SEX", "COUNTRY", "ARMCD", "ARM"
)

.cdisc_ae_columns <- c(
  "STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD",
  "AEBODSYS", "AESEV", "AESER", "AEREL", "AEACN", "AEOUT",
  "AESTDTC", "AEENDTC"
)

.cdisc_adsl_columns <- c(
  "STUDYID", "USUBJID", "SUBJID", "TRT01P", "TRT01PN",
  "ITTFL", "PPROTFL"
)

.cdisc_adbds_columns <- c(
  "STUDYID", "USUBJID", "PARAMCD", "PARAM", "AVISIT", "AVISITN",
  "ADT", "BASE", "CHG", "AVAL", "ANL01FL"
)

.cdisc_finding_source_columns <- c(
  "participant_id", "test_code", "test_name", "original_result",
  "original_unit", "standard_result", "standard_unit", "date_time",
  "visit", "visit_number"
)

.cdisc_severity_map <- c(
  "1" = "MILD",
  "2" = "MODERATE",
  "3" = "SEVERE",
  "4" = "LIFE THREATENING",
  "5" = "DEATH"
)

.cdisc_causality_map <- c(
  "not_assessed" = "NOT ASSESSED",
  "unrelated" = "NOT RELATED",
  "unlikely" = "UNLIKELY RELATED",
  "possible" = "POSSIBLY RELATED",
  "probable" = "PROBABLY RELATED",
  "definite" = "RELATED"
)

.cdisc_outcome_map <- c(
  "unknown" = "UNKNOWN",
  "not_recovered" = "NOT RECOVERED/NOT RESOLVED",
  "recovering" = "RECOVERING/RESOLVING",
  "recovered" = "RECOVERED/RESOLVED",
  "recovered_with_sequelae" = "RECOVERED/RESOLVED WITH SEQUELAE",
  "fatal" = "FATAL"
)

.cdisc_action_map <- c(
  "dose not changed" = "DOSE NOT CHANGED",
  "dose reduced" = "DOSE REDUCED",
  "dose increased" = "DOSE INCREASED",
  "drug interrupted" = "DRUG INTERRUPTED",
  "dose interrupted" = "DRUG INTERRUPTED",
  "drug withdrawn" = "DRUG WITHDRAWN",
  "not applicable" = "NOT APPLICABLE",
  "unknown" = "UNKNOWN"
)

.cdisc_scalar_string <- function(x, name) {
  if (!.is_scalar_string(x) || !nzchar(trimws(x))) {
    stop(name, " must be one non-empty string.", call. = FALSE)
  }
  as.character(x)
}

.cdisc_column <- function(data, column, argument, optional = FALSE) {
  if (is.null(column) && optional) {
    return(NULL)
  }
  if (!.is_scalar_string(column) || !column %in% names(data)) {
    stop(argument, " must name an existing column.", call. = FALSE)
  }
  column
}

.cdisc_study_id <- function(x) {
  value <- .cdisc_scalar_string(as.character(x), "study_id")
  if (grepl("[[:cntrl:]]", value)) {
    stop("study_id must not contain control characters.", call. = FALSE)
  }
  value
}

.cdisc_character <- function(x) {
  value <- as.character(x)
  value[is.na(x)] <- NA_character_
  value
}

.cdisc_optional_character <- function(data, column, n, argument) {
  if (is.null(column)) {
    return(rep.int(NA_character_, n))
  }
  column <- .cdisc_column(data, column, argument, optional = TRUE)
  value <- .cdisc_character(data[[column]])
  if (any(!is.na(value) & !nzchar(value))) {
    stop(argument, " contains an empty value.", call. = FALSE)
  }
  value
}

.cdisc_iso_date <- function(x, name, allow_na = TRUE) {
  if (inherits(x, "Date")) {
    value <- format(x, "%Y-%m-%d")
    value[is.na(x)] <- NA_character_
    if (any(!is.na(x) & !is.finite(unclass(x)))) {
      stop(name, " contains a non-finite Date.", call. = FALSE)
    }
  } else {
    value <- .cdisc_character(x)
    candidate <- !is.na(value)
    syntax_ok <- !candidate | grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
      value
    )
    parsed <- rep(as.Date(NA), length(value))
    if (any(candidate & syntax_ok)) {
      parsed[candidate & syntax_ok] <- suppressWarnings(as.Date(
        value[candidate & syntax_ok],
        format = "%Y-%m-%d"
      ))
    }
    if (any(!syntax_ok | (candidate & is.na(parsed)))) {
      stop(name, " must contain valid ISO 8601 dates.", call. = FALSE)
    }
  }
  if (!allow_na && anyNA(value)) {
    stop(name, " must not contain missing dates.", call. = FALSE)
  }
  value
}

.cdisc_iso_datetime <- function(x, name) {
  if (inherits(x, "POSIXct")) {
    if (any(!is.na(x) & !is.finite(as.numeric(x)))) {
      stop(name, " contains a non-finite date-time.", call. = FALSE)
    }
    timezone <- attr(x, "tzone")
    if (!length(timezone) || is.na(timezone[[1L]]) ||
        !nzchar(timezone[[1L]])) {
      timezone <- "UTC"
    } else {
      timezone <- timezone[[1L]]
    }
    value <- format(
      x,
      "%Y-%m-%dT%H:%M:%OS6%z",
      tz = timezone,
      usetz = FALSE
    )
    value <- sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", value)
    if (identical(timezone, "UTC") || identical(timezone, "GMT")) {
      value <- sub("\\+00:00$", "Z", value)
    }
    value[is.na(x)] <- NA_character_
    return(value)
  }
  value <- .cdisc_character(x)
  present <- !is.na(value)
  valid <- .cdisc_datetime_valid(value)
  if (any(!valid)) {
    stop(
      name,
      " must contain ISO 8601 date-times with an explicit offset.",
      call. = FALSE
    )
  }
  if (any(present)) {
    normalized <- sub("Z$", "+0000", value[present])
    normalized <- sub(
      "([+-][0-9]{2}):([0-9]{2})$",
      "\\1\\2",
      normalized
    )
    parsed <- suppressWarnings(as.POSIXct(
      normalized,
      format = "%Y-%m-%dT%H:%M:%OS%z",
      tz = "UTC"
    ))
    if (anyNA(parsed)) {
      stop(name, " contains an invalid date-time.", call. = FALSE)
    }
  }
  value
}

.cdisc_datetime_valid <- function(value) {
  value <- .cdisc_character(value)
  missing <- is.na(value)
  syntax <- missing | grepl(
    paste0(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
      "[0-9]{2}:[0-9]{2}:[0-9]{2}",
      "(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})$"
    ),
    value
  )
  valid <- missing
  index <- which(!missing & syntax)
  if (!length(index)) {
    return(valid)
  }
  current <- value[index]
  date_value <- substr(current, 1L, 10L)
  date_valid <- !is.na(suppressWarnings(as.Date(
    date_value,
    format = "%Y-%m-%d"
  )))
  hour <- suppressWarnings(as.integer(substr(current, 12L, 13L)))
  minute <- suppressWarnings(as.integer(substr(current, 15L, 16L)))
  second <- suppressWarnings(as.integer(substr(current, 18L, 19L)))
  offset <- sub("^.*(Z|[+-][0-9]{2}:[0-9]{2})$", "\\1", current)
  offset_hour <- ifelse(
    offset == "Z",
    0L,
    suppressWarnings(as.integer(substr(offset, 2L, 3L)))
  )
  offset_minute <- ifelse(
    offset == "Z",
    0L,
    suppressWarnings(as.integer(substr(offset, 5L, 6L)))
  )
  valid[index] <- date_valid &
    hour >= 0L & hour <= 23L &
    minute >= 0L & minute <= 59L &
    second >= 0L & second <= 59L &
    offset_hour >= 0L & offset_hour <= 14L &
    offset_minute >= 0L & offset_minute <= 59L &
    (offset_hour < 14L | offset_minute == 0L)
  valid & syntax
}

.cdisc_order <- function(data, columns) {
  if (!nrow(data)) {
    return(integer(0))
  }
  columns <- intersect(columns, names(data))
  if (!length(columns)) {
    return(seq_len(nrow(data)))
  }
  keys <- lapply(data[columns], function(value) {
    if (inherits(value, "Date") || inherits(value, "POSIXct")) {
      return(as.numeric(value))
    }
    if (is.factor(value)) {
      return(as.character(value))
    }
    if (is.list(value)) {
      return(vapply(value, function(item) {
        paste(utils::capture.output(dput(item)), collapse = "")
      }, character(1)))
    }
    value
  })
  do.call(order, c(keys, list(na.last = TRUE, method = "radix")))
}

.cdisc_sequence <- function(subject) {
  out <- integer(length(subject))
  if (!length(subject)) {
    return(out)
  }
  groups <- split(seq_along(subject), subject, drop = TRUE)
  for (indices in groups) {
    if (length(indices) > .Machine$integer.max) {
      stop("A domain sequence exceeds the R integer limit.", call. = FALSE)
    }
    out[indices] <- seq_along(indices)
  }
  out
}

.cdisc_code <- function(value, name) {
  value <- .cdisc_character(value)
  present <- !is.na(value)
  if (any(
    present &
      !grepl("^[A-Z][A-Z0-9_]{0,7}$", value)
  )) {
    stop(
      name,
      " must start with a letter and contain at most eight uppercase ",
      "letters, digits, or underscores.",
      call. = FALSE
    )
  }
  value
}

.cdisc_one_to_one <- function(code, label, code_name, label_name) {
  present <- !is.na(code) & !is.na(label)
  pairs <- unique(data.frame(
    code = code[present],
    label = label[present],
    stringsAsFactors = FALSE
  ))
  if (nrow(pairs) && anyDuplicated(pairs$code)) {
    stop(
      code_name,
      " must map to exactly one ",
      label_name,
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.cdisc_id_map <- function(participants, id_col, study_id) {
  id_col <- .cdisc_column(participants, id_col, "id_col")
  ids <- .cdisc_character(participants[[id_col]])
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop(
      "Participant IDs must be unique, non-empty, and non-missing.",
      call. = FALSE
    )
  }
  order <- order(ids, method = "radix")
  source_id <- ids[order]
  usubjid <- paste(study_id, source_id, sep = "-")
  if (anyDuplicated(usubjid)) {
    stop("Participant IDs do not produce unique USUBJID values.",
         call. = FALSE)
  }
  list(
    rows = order,
    map = data.frame(
      source_id = source_id,
      USUBJID = usubjid,
      stringsAsFactors = FALSE
    )
  )
}

.cdisc_link_subjects <- function(ids, id_map, source) {
  ids <- .cdisc_character(ids)
  if (anyNA(ids) || any(!nzchar(ids))) {
    stop(source, " participant IDs must be non-empty and non-missing.",
         call. = FALSE)
  }
  index <- match(ids, id_map$source_id)
  if (anyNA(index)) {
    stop(
      source,
      " contains participant IDs absent from participants: ",
      paste(unique(ids[is.na(index)]), collapse = ", "),
      call. = FALSE
    )
  }
  id_map$USUBJID[index]
}

.cdisc_manifest <- function(datasets, standard) {
  labels <- c(
    DM = "Demographics",
    AE = "Adverse Events",
    VS = "Vital Signs",
    EG = "ECG Test Results",
    XP = "Physiological Signal Findings",
    ADSL = "Subject-Level Analysis Dataset",
    ADBDS = "Basic Data Structure Analysis Dataset",
    QS = "Questionnaires",
    ADQS = "Questionnaire Analysis Dataset"
  )
  classes <- c(
    DM = "SPECIAL PURPOSE",
    AE = "EVENTS",
    VS = "FINDINGS",
    EG = "FINDINGS",
    XP = "FINDINGS",
    ADSL = "SUBJECT LEVEL ANALYSIS DATASET",
    ADBDS = "BASIC DATA STRUCTURE",
    QS = "FINDINGS",
    ADQS = "BASIC DATA STRUCTURE"
  )
  keys <- c(
    DM = "USUBJID",
    AE = "USUBJID,AESEQ",
    VS = "USUBJID,VSSEQ",
    EG = "USUBJID,EGSEQ",
    XP = "USUBJID,XPSEQ",
    ADSL = "USUBJID",
    ADBDS = "USUBJID,PARAMCD,AVISITN,ADT",
    QS = "USUBJID,QSSEQ",
    ADQS = "USUBJID,PARAMCD,AVISITN,ADT"
  )
  dataset_names <- names(datasets)
  data.frame(
    dataset = dataset_names,
    label = unname(ifelse(
      dataset_names %in% names(labels),
      labels[dataset_names],
      dataset_names
    )),
    class = unname(ifelse(
      dataset_names %in% names(classes),
      classes[dataset_names],
      if (identical(standard, "SDTM-shaped")) "SPONSOR DEFINED" else
        "ANALYSIS DATASET"
    )),
    n_rows = as.integer(vapply(datasets, nrow, integer(1))),
    n_columns = as.integer(vapply(datasets, ncol, integer(1))),
    key = unname(ifelse(
      dataset_names %in% names(keys),
      keys[dataset_names],
      "USUBJID"
    )),
    sponsor_defined = dataset_names == "XP" |
      !dataset_names %in% names(labels),
    stringsAsFactors = FALSE
  )
}

.cdisc_empty_dm <- function() {
  out <- as.data.frame(
    stats::setNames(rep(list(character(0)), length(.cdisc_dm_columns)),
                    .cdisc_dm_columns),
    stringsAsFactors = FALSE
  )
  out
}

.cdisc_empty_ae <- function() {
  out <- as.data.frame(
    stats::setNames(rep(list(character(0)), length(.cdisc_ae_columns)),
                    .cdisc_ae_columns),
    stringsAsFactors = FALSE
  )
  out$AESEQ <- integer(0)
  out[, .cdisc_ae_columns, drop = FALSE]
}

.cdisc_finding_columns <- function(domain) {
  c(
    "STUDYID", "DOMAIN", "USUBJID", paste0(domain, "SEQ"),
    paste0(domain, "TESTCD"), paste0(domain, "TEST"),
    paste0(domain, "ORRES"), paste0(domain, "ORRESU"),
    paste0(domain, "STRESC"), paste0(domain, "STRESN"),
    paste0(domain, "STRESU"), "VISITNUM", "VISIT",
    paste0(domain, "DTC")
  )
}

.cdisc_empty_finding <- function(domain) {
  columns <- .cdisc_finding_columns(domain)
  out <- as.data.frame(
    stats::setNames(rep(list(character(0)), length(columns)), columns),
    stringsAsFactors = FALSE
  )
  out[[paste0(domain, "SEQ")]] <- integer(0)
  out[[paste0(domain, "STRESN")]] <- numeric(0)
  out$VISITNUM <- numeric(0)
  out[, columns, drop = FALSE]
}

.cdisc_findings <- function(data, domain, study_id, id_map) {
  if (!is.data.frame(data)) {
    stop("findings$", domain, " must be a data.frame.", call. = FALSE)
  }
  missing_columns <- setdiff(.cdisc_finding_source_columns, names(data))
  if (length(missing_columns)) {
    stop(
      "findings$",
      domain,
      " is missing normalized columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  data <- data[, .cdisc_finding_source_columns, drop = FALSE]
  if (!nrow(data)) {
    return(.cdisc_empty_finding(domain))
  }
  participant_id <- .cdisc_character(data$participant_id)
  usubjid <- .cdisc_link_subjects(
    participant_id,
    id_map,
    paste0("findings$", domain)
  )
  test_code <- .cdisc_code(data$test_code, "test_code")
  test_name <- .cdisc_character(data$test_name)
  if (anyNA(test_code) || anyNA(test_name) || any(!nzchar(test_name))) {
    stop("Finding test_code and test_name must be non-missing.",
         call. = FALSE)
  }
  .cdisc_one_to_one(test_code, test_name, "test_code", "test_name")

  if (!is.atomic(data$original_result) || is.list(data$original_result) ||
      !is.numeric(data$standard_result)) {
    stop(
      "original_result must be atomic and standard_result must be numeric.",
      call. = FALSE
    )
  }
  if (any(!is.na(data$standard_result) &
          !is.finite(data$standard_result))) {
    stop("standard_result must be finite when supplied.", call. = FALSE)
  }
  if (is.numeric(data$original_result) &&
      any(!is.na(data$original_result) &
          !is.finite(data$original_result))) {
    stop("original_result must be finite when numeric.", call. = FALSE)
  }
  original_result <- .cdisc_character(data$original_result)
  standard_result <- as.numeric(data$standard_result)
  if (any(is.na(original_result) & is.na(standard_result))) {
    stop("Each finding requires an original or standard result.",
         call. = FALSE)
  }
  original_unit <- .cdisc_character(data$original_unit)
  standard_unit <- .cdisc_character(data$standard_unit)
  if (any(!is.na(original_unit) & !nzchar(original_unit)) ||
      any(!is.na(standard_unit) & !nzchar(standard_unit))) {
    stop("Finding units must be non-empty when supplied.", call. = FALSE)
  }
  if (!is.numeric(data$visit_number) ||
      any(!is.na(data$visit_number) &
          !is.finite(data$visit_number))) {
    stop("visit_number must be finite numeric when supplied.",
         call. = FALSE)
  }
  visit <- .cdisc_character(data$visit)
  if (any(!is.na(visit) & !nzchar(visit))) {
    stop("visit must be non-empty when supplied.", call. = FALSE)
  }
  date_time <- .cdisc_iso_datetime(data$date_time, "date_time")
  standard_character <- ifelse(
    is.na(standard_result),
    NA_character_,
    format(standard_result, scientific = FALSE, trim = TRUE, digits = 15)
  )
  source <- data.frame(
    participant_id = participant_id,
    USUBJID = usubjid,
    test_code = test_code,
    test_name = test_name,
    original_result = original_result,
    original_unit = original_unit,
    standard_result = standard_result,
    standard_character = standard_character,
    standard_unit = standard_unit,
    date_time = date_time,
    visit = visit,
    visit_number = as.numeric(data$visit_number),
    stringsAsFactors = FALSE
  )
  row_order <- .cdisc_order(source, c(
    "USUBJID", "visit_number", "date_time", "test_code", "test_name",
    "original_result", "standard_result", "standard_unit"
  ))
  source <- source[row_order, , drop = FALSE]
  sequence <- .cdisc_sequence(source$USUBJID)
  values <- list(
    STUDYID = rep.int(study_id, nrow(source)),
    DOMAIN = rep.int(domain, nrow(source)),
    USUBJID = source$USUBJID,
    sequence,
    source$test_code,
    source$test_name,
    source$original_result,
    source$original_unit,
    source$standard_character,
    source$standard_result,
    source$standard_unit,
    source$visit_number,
    source$visit,
    source$date_time
  )
  names(values) <- .cdisc_finding_columns(domain)
  out <- as.data.frame(values, stringsAsFactors = FALSE)
  out[[paste0(domain, "SEQ")]] <- as.integer(
    out[[paste0(domain, "SEQ")]]
  )
  rownames(out) <- NULL
  out
}

.cdisc_ae_metadata_value <- function(metadata, names) {
  vapply(metadata, function(item) {
    if (!is.list(item)) {
      return(NA_character_)
    }
    for (name in names) {
      value <- item[[name]]
      if (.is_scalar_string(value, allow_na = TRUE)) {
        return(as.character(value))
      }
    }
    NA_character_
  }, character(1))
}

.cdisc_map_action <- function(action) {
  action <- .cdisc_character(action)
  normalized <- tolower(gsub("_", " ", action, fixed = TRUE))
  mapped <- unname(.cdisc_action_map[normalized])
  fallback <- toupper(action)
  fallback[is.na(action)] <- NA_character_
  mapped[is.na(mapped)] <- fallback[is.na(mapped)]
  mapped
}

.cdisc_build_ae <- function(adverse_events, study_id, id_map) {
  if (is.null(adverse_events)) {
    return(.cdisc_empty_ae())
  }
  events <- .as_ae_data(adverse_events)
  if (!nrow(events)) {
    return(.cdisc_empty_ae())
  }
  events$USUBJID <- .cdisc_link_subjects(
    events$participant_id,
    id_map,
    "adverse_events"
  )
  events$.AEBODSYS <- .cdisc_ae_metadata_value(
    events$metadata,
    c("meddra_soc", "soc", "body_system")
  )
  order <- .cdisc_order(events, c(
    "USUBJID", "onset_date", "end_date", "term", "meddra_code",
    "meddra_pt", ".AEBODSYS", "ctcae_grade", "serious", "causality",
    "action", "outcome"
  ))
  events <- events[order, , drop = FALSE]
  out <- data.frame(
    STUDYID = rep.int(study_id, nrow(events)),
    DOMAIN = rep.int("AE", nrow(events)),
    USUBJID = events$USUBJID,
    AESEQ = .cdisc_sequence(events$USUBJID),
    AETERM = .cdisc_character(events$term),
    AEDECOD = .cdisc_character(events$meddra_pt),
    AEBODSYS = events$.AEBODSYS,
    AESEV = unname(.cdisc_severity_map[
      as.character(events$ctcae_grade)
    ]),
    AESER = ifelse(events$serious, "Y", "N"),
    AEREL = unname(.cdisc_causality_map[events$causality]),
    AEACN = .cdisc_map_action(events$action),
    AEOUT = unname(.cdisc_outcome_map[events$outcome]),
    AESTDTC = format(events$onset_date, "%Y-%m-%d"),
    AEENDTC = format(events$end_date, "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  out$AEENDTC[is.na(events$end_date)] <- NA_character_
  out$AESEQ <- as.integer(out$AESEQ)
  rownames(out) <- NULL
  out[, .cdisc_ae_columns, drop = FALSE]
}

.cdisc_extra_sdtm <- function(
  extra_domains,
  study_id,
  id_map,
  generated
) {
  if (!is.list(extra_domains) || is.data.frame(extra_domains)) {
    stop("extra_domains must be a named list of data.frames.",
         call. = FALSE)
  }
  if (!length(extra_domains)) {
    return(list())
  }
  names_extra <- names(extra_domains)
  if (is.null(names_extra) || anyNA(names_extra) ||
      any(!grepl("^[A-Z]{2}$", names_extra)) ||
      anyDuplicated(names_extra)) {
    stop(
      "extra_domains must have unique two-letter uppercase names.",
      call. = FALSE
    )
  }
  collision <- intersect(names_extra, generated)
  if (length(collision)) {
    stop(
      "extra_domains collides with generated domains: ",
      paste(collision, collapse = ", "),
      call. = FALSE
    )
  }
  result <- vector("list", length(extra_domains))
  names(result) <- names_extra
  for (i in seq_along(extra_domains)) {
    domain <- names_extra[[i]]
    data <- extra_domains[[i]]
    if (!is.data.frame(data)) {
      stop("extra domain ", domain, " must be a data.frame.",
           call. = FALSE)
    }
    if (any(vapply(data, function(value) {
      is.list(value) || is.matrix(value) || is.data.frame(value)
    }, logical(1)))) {
      stop("extra domain ", domain, " variables must be atomic vectors.",
           call. = FALSE)
    }
    required <- c("STUDYID", "DOMAIN", "USUBJID")
    if (!all(required %in% names(data))) {
      stop(
        "extra domain ",
        domain,
        " requires STUDYID, DOMAIN, and USUBJID.",
        call. = FALSE
      )
    }
    if (any(.cdisc_character(data$STUDYID) != study_id, na.rm = TRUE) ||
        anyNA(data$STUDYID) ||
        any(.cdisc_character(data$DOMAIN) != domain, na.rm = TRUE) ||
        anyNA(data$DOMAIN)) {
      stop("extra domain ", domain, " has inconsistent constants.",
           call. = FALSE)
    }
    usubjid <- .cdisc_character(data$USUBJID)
    if (anyNA(usubjid) || any(!usubjid %in% id_map$USUBJID)) {
      stop("extra domain ", domain, " has an unknown USUBJID.",
           call. = FALSE)
    }
    sequence_name <- paste0(domain, "SEQ")
    order_columns <- c("USUBJID", sequence_name, names(data))
    if (!sequence_name %in% names(data)) {
      order_columns <- "USUBJID"
    }
    data <- data[.cdisc_order(data, order_columns), , drop = FALSE]
    rownames(data) <- NULL
    result[[i]] <- data
  }
  result
}

#' Export Trial Data to CDISC SDTM-Shaped Domains
#'
#' Builds deterministic, linked trial domains with explicit sponsor mappings.
#' The output is structurally SDTM-shaped; it is not a claim of CDISC,
#' regulator, or third-party validator conformance.
#'
#' @param trial A [Trial] object.
#' @param participants One row per screened or enrolled participant.
#' @param adverse_events Optional adverse events accepted by [aeSummary()].
#' @param findings Named list containing normalized `VS`, `EG`, or `XP`
#'   findings data frames.
#' @param study_id Non-empty study identifier.
#' @param id_col,arm_col Participant ID and assigned-arm columns.
#' @param sex_col,birth_date_col,reference_start_col,reference_end_col Optional
#'   participant columns.
#' @param country_col,site_col Optional country and site columns.
#' @param extra_domains Named list of already constructed two-letter domains,
#'   such as output from `PhysioClinical::toCDISC_QS()`.
#' @return An `sdtm_export` containing datasets, manifest, and mapping metadata.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' participants <- data.frame(
#'   id = c("p1", "p2"),
#'   arm = c("active", "control")
#' )
#' export <- toSDTM(trial, participants)
#' export$datasets$DM
#' @export
toSDTM <- function(
  trial,
  participants,
  adverse_events = NULL,
  findings = list(),
  study_id = trial@id,
  id_col = "id",
  arm_col = "arm",
  sex_col = NULL,
  birth_date_col = NULL,
  reference_start_col = NULL,
  reference_end_col = NULL,
  country_col = NULL,
  site_col = NULL,
  extra_domains = list()
) {
  if (!methods::is(trial, "Trial")) {
    stop("trial must be a Trial object.", call. = FALSE)
  }
  if (!is.data.frame(participants) || !nrow(participants)) {
    stop("participants must be a non-empty data.frame.", call. = FALSE)
  }
  study_id <- .cdisc_study_id(study_id)
  arm_col <- .cdisc_column(participants, arm_col, "arm_col")
  identity <- .cdisc_id_map(participants, id_col, study_id)
  rows <- identity$rows
  id_map <- identity$map
  arm <- .cdisc_character(participants[[arm_col]])[rows]
  if (any(!is.na(arm) & (!nzchar(arm) | !arm %in% trial@arms))) {
    stop("arm_col contains an unknown or empty trial arm.", call. = FALSE)
  }
  arm_number <- match(arm, trial@arms)
  arm_code <- ifelse(is.na(arm_number), NA_character_,
                     paste0("ARM", arm_number))

  sex <- .cdisc_optional_character(
    participants[rows, , drop = FALSE],
    sex_col,
    length(rows),
    "sex_col"
  )
  allowed_sex <- c("M", "F", "U", "UNDIFFERENTIATED")
  if (any(!is.na(sex) & !sex %in% allowed_sex)) {
    stop(
      "sex_col must already use M/F/U/UNDIFFERENTIATED terminology.",
      call. = FALSE
    )
  }
  date_column <- function(column, argument) {
    if (is.null(column)) {
      return(rep.int(NA_character_, length(rows)))
    }
    column <- .cdisc_column(participants, column, argument, optional = TRUE)
    .cdisc_iso_date(participants[[column]][rows], argument)
  }
  dm <- data.frame(
    STUDYID = rep.int(study_id, length(rows)),
    DOMAIN = rep.int("DM", length(rows)),
    USUBJID = id_map$USUBJID,
    SUBJID = id_map$source_id,
    SITEID = .cdisc_optional_character(
      participants[rows, , drop = FALSE],
      site_col,
      length(rows),
      "site_col"
    ),
    RFSTDTC = date_column(reference_start_col, "reference_start_col"),
    RFENDTC = date_column(reference_end_col, "reference_end_col"),
    BRTHDTC = date_column(birth_date_col, "birth_date_col"),
    SEX = sex,
    COUNTRY = .cdisc_optional_character(
      participants[rows, , drop = FALSE],
      country_col,
      length(rows),
      "country_col"
    ),
    ARMCD = arm_code,
    ARM = arm,
    stringsAsFactors = FALSE
  )
  dm <- dm[, .cdisc_dm_columns, drop = FALSE]

  ae <- .cdisc_build_ae(adverse_events, study_id, id_map)
  if (!is.list(findings) || is.data.frame(findings)) {
    stop("findings must be a named list.", call. = FALSE)
  }
  finding_names <- names(findings)
  if (length(findings) && (
    is.null(finding_names) || anyNA(finding_names) ||
    anyDuplicated(finding_names) ||
    any(!finding_names %in% c("VS", "EG", "XP"))
  )) {
    stop("findings names must be unique and drawn from VS, EG, and XP.",
         call. = FALSE)
  }
  finding_domains <- lapply(finding_names, function(domain) {
    .cdisc_findings(findings[[domain]], domain, study_id, id_map)
  })
  names(finding_domains) <- finding_names
  generated_names <- c("DM", "AE", finding_names)
  extra <- .cdisc_extra_sdtm(
    extra_domains,
    study_id,
    id_map,
    generated_names
  )
  datasets <- c(list(DM = dm, AE = ae), finding_domains, extra)
  mappings <- list(
    arm = data.frame(
      source = trial@arms,
      target = paste0("ARM", seq_along(trial@arms)),
      stringsAsFactors = FALSE
    ),
    ae_severity = data.frame(
      source = as.integer(names(.cdisc_severity_map)),
      target = unname(.cdisc_severity_map),
      stringsAsFactors = FALSE
    ),
    ae_causality = data.frame(
      source = names(.cdisc_causality_map),
      target = unname(.cdisc_causality_map),
      stringsAsFactors = FALSE
    ),
    ae_action = data.frame(
      source = names(.cdisc_action_map),
      target = unname(.cdisc_action_map),
      stringsAsFactors = FALSE
    ),
    ae_outcome = data.frame(
      source = names(.cdisc_outcome_map),
      target = unname(.cdisc_outcome_map),
      stringsAsFactors = FALSE
    )
  )
  metadata <- list(
    id_map = id_map,
    source_to_target = list(
      DM = c(
        id_col = "SUBJID/USUBJID",
        arm_col = "ARM",
        sex_col = "SEX",
        birth_date_col = "BRTHDTC",
        reference_start_col = "RFSTDTC",
        reference_end_col = "RFENDTC",
        country_col = "COUNTRY",
        site_col = "SITEID"
      ),
      AE = c(
        participant_id = "USUBJID",
        term = "AETERM",
        meddra_pt = "AEDECOD",
        ctcae_grade = "AESEV",
        serious = "AESER",
        causality = "AEREL",
        action = "AEACN",
        outcome = "AEOUT"
      )
    ),
    mappings = mappings,
    sponsor_defined = list(
      domains = intersect(names(datasets), "XP"),
      extra_domains = names(extra) %||% character(0),
      ae_action_values = setdiff(
        unique(stats::na.omit(ae$AEACN)),
        unname(.cdisc_action_map)
      ),
      meddra = if (is.null(adverse_events)) {
        data.frame(
          code = character(0),
          version = character(0),
          stringsAsFactors = FALSE
        )
      } else {
        source_ae <- .as_ae_data(adverse_events)
        unique(data.frame(
          code = .cdisc_character(source_ae$meddra_code),
          version = .cdisc_character(source_ae$meddra_version),
          stringsAsFactors = FALSE
        ))
      }
    )
  )
  structure(
    list(
      datasets = datasets,
      manifest = .cdisc_manifest(datasets, "SDTM-shaped"),
      study_id = study_id,
      standard = "SDTM-shaped",
      metadata = metadata
    ),
    class = "sdtm_export"
  )
}

.cdisc_flag_name <- function(set_name) {
  value <- toupper(gsub("[^A-Za-z0-9_]", "", set_name))
  if (!nzchar(value) || !grepl("^[A-Z]", value)) {
    stop("Analysis-set names must produce a valid flag name.",
         call. = FALSE)
  }
  paste0(substr(value, 1L, 8L), "FL")
}

.cdisc_validate_flag_set <- function(x, trial_id, source_ids, name) {
  .validate_analysis_set(x)
  if (!identical(x$trial_id, trial_id)) {
    stop(name, " has a different trial_id.", call. = FALSE)
  }
  if (!setequal(x$source_ids, source_ids) ||
      length(x$source_ids) != length(source_ids)) {
    stop(name, " has different source_ids.", call. = FALSE)
  }
  invisible(TRUE)
}

.cdisc_empty_adbds <- function() {
  data.frame(
    STUDYID = character(0),
    USUBJID = character(0),
    PARAMCD = character(0),
    PARAM = character(0),
    AVISIT = character(0),
    AVISITN = numeric(0),
    ADT = character(0),
    BASE = numeric(0),
    CHG = numeric(0),
    AVAL = numeric(0),
    ANL01FL = character(0),
    stringsAsFactors = FALSE
  )
}

.cdisc_optional_bds_column <- function(data, name, prototype) {
  if (name %in% names(data)) {
    return(data[[name]])
  }
  if (is.character(prototype)) {
    return(rep.int(NA_character_, nrow(data)))
  }
  rep.int(NA_real_, nrow(data))
}

.cdisc_build_adbds <- function(
  bds,
  study_id,
  id_map,
  primary_members
) {
  if (is.null(bds)) {
    return(.cdisc_empty_adbds())
  }
  if (!is.data.frame(bds)) {
    stop("The generic ADBDS source must be a data.frame.",
         call. = FALSE)
  }
  required <- c("participant_id", "param_code", "param_label", "value")
  missing_columns <- setdiff(required, names(bds))
  if (length(missing_columns)) {
    stop(
      "bds is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(bds)) {
    return(.cdisc_empty_adbds())
  }
  participant_id <- .cdisc_character(bds$participant_id)
  usubjid <- .cdisc_link_subjects(participant_id, id_map, "bds")
  param_code <- .cdisc_code(bds$param_code, "param_code")
  param_label <- .cdisc_character(bds$param_label)
  if (anyNA(param_code) || anyNA(param_label) ||
      any(!nzchar(param_label))) {
    stop("param_code and param_label must be non-empty and non-missing.",
         call. = FALSE)
  }
  .cdisc_one_to_one(
    param_code,
    param_label,
    "param_code",
    "param_label"
  )
  value <- bds$value
  baseline <- .cdisc_optional_bds_column(bds, "baseline_value", numeric())
  change <- .cdisc_optional_bds_column(bds, "change_value", numeric())
  analysis <- .cdisc_optional_bds_column(
    bds,
    "analysis_value",
    numeric()
  )
  numeric_values <- list(
    value = value,
    baseline_value = baseline,
    change_value = change,
    analysis_value = analysis
  )
  for (name in names(numeric_values)) {
    current <- numeric_values[[name]]
    if (!is.numeric(current) ||
        any(!is.na(current) & !is.finite(current))) {
      stop(name, " must be finite numeric when supplied.",
           call. = FALSE)
    }
  }
  visit <- .cdisc_character(
    .cdisc_optional_bds_column(bds, "visit", character())
  )
  if (any(!is.na(visit) & !nzchar(visit))) {
    stop("visit must be non-empty when supplied.", call. = FALSE)
  }
  visit_number <- .cdisc_optional_bds_column(
    bds,
    "visit_number",
    numeric()
  )
  if (!is.numeric(visit_number) ||
      any(!is.na(visit_number) & !is.finite(visit_number))) {
    stop("visit_number must be finite numeric when supplied.",
         call. = FALSE)
  }
  analysis_date <- .cdisc_iso_date(
    .cdisc_optional_bds_column(bds, "analysis_date", character()),
    "analysis_date"
  )
  check <- !is.na(value) & !is.na(baseline) & !is.na(change)
  tolerance <- 1e-10 * pmax(1, abs(value), abs(baseline), abs(change))
  if (any(check & abs(change - (value - baseline)) > tolerance)) {
    stop("change_value must equal value minus baseline_value.",
         call. = FALSE)
  }
  aval <- analysis
  aval[is.na(aval)] <- value[is.na(aval)]
  out <- data.frame(
    STUDYID = rep.int(study_id, nrow(bds)),
    USUBJID = usubjid,
    PARAMCD = param_code,
    PARAM = param_label,
    AVISIT = visit,
    AVISITN = as.numeric(visit_number),
    ADT = analysis_date,
    BASE = as.numeric(baseline),
    CHG = as.numeric(change),
    AVAL = as.numeric(aval),
    ANL01FL = ifelse(participant_id %in% primary_members, "Y", "N"),
    stringsAsFactors = FALSE
  )
  order <- .cdisc_order(out, c(
    "USUBJID", "PARAMCD", "AVISITN", "ADT", "AVISIT",
    "BASE", "CHG", "AVAL"
  ))
  out <- out[order, .cdisc_adbds_columns, drop = FALSE]
  key_value <- lapply(
    out[c("USUBJID", "PARAMCD", "AVISITN", "ADT")],
    function(value) {
      value <- as.character(value)
      value[is.na(value)] <- "<MISSING>"
      value
    }
  )
  key <- do.call(paste, c(key_value, sep = "\r"))
  if (anyDuplicated(key)) {
    stop(
      "ADBDS requires unique USUBJID/PARAMCD/AVISITN/ADT keys.",
      call. = FALSE
    )
  }
  rownames(out) <- NULL
  out
}

.cdisc_adam_extra <- function(
  datasets,
  study_id,
  id_map,
  reserved = c("ADSL", "ADBDS")
) {
  if (!length(datasets)) {
    return(list())
  }
  if (!is.list(datasets) || is.data.frame(datasets)) {
    stop("Additional ADaM datasets must be supplied as a named list.",
         call. = FALSE)
  }
  dataset_names <- names(datasets)
  if (is.null(dataset_names) || anyNA(dataset_names) ||
      any(!grepl("^[A-Z][A-Z0-9]{1,7}$", dataset_names)) ||
      anyDuplicated(dataset_names) ||
      length(intersect(dataset_names, reserved))) {
    stop("Additional ADaM dataset names are invalid or reserved.",
         call. = FALSE)
  }
  result <- vector("list", length(datasets))
  names(result) <- dataset_names
  for (i in seq_along(datasets)) {
    name <- dataset_names[[i]]
    data <- datasets[[i]]
    if (!is.data.frame(data) ||
        !all(c("STUDYID", "USUBJID") %in% names(data))) {
      stop(name, " must be a data.frame with STUDYID and USUBJID.",
           call. = FALSE)
    }
    if (any(vapply(data, function(value) {
      is.list(value) || is.matrix(value) || is.data.frame(value)
    }, logical(1)))) {
      stop(name, " variables must be atomic vectors.", call. = FALSE)
    }
    if (anyNA(data$STUDYID) ||
        any(.cdisc_character(data$STUDYID) != study_id) ||
        anyNA(data$USUBJID) ||
        any(!.cdisc_character(data$USUBJID) %in% id_map$USUBJID)) {
      stop(name, " has inconsistent study or subject linkage.",
           call. = FALSE)
    }
    rownames(data) <- NULL
    result[[i]] <- data
  }
  result
}

#' Export Trial Analysis Sets to ADaM-Shaped Data
#'
#' Builds an ADSL population table and a generic BDS analysis table from
#' explicit analysis sets. This structural export is not a claim of formal
#' ADaM conformance.
#'
#' @param set The authoritative primary `analysis_set`.
#' @param subject_data One row per source participant.
#' @param bds Optional generic BDS source data frame. A named list may contain
#'   an `ADBDS` source plus already constructed analysis datasets such as
#'   `ADQS`.
#' @param study_id Non-empty study identifier.
#' @param id_col,treatment_col Subject ID and originally assigned treatment
#'   columns.
#' @param population_flags Optional named list of other `analysis_set`
#'   objects from the same source population.
#' @return An `adam_export` containing datasets, manifest, and population
#'   metadata.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' subjects <- data.frame(
#'   id = c("p1", "p2", "screen"),
#'   arm = c("active", "control", NA),
#'   randomized = c(TRUE, TRUE, FALSE)
#' )
#' itt <- intentionToTreat(trial, subjects)
#' export <- toADaM(itt, subjects)
#' export$datasets$ADSL
#' @export
toADaM <- function(
  set,
  subject_data,
  bds = NULL,
  study_id = set$trial_id,
  id_col = "id",
  treatment_col = "arm",
  population_flags = NULL
) {
  .validate_analysis_set(set)
  if (!is.data.frame(subject_data) || !nrow(subject_data)) {
    stop("subject_data must be a non-empty data.frame.", call. = FALSE)
  }
  study_id <- .cdisc_study_id(study_id)
  id_col <- .cdisc_column(subject_data, id_col, "id_col")
  treatment_col <- .cdisc_column(
    subject_data,
    treatment_col,
    "treatment_col"
  )
  identity <- .cdisc_id_map(subject_data, id_col, study_id)
  id_map <- identity$map
  if (!setequal(id_map$source_id, set$source_ids) ||
      nrow(id_map) != length(set$source_ids)) {
    stop("subject_data IDs must equal the analysis-set source IDs.",
         call. = FALSE)
  }
  rows <- identity$rows
  treatment <- .cdisc_character(subject_data[[treatment_col]])[rows]
  if (any(!is.na(treatment) & !nzchar(treatment))) {
    stop("treatment_col contains an empty treatment.", call. = FALSE)
  }
  arm_order <- set$arm_order %||% unique(stats::na.omit(
    .cdisc_character(subject_data[[treatment_col]])
  ))
  if (!is.character(arm_order) || anyNA(arm_order) ||
      any(!nzchar(arm_order)) || anyDuplicated(arm_order) ||
      any(!is.na(treatment) & !treatment %in% arm_order)) {
    stop("The analysis set does not define a valid trial arm order.",
         call. = FALSE)
  }

  flags <- list()
  flags[[toupper(set$set)]] <- set
  if (!is.null(population_flags)) {
    if (!is.list(population_flags) || is.data.frame(population_flags)) {
      stop("population_flags must be a uniquely named list.",
           call. = FALSE)
    }
    if (length(population_flags) && (
        is.null(names(population_flags)) ||
        anyNA(names(population_flags)) || any(!nzchar(names(population_flags))) ||
        anyDuplicated(toupper(names(population_flags)))
    )) {
      stop("population_flags must be a uniquely named list.",
           call. = FALSE)
    }
    for (i in seq_along(population_flags)) {
      candidate <- population_flags[[i]]
      name <- toupper(names(population_flags)[[i]])
      .cdisc_validate_flag_set(
        candidate,
        set$trial_id,
        set$source_ids,
        paste0("population_flags$", names(population_flags)[[i]])
      )
      if (!identical(toupper(candidate$set), name)) {
        stop("population_flags names must match each set name.",
             call. = FALSE)
      }
      if (name %in% names(flags) &&
          !setequal(candidate$members$participant_id,
                    flags[[name]]$members$participant_id)) {
        stop("Conflicting analysis sets share a flag name.",
             call. = FALSE)
      }
      flags[[name]] <- candidate
    }
  }
  membership <- function(name) {
    candidate <- flags[[name]]
    if (is.null(candidate)) {
      return(rep.int(NA_character_, nrow(id_map)))
    }
    ifelse(id_map$source_id %in% candidate$members$participant_id, "Y", "N")
  }
  adsl <- data.frame(
    STUDYID = rep.int(study_id, nrow(id_map)),
    USUBJID = id_map$USUBJID,
    SUBJID = id_map$source_id,
    TRT01P = treatment,
    TRT01PN = as.numeric(match(treatment, arm_order)),
    ITTFL = membership("ITT"),
    PPROTFL = membership("PP"),
    stringsAsFactors = FALSE
  )
  if (!toupper(set$set) %in% c("ITT", "PP")) {
    custom_flag <- .cdisc_flag_name(set$set)
    if (custom_flag %in% names(adsl)) {
      stop("The primary analysis-set flag collides with ADSL.",
           call. = FALSE)
    }
    adsl[[custom_flag]] <- membership(toupper(set$set))
  }
  source_extra <- subject_data[rows, , drop = FALSE]
  extra_names <- setdiff(names(source_extra), c(id_col, treatment_col))
  collision <- intersect(extra_names, names(adsl))
  if (length(collision)) {
    stop(
      "subject_data columns collide with ADSL variables: ",
      paste(collision, collapse = ", "),
      call. = FALSE
    )
  }
  for (name in extra_names) {
    value <- source_extra[[name]]
    if (!is.atomic(value) || is.matrix(value) || is.list(value)) {
      stop(
        "Retained subject_data columns must be scalar atomic vectors.",
        call. = FALSE
      )
    }
    adsl[[name]] <- value
  }
  rownames(adsl) <- NULL

  generic_bds <- bds
  extra_input <- list()
  if (is.list(bds) && !is.data.frame(bds)) {
    invalid_names <- length(bds) && (
      is.null(names(bds)) || anyNA(names(bds)) ||
        any(!nzchar(names(bds))) || anyDuplicated(names(bds))
    )
    if (invalid_names) {
      stop("A list-valued bds must have unique non-empty names.",
           call. = FALSE)
    }
    generic_bds <- bds[["ADBDS"]]
    extra_input <- bds[setdiff(names(bds), "ADBDS")]
  }
  adbds <- .cdisc_build_adbds(
    generic_bds,
    study_id,
    id_map,
    set$members$participant_id
  )
  extra <- .cdisc_adam_extra(extra_input, study_id, id_map)
  datasets <- c(list(ADSL = adsl, ADBDS = adbds), extra)
  exclusions <- lapply(flags, function(candidate) candidate$excluded)
  structure(
    list(
      datasets = datasets,
      manifest = .cdisc_manifest(datasets, "ADaM-shaped"),
      study_id = study_id,
      standard = "ADaM-shaped",
      metadata = list(
        id_map = id_map,
        arm_order = arm_order,
        primary_set = set$set,
        population_flags = names(flags),
        analysis_exclusions = exclusions,
        source_to_target = list(
          ADSL = c(
            id_col = "SUBJID/USUBJID",
            treatment_col = "TRT01P/TRT01PN"
          ),
          ADBDS = c(
            participant_id = "USUBJID",
            param_code = "PARAMCD",
            param_label = "PARAM",
            analysis_value = "AVAL"
          )
        )
      )
    ),
    class = "adam_export"
  )
}

.cdisc_issue_table <- function() {
  data.frame(
    dataset = character(0),
    severity = character(0),
    rule_id = character(0),
    variable = character(0),
    row = integer(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
}

.cdisc_controlled_terms <- function(controlled_terms) {
  if (is.null(controlled_terms)) {
    return(list())
  }
  if (is.data.frame(controlled_terms)) {
    if (!all(c("variable", "value") %in% names(controlled_terms))) {
      stop(
        "controlled_terms data.frame requires variable and value columns.",
        call. = FALSE
      )
    }
    variable <- .cdisc_character(controlled_terms$variable)
    value <- .cdisc_character(controlled_terms$value)
    if (anyNA(variable) || anyNA(value) ||
        any(!nzchar(variable)) || any(!nzchar(value))) {
      stop("controlled_terms contains a missing or empty value.",
           call. = FALSE)
    }
    return(split(value, variable))
  }
  if (!is.list(controlled_terms) || is.null(names(controlled_terms)) ||
      anyNA(names(controlled_terms)) || any(!nzchar(names(controlled_terms))) ||
      anyDuplicated(names(controlled_terms))) {
    stop("controlled_terms must be a named list or a two-column data.frame.",
         call. = FALSE)
  }
  result <- lapply(controlled_terms, function(value) {
    if (!is.atomic(value) || is.list(value)) {
      stop("Each controlled_terms entry must be an atomic vector.",
           call. = FALSE)
    }
    value <- .cdisc_character(value)
    if (anyNA(value) || any(!nzchar(value))) {
      stop("controlled_terms entries must be non-empty and non-missing.",
           call. = FALSE)
    }
    unique(value)
  })
  result
}

.cdisc_expected_columns <- function(name, standard) {
  if (identical(name, "DM")) {
    return(.cdisc_dm_columns)
  }
  if (identical(name, "AE")) {
    return(.cdisc_ae_columns)
  }
  if (name %in% c("VS", "EG", "XP")) {
    return(.cdisc_finding_columns(name))
  }
  if (identical(name, "ADSL")) {
    return(.cdisc_adsl_columns)
  }
  if (identical(name, "ADBDS")) {
    return(.cdisc_adbds_columns)
  }
  NULL
}

.cdisc_expected_types <- function(name) {
  character_type <- function(columns) {
    stats::setNames(rep.int("character", length(columns)), columns)
  }
  if (identical(name, "DM")) {
    return(character_type(.cdisc_dm_columns))
  }
  if (identical(name, "AE")) {
    result <- character_type(.cdisc_ae_columns)
    result[["AESEQ"]] <- "integer"
    return(result)
  }
  if (name %in% c("VS", "EG", "XP")) {
    columns <- .cdisc_finding_columns(name)
    result <- character_type(columns)
    result[[paste0(name, "SEQ")]] <- "integer"
    result[[paste0(name, "STRESN")]] <- "numeric"
    result[["VISITNUM"]] <- "numeric"
    return(result)
  }
  if (identical(name, "ADSL")) {
    result <- character_type(.cdisc_adsl_columns)
    result[["TRT01PN"]] <- "numeric"
    return(result)
  }
  if (identical(name, "ADBDS")) {
    result <- character_type(.cdisc_adbds_columns)
    for (column in c("AVISITN", "BASE", "CHG", "AVAL")) {
      result[[column]] <- "numeric"
    }
    return(result)
  }
  character(0)
}

.cdisc_has_type <- function(value, expected) {
  switch(
    expected,
    character = is.character(value),
    integer = is.integer(value),
    numeric = is.numeric(value),
    FALSE
  )
}

.cdisc_key <- function(data, columns) {
  if (!all(columns %in% names(data))) {
    return(NULL)
  }
  values <- lapply(data[columns], function(value) {
    value <- as.character(value)
    value[is.na(value)] <- "<MISSING>"
    value
  })
  do.call(paste, c(values, sep = "\r"))
}

.cdisc_is_iso_date <- function(x) {
  value <- .cdisc_character(x)
  missing <- is.na(value)
  syntax <- missing | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)
  parsed <- rep(as.Date(NA), length(value))
  index <- !missing & syntax
  if (any(index)) {
    parsed[index] <- suppressWarnings(as.Date(value[index]))
  }
  missing | (syntax & !is.na(parsed))
}

.cdisc_is_iso_datetime <- function(x) {
  value <- .cdisc_character(x)
  is.na(value) | grepl(
    paste0(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
      "[0-9]{2}:[0-9]{2}:[0-9]{2}",
      "(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})$"
    ),
    value
  )
}

#' Validate a CDISC-Shaped Export
#'
#' Checks the documented structural contract for exports created by
#' [toSDTM()] or [toADaM()]. A clean report does not mean an external
#' conformance product or regulator has accepted the data.
#'
#' @param x An `sdtm_export` or `adam_export`.
#' @param controlled_terms Optional named list, or a data frame with
#'   `variable` and `value`, adding caller-controlled terminology checks.
#' @return A `cdisc_validation` object with deterministic issues and summary.
#' @examples
#' trial <- Trial("T1", c("A", "B"))
#' participants <- data.frame(id = c("p1", "p2"), arm = c("A", "B"))
#' validateCDISC(toSDTM(trial, participants))
#' @export
validateCDISC <- function(x, controlled_terms = NULL) {
  if (!inherits(x, "sdtm_export") && !inherits(x, "adam_export")) {
    stop("x must be an sdtm_export or adam_export.", call. = FALSE)
  }
  if (!is.list(x) ||
      !all(c(
        "datasets", "manifest", "study_id", "standard", "metadata"
      ) %in% names(x)) ||
      !is.list(x$datasets) || !is.data.frame(x$manifest) ||
      !.is_scalar_string(x$study_id) ||
      !x$standard %in% c("SDTM-shaped", "ADaM-shaped")) {
    stop("x has an unreadable export shape.", call. = FALSE)
  }
  caller_terms <- .cdisc_controlled_terms(controlled_terms)
  issues <- .cdisc_issue_table()
  add_issue <- function(
    dataset,
    severity,
    rule_id,
    variable = NA_character_,
    row = NA_integer_,
    message
  ) {
    issues[nrow(issues) + 1L, ] <<- data.frame(
      dataset = as.character(dataset),
      severity = as.character(severity),
      rule_id = as.character(rule_id),
      variable = as.character(variable),
      row = as.integer(row),
      message = as.character(message),
      stringsAsFactors = FALSE
    )
  }
  dataset_names <- names(x$datasets)
  if (is.null(dataset_names) || anyNA(dataset_names) ||
      any(!nzchar(dataset_names)) || anyDuplicated(dataset_names)) {
    stop("x$datasets must have unique non-empty names.", call. = FALSE)
  }
  required_datasets <- if (identical(x$standard, "SDTM-shaped")) {
    c("DM", "AE")
  } else {
    c("ADSL", "ADBDS")
  }
  for (name in setdiff(required_datasets, dataset_names)) {
    add_issue(
      name, "error", "REQUIRED_DATASET",
      message = paste("Required dataset", name, "is absent.")
    )
  }

  manifest_required <- c(
    "dataset", "label", "class", "n_rows", "n_columns", "key"
  )
  if (!all(manifest_required %in% names(x$manifest))) {
    add_issue(
      "MANIFEST", "error", "MANIFEST_SCHEMA",
      message = "The manifest is missing required columns."
    )
  } else {
    if (!identical(.cdisc_character(x$manifest$dataset), dataset_names)) {
      add_issue(
        "MANIFEST", "error", "MANIFEST_DATASET",
        message = "Manifest dataset order does not match datasets."
      )
    }
  }

  link_subjects <- character(0)
  if ("DM" %in% dataset_names && is.data.frame(x$datasets$DM) &&
      "USUBJID" %in% names(x$datasets$DM)) {
    link_subjects <- .cdisc_character(x$datasets$DM$USUBJID)
  }
  if ("ADSL" %in% dataset_names && is.data.frame(x$datasets$ADSL) &&
      "USUBJID" %in% names(x$datasets$ADSL)) {
    link_subjects <- .cdisc_character(x$datasets$ADSL$USUBJID)
  }

  for (dataset_index in seq_along(x$datasets)) {
    name <- dataset_names[[dataset_index]]
    data <- x$datasets[[dataset_index]]
    if (!is.data.frame(data)) {
      add_issue(
        name, "error", "DATASET_TYPE",
        message = "Dataset is not a data.frame."
      )
      next
    }
    if (anyDuplicated(names(data))) {
      add_issue(
        name, "error", "VARIABLE_NAMES",
        message = "Dataset has duplicated variable names."
      )
    }
    expected <- .cdisc_expected_columns(name, x$standard)
    if (!is.null(expected)) {
      if (identical(name, "ADSL")) {
        correct <- length(names(data)) >= length(expected) &&
          identical(names(data)[seq_along(expected)], expected)
      } else {
        correct <- identical(names(data), expected)
      }
      if (!correct) {
        add_issue(
          name, "error", "REQUIRED_VARIABLES",
          message = "Required variables or their fixed order are incorrect."
        )
      }
    }
    expected_types <- .cdisc_expected_types(name)
    for (variable in intersect(names(expected_types), names(data))) {
      if (!.cdisc_has_type(data[[variable]], expected_types[[variable]])) {
        add_issue(
          name, "error", "VARIABLE_TYPE", variable,
          message = paste0(
            variable,
            " must have ",
            expected_types[[variable]],
            " storage."
          )
        )
      }
    }
    if (all(c("dataset", "n_rows", "n_columns") %in%
            names(x$manifest))) {
      manifest_row <- match(name, .cdisc_character(x$manifest$dataset))
      if (is.na(manifest_row) ||
          !identical(as.integer(x$manifest$n_rows[[manifest_row]]),
                     as.integer(nrow(data))) ||
          !identical(as.integer(x$manifest$n_columns[[manifest_row]]),
                     as.integer(ncol(data)))) {
        add_issue(
          name, "error", "MANIFEST_COUNTS",
          message = "Manifest row or column counts do not match."
        )
      }
    }
    numeric_columns <- vapply(data, is.numeric, logical(1))
    for (variable in names(data)[numeric_columns]) {
      bad <- which(!is.na(data[[variable]]) &
                     !is.finite(data[[variable]]))
      for (row in bad) {
        add_issue(
          name, "error", "NONFINITE_NUMERIC", variable, row,
          "Numeric values must be finite when non-missing."
        )
      }
    }
    character_columns <- vapply(
      data,
      function(value) is.character(value) || is.factor(value),
      logical(1)
    )
    for (variable in names(data)[character_columns]) {
      value <- .cdisc_character(data[[variable]])
      bad <- which(!is.na(value) & value %in% c("NA", "NaN", "Inf"))
      for (row in bad) {
        add_issue(
          name, "error", "TEXTUAL_MISSING", variable, row,
          "Missing or non-finite values must not be emitted as text."
        )
      }
    }
    if ("STUDYID" %in% names(data)) {
      value <- .cdisc_character(data$STUDYID)
      bad <- which(is.na(value) | value != x$study_id)
      for (row in bad) {
        add_issue(
          name, "error", "STUDYID_CONSTANT", "STUDYID", row,
          "STUDYID must equal the export study_id."
        )
      }
    } else {
      add_issue(
        name, "error", "REQUIRED_VARIABLES", "STUDYID",
        message = "STUDYID is required."
      )
    }
    if (identical(x$standard, "SDTM-shaped")) {
      if (!"DOMAIN" %in% names(data)) {
        add_issue(
          name, "error", "REQUIRED_VARIABLES", "DOMAIN",
          message = "DOMAIN is required."
        )
      } else {
        value <- .cdisc_character(data$DOMAIN)
        bad <- which(is.na(value) | value != name)
        for (row in bad) {
          add_issue(
            name, "error", "DOMAIN_CONSTANT", "DOMAIN", row,
            "DOMAIN must equal the dataset name."
          )
        }
      }
    }
    if ("USUBJID" %in% names(data)) {
      value <- .cdisc_character(data$USUBJID)
      if (name %in% c("DM", "ADSL")) {
        bad <- which(is.na(value) | !nzchar(value))
      } else {
        bad <- which(is.na(value) | !value %in% link_subjects)
      }
      for (row in bad) {
        add_issue(
          name, "error", "SUBJECT_LINK", "USUBJID", row,
          "USUBJID is missing or does not link to the subject dataset."
        )
      }
    } else {
      add_issue(
        name, "error", "REQUIRED_VARIABLES", "USUBJID",
        message = "USUBJID is required."
      )
    }

    key_columns <- if (identical(name, "DM") ||
                       identical(name, "ADSL")) {
      "USUBJID"
    } else if (identical(name, "AE")) {
      c("USUBJID", "AESEQ")
    } else if (name %in% c("VS", "EG", "XP")) {
      c("USUBJID", paste0(name, "SEQ"))
    } else if (identical(name, "ADBDS")) {
      c("USUBJID", "PARAMCD", "AVISITN", "ADT")
    } else {
      character(0)
    }
    key <- .cdisc_key(data, key_columns)
    if (length(key)) {
      duplicate <- which(duplicated(key) | duplicated(key, fromLast = TRUE))
      for (row in duplicate) {
        add_issue(
          name, "error", "DUPLICATE_KEY",
          paste(key_columns, collapse = ","), row,
          "Dataset key is duplicated."
        )
      }
    }
    sequence_name <- if (identical(name, "AE")) {
      "AESEQ"
    } else if (name %in% c("VS", "EG", "XP")) {
      paste0(name, "SEQ")
    } else {
      NULL
    }
    if (!is.null(sequence_name) &&
        all(c("USUBJID", sequence_name) %in% names(data))) {
      sequence <- data[[sequence_name]]
      bad_type <- !is.integer(sequence) || anyNA(sequence) ||
        any(sequence <= 0L)
      if (bad_type) {
        add_issue(
          name, "error", "SEQUENCE", sequence_name,
          message = "Sequence must contain positive non-missing integers."
        )
      } else {
        groups <- split(sequence, .cdisc_character(data$USUBJID), drop = TRUE)
        if (any(vapply(groups, function(value) {
          !identical(sort(value), seq_along(value))
        }, logical(1)))) {
          add_issue(
            name, "error", "SEQUENCE", sequence_name,
            message = "Sequence must be contiguous within each subject."
          )
        }
      }
    }

    code_name <- if (name %in% c("VS", "EG", "XP")) {
      paste0(name, "TESTCD")
    } else if (identical(name, "ADBDS")) {
      "PARAMCD"
    } else {
      NULL
    }
    label_name <- if (name %in% c("VS", "EG", "XP")) {
      paste0(name, "TEST")
    } else if (identical(name, "ADBDS")) {
      "PARAM"
    } else {
      NULL
    }
    if (!is.null(code_name) &&
        all(c(code_name, label_name) %in% names(data))) {
      code <- .cdisc_character(data[[code_name]])
      label <- .cdisc_character(data[[label_name]])
      bad <- which(
        is.na(code) | !grepl("^[A-Z][A-Z0-9_]{0,7}$", code)
      )
      for (row in bad) {
        add_issue(
          name, "error", "TEST_CODE", code_name, row,
          "Test or parameter code is invalid."
        )
      }
      pairs <- unique(data.frame(code = code, label = label))
      conflicting <- unique(pairs$code[duplicated(pairs$code)])
      if (length(conflicting)) {
        add_issue(
          name, "error", "TEST_NAME", label_name,
          message = "A code maps to multiple labels."
        )
      }
    }

    date_columns <- names(data)[
      grepl("DTC$", names(data)) | names(data) == "ADT"
    ]
    for (variable in date_columns) {
      value <- data[[variable]]
      valid <- if (
        name %in% c("VS", "EG", "XP") &&
        identical(variable, paste0(name, "DTC"))
      ) {
        .cdisc_datetime_valid(value)
      } else if (
        grepl("DTC$", variable) &&
        !variable %in% c(
          "RFSTDTC", "RFENDTC", "BRTHDTC",
          "AESTDTC", "AEENDTC"
        )
      ) {
        .cdisc_is_iso_date(value) | .cdisc_datetime_valid(value)
      } else {
        .cdisc_is_iso_date(value)
      }
      for (row in which(!valid)) {
        add_issue(
          name, "error", "ISO_DATE", variable, row,
          "Date or date-time value is not valid ISO 8601."
        )
      }
    }

    default_terms <- list(
      SEX = c("M", "F", "U", "UNDIFFERENTIATED"),
      AESER = c("Y", "N"),
      AESEV = unname(.cdisc_severity_map),
      AEREL = unname(.cdisc_causality_map),
      AEOUT = unname(.cdisc_outcome_map),
      ITTFL = c("Y", "N"),
      PPROTFL = c("Y", "N"),
      ANL01FL = c("Y", "N")
    )
    terms <- c(default_terms, caller_terms)
    terms <- terms[!duplicated(names(terms), fromLast = TRUE)]
    for (variable in intersect(names(terms), names(data))) {
      value <- .cdisc_character(data[[variable]])
      bad <- which(!is.na(value) & !value %in% terms[[variable]])
      for (row in bad) {
        add_issue(
          name, "error", "CONTROLLED_TERM", variable, row,
          "Value is absent from the applicable controlled terms."
        )
      }
    }
    if ("AEACN" %in% names(data)) {
      action <- .cdisc_character(data$AEACN)
      sponsor <- which(
        !is.na(action) & !action %in% unname(.cdisc_action_map)
      )
      for (row in sponsor) {
        add_issue(
          name, "warning", "SPONSOR_TERM", "AEACN", row,
          "AE action uses sponsor-defined terminology."
        )
      }
    }
    if (identical(name, "XP")) {
      add_issue(
        name, "warning", "SPONSOR_DOMAIN",
        message = "XP is a sponsor-defined physiological findings domain."
      )
    }
    if (identical(name, "ADSL")) {
      for (variable in intersect(c("ITTFL", "PPROTFL"), names(data))) {
        if (nrow(data) && all(is.na(data[[variable]]))) {
          add_issue(
            name, "warning", "POPULATION_FLAG_MISSING", variable,
            message = paste(variable, "was not supplied by an analysis set.")
          )
        }
      }
    }
  }
  if (nrow(issues)) {
    issues <- issues[order(
      issues$dataset,
      match(issues$severity, c("error", "warning")),
      issues$rule_id,
      issues$variable,
      issues$row,
      na.last = TRUE,
      method = "radix"
    ), , drop = FALSE]
    rownames(issues) <- NULL
  }
  summary <- data.frame(
    severity = c("error", "warning"),
    n = c(
      sum(issues$severity == "error"),
      sum(issues$severity == "warning")
    ),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      valid = !any(issues$severity == "error"),
      issues = issues,
      summary = summary,
      checked = x$manifest
    ),
    class = "cdisc_validation"
  )
}

#' @export
as.data.frame.cdisc_validation <- function(x, ...) {
  x$issues
}

#' @export
print.cdisc_validation <- function(x, ...) {
  cat(
    "<cdisc_validation> ",
    if (isTRUE(x$valid)) "valid" else "invalid",
    ": ",
    x$summary$n[x$summary$severity == "error"],
    " error(s), ",
    x$summary$n[x$summary$severity == "warning"],
    " warning(s)\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.sdtm_export <- function(x, ...) {
  cat(
    "<sdtm_export> ",
    x$study_id,
    " (",
    paste(names(x$datasets), collapse = ", "),
    "; structural SDTM-shaped output)\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.adam_export <- function(x, ...) {
  cat(
    "<adam_export> ",
    x$study_id,
    " (",
    paste(names(x$datasets), collapse = ", "),
    "; structural ADaM-shaped output)\n",
    sep = ""
  )
  invisible(x)
}

.cdisc_xml_check <- function(value, name) {
  value <- enc2utf8(as.character(value))
  invalid <- grepl("[\x01-\x08\x0B\x0C\x0E-\x1F]", value)
  if (any(invalid, na.rm = TRUE)) {
    stop(name, " contains an invalid XML control character.",
         call. = FALSE)
  }
  value
}

.cdisc_xml_escape <- function(value) {
  value <- .cdisc_xml_check(value, "XML text")
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub("\"", "&quot;", value, fixed = TRUE)
  gsub("'", "&apos;", value, fixed = TRUE)
}

.cdisc_xml_type <- function(value) {
  if (inherits(value, "Date")) {
    return("date")
  }
  if (inherits(value, "POSIXct")) {
    return("datetime")
  }
  if (is.integer(value)) {
    return("integer")
  }
  if (is.numeric(value)) {
    return("float")
  }
  "text"
}

.cdisc_xml_length <- function(value) {
  if (!is.character(value) && !is.factor(value)) {
    return(NULL)
  }
  value <- as.character(value)
  value <- value[!is.na(value)]
  if (!length(value)) {
    return(1L)
  }
  max(1L, max(nchar(value, type = "chars")))
}

#' Create a Deterministic define.xml Metadata Stub
#'
#' Serializes metadata for the exact datasets in an SDTM-shaped or ADaM-shaped
#' export. The result is a deterministic ODM metadata stub, not a
#' submission-ready define.xml document.
#'
#' @param x An `sdtm_export` or `adam_export`.
#' @param file Optional output path. Writing uses a temporary file followed by
#'   an atomic rename.
#' @param study_name,protocol_name Optional display metadata.
#' @param standard_version Optional standard-version label.
#' @return Invisibly, the XML string when `file` is `NULL`; otherwise the
#'   normalized written path.
#' @examples
#' trial <- Trial("T1", c("A", "B"))
#' participants <- data.frame(id = c("p1", "p2"), arm = c("A", "B"))
#' xml <- defineXML(toSDTM(trial, participants))
#' grepl("ItemGroupDef", xml, fixed = TRUE)
#' @export
defineXML <- function(
  x,
  file = NULL,
  study_name = NULL,
  protocol_name = NULL,
  standard_version = NULL
) {
  if (!inherits(x, "sdtm_export") && !inherits(x, "adam_export")) {
    stop("x must be an sdtm_export or adam_export.", call. = FALSE)
  }
  validation <- validateCDISC(x)
  if (!validation$valid) {
    stop("x must pass structural validation before defineXML().",
         call. = FALSE)
  }
  study_name <- .cdisc_scalar_string(
    study_name %||% x$study_id,
    "study_name"
  )
  protocol_name <- .cdisc_scalar_string(
    protocol_name %||% x$study_id,
    "protocol_name"
  )
  standard_version <- .cdisc_scalar_string(
    standard_version %||% "unspecified",
    "standard_version"
  )
  for (name in names(x$datasets)) {
    data <- x$datasets[[name]]
    character_columns <- vapply(
      data,
      function(value) is.character(value) || is.factor(value),
      logical(1)
    )
    for (variable in names(data)[character_columns]) {
      .cdisc_xml_check(
        data[[variable]],
        paste0(name, "$", variable)
      )
    }
  }
  escaped <- .cdisc_xml_escape
  standard <- x$standard
  file_oid <- paste0(
    "PHY.",
    substr(digest::digest(
      list(
        study_id = x$study_id,
        standard = standard,
        standard_version = standard_version,
        datasets = names(x$datasets)
      ),
      algo = "sha256"
    ), 1L, 24L)
  )
  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    paste0(
      '<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" ',
      'ODMVersion="1.3.2" FileType="Snapshot" FileOID="',
      escaped(file_oid),
      '">'
    ),
    paste0(
      '  <Study OID="STUDY.',
      escaped(x$study_id),
      '">'
    ),
    "    <GlobalVariables>",
    paste0("      <StudyName>", escaped(study_name), "</StudyName>"),
    paste0("      <StudyDescription>", escaped(standard),
           " metadata stub</StudyDescription>"),
    paste0("      <ProtocolName>", escaped(protocol_name),
           "</ProtocolName>"),
    "    </GlobalVariables>",
    paste0(
      '    <MetaDataVersion OID="MDV.1" Name="',
      escaped(standard),
      '" Description="Deterministic structural metadata stub">'
    )
  )
  item_defs <- character(0)
  manifest <- x$manifest
  for (dataset_index in seq_along(x$datasets)) {
    name <- names(x$datasets)[[dataset_index]]
    data <- x$datasets[[dataset_index]]
    manifest_index <- match(name, manifest$dataset)
    label <- manifest$label[[manifest_index]]
    class_name <- manifest$class[[manifest_index]]
    key <- strsplit(
      manifest$key[[manifest_index]],
      ",",
      fixed = TRUE
    )[[1L]]
    key <- trimws(key)
    repeating <- if (name %in% c("DM", "ADSL")) "No" else "Yes"
    purpose <- if (identical(standard, "ADaM-shaped")) {
      "Analysis"
    } else {
      "Tabulation"
    }
    lines <- c(
      lines,
      paste0(
        '      <ItemGroupDef OID="IG.',
        escaped(name),
        '" Name="',
        escaped(name),
        '" Repeating="',
        repeating,
        '" IsReferenceData="No" Purpose="',
        purpose,
        '">'
      ),
      paste0(
        "        <Description><TranslatedText xml:lang=\"en\">",
        escaped(label),
        "</TranslatedText></Description>"
      )
    )
    for (column_index in seq_along(data)) {
      variable <- names(data)[[column_index]]
      key_sequence <- match(variable, key)
      key_attribute <- if (is.na(key_sequence)) {
        ""
      } else {
        paste0(' KeySequence="', key_sequence, '"')
      }
      lines <- c(
        lines,
        paste0(
          '        <ItemRef ItemOID="IT.',
          escaped(name),
          ".",
          escaped(variable),
          '" OrderNumber="',
          column_index,
          '" Mandatory="No"',
          key_attribute,
          "/>"
        )
      )
      length_value <- .cdisc_xml_length(data[[variable]])
      length_attribute <- if (is.null(length_value)) {
        ""
      } else {
        paste0(' Length="', length_value, '"')
      }
      item_defs <- c(
        item_defs,
        paste0(
          '      <ItemDef OID="IT.',
          escaped(name),
          ".",
          escaped(variable),
          '" Name="',
          escaped(variable),
          '" DataType="',
          .cdisc_xml_type(data[[variable]]),
          '"',
          length_attribute,
          ">"
        ),
        paste0(
          "        <Description><TranslatedText xml:lang=\"en\">",
          escaped(variable),
          "</TranslatedText></Description>"
        ),
        "      </ItemDef>"
      )
    }
    alias_context <- if (
      "sponsor_defined" %in% names(manifest) &&
      isTRUE(manifest$sponsor_defined[[manifest_index]])
    ) {
      "SponsorDefined"
    } else {
      "Standard"
    }
    lines <- c(
      lines,
      paste0(
        '        <Alias Context="',
        alias_context,
        '" Name="',
        escaped(class_name),
        '"/>'
      ),
      "      </ItemGroupDef>"
    )
  }
  lines <- c(
    lines,
    item_defs,
    paste0(
      '      <Alias Context="StandardVersion" Name="',
      escaped(standard_version),
      '"/>'
    ),
    "    </MetaDataVersion>",
    "  </Study>",
    "</ODM>"
  )
  xml <- enc2utf8(paste(lines, collapse = "\n"))
  if (is.null(file)) {
    return(invisible(xml))
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !nzchar(file)) {
    stop("file must be one non-empty path.", call. = FALSE)
  }
  directory <- dirname(file)
  if (!dir.exists(directory)) {
    stop("The output directory does not exist.", call. = FALSE)
  }
  temporary <- tempfile(".define-", tmpdir = directory)
  connection <- file(temporary, open = "wb")
  on.exit({
    try(close(connection), silent = TRUE)
    if (file.exists(temporary)) {
      unlink(temporary)
    }
  }, add = TRUE)
  writeBin(charToRaw(xml), connection)
  close(connection)
  if (!file.rename(temporary, file)) {
    stop("Could not atomically replace the define.xml output.",
         call. = FALSE)
  }
  invisible(normalizePath(file, mustWork = TRUE))
}
