make_cdisc_trial <- function() {
  Trial("TRIAL01", c("active", "control"))
}

make_cdisc_participants <- function() {
  data.frame(
    id = c(paste0("p", 1:8), "screen"),
    arm = c(rep("active", 4), rep("control", 4), NA),
    randomized = c(rep(TRUE, 8), FALSE),
    adherent = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, NA),
    sex = c("F", "M", "F", "U", "M", "F", "M", "F", "U"),
    birth = as.Date("1980-01-01") + seq_len(9),
    start = as.Date("2026-01-01") + c(0:7, NA),
    end = as.Date("2026-03-01") + c(0:7, NA),
    country = rep("JPN", 9),
    site = rep(c("01", "02", "03"), 3),
    stringsAsFactors = FALSE
  )
}

make_cdisc_ae <- function() {
  date <- as.Date("2026-01-01")
  list(
    adverseEvent("p1", "Nausea", date, ctcae_grade = 1),
    adverseEvent(
      "p1", "Infusion reaction", date + 1,
      ctcae_grade = 2, serious = TRUE,
      causality = "probable", action = "dose interrupted",
      meddra_pt = "Infusion related reaction",
      meddra_code = "10051792", meddra_version = "28.0",
      metadata = list(meddra_soc = "General disorders")
    ),
    adverseEvent(
      "p2", "Fatigue", date + 2,
      ctcae_grade = 3, serious = TRUE
    ),
    adverseEvent("p5", "Nausea", date, ctcae_grade = 2),
    adverseEvent("p6", "Nausea", date + 1, ctcae_grade = 2),
    adverseEvent(
      "p6", "Hypotension", date + 2,
      ctcae_grade = 4, serious = TRUE
    )
  )
}

make_cdisc_finding <- function(code, label, offset = 0) {
  data.frame(
    participant_id = rep(c("p1", "p5"), each = 2),
    test_code = rep(code, 4),
    test_name = rep(label, 4),
    original_result = as.character(c(70, 72, 68, 69) + offset),
    original_unit = rep("unit", 4),
    standard_result = c(70, 72, 68, 69) + offset,
    standard_unit = rep("unit", 4),
    date_time = rep(
      c("2026-01-02T09:00:00+09:00", "2026-02-02T09:00:00+09:00"),
      2
    ),
    visit = rep(c("Baseline", "Week 4"), 2),
    visit_number = rep(c(1, 2), 2),
    stringsAsFactors = FALSE
  )
}

make_cdisc_sdtm <- function(include_xp = TRUE) {
  trial <- make_cdisc_trial()
  participants <- make_cdisc_participants()
  findings <- list(
    VS = make_cdisc_finding("PULSE", "Pulse Rate"),
    EG = make_cdisc_finding("HR", "Heart Rate", 1)
  )
  if (include_xp) {
    findings$XP <- make_cdisc_finding("SWAY", "Postural Sway", 2)
  }
  toSDTM(
    trial,
    participants,
    adverse_events = make_cdisc_ae(),
    findings = findings,
    sex_col = "sex",
    birth_date_col = "birth",
    reference_start_col = "start",
    reference_end_col = "end",
    country_col = "country",
    site_col = "site"
  )
}

test_that("toSDTM creates exact DM and AE linkage", {
  export <- make_cdisc_sdtm()
  expect_s3_class(export, "sdtm_export")
  expect_identical(export$standard, "SDTM-shaped")
  expect_identical(names(export$datasets), c("DM", "AE", "VS", "EG", "XP"))

  dm <- export$datasets$DM
  ae <- export$datasets$AE
  expect_identical(names(dm), PhysioTrial:::.cdisc_dm_columns)
  expect_identical(names(ae), PhysioTrial:::.cdisc_ae_columns)
  expect_equal(nrow(dm), 9L)
  expect_identical(anyDuplicated(dm$USUBJID), 0L)
  expect_identical(dm$SUBJID, sort(make_cdisc_participants()$id))
  expect_identical(
    dm$ARMCD[match(c("p1", "p5", "screen"), dm$SUBJID)],
    c("ARM1", "ARM2", NA_character_)
  )
  expect_equal(nrow(ae), 6L)
  expect_true(all(ae$USUBJID %in% dm$USUBJID))
  expect_true(all(vapply(
    split(ae$AESEQ, ae$USUBJID),
    function(value) identical(value, seq_along(value)),
    logical(1)
  )))
  grade_two_sae <- ae[
    ae$AETERM == "Infusion reaction",
    ,
    drop = FALSE
  ]
  expect_identical(grade_two_sae$AESEV, "MODERATE")
  expect_identical(grade_two_sae$AESER, "Y")
  expect_identical(grade_two_sae$AEACN, "DRUG INTERRUPTED")
  expect_identical(grade_two_sae$AEBODSYS, "General disorders")
  expect_true(all(is.na(ae$AEDECOD[ae$AETERM == "Nausea"])))
  expect_true(any(
    export$metadata$sponsor_defined$meddra$code == "10051792",
    na.rm = TRUE
  ))
})

test_that("SDTM identities and sequences do not depend on input row order", {
  trial <- make_cdisc_trial()
  participants <- make_cdisc_participants()
  events <- make_cdisc_ae()
  findings <- list(VS = make_cdisc_finding("PULSE", "Pulse Rate"))
  reference <- toSDTM(
    trial,
    participants,
    adverse_events = events,
    findings = findings,
    sex_col = "sex"
  )
  shuffled <- toSDTM(
    trial,
    participants[c(9, 4, 2, 8, 1, 7, 3, 6, 5), ],
    adverse_events = events[c(6, 2, 4, 1, 5, 3)],
    findings = list(VS = findings$VS[c(4, 2, 1, 3), ]),
    sex_col = "sex"
  )
  expect_identical(reference$datasets$DM, shuffled$datasets$DM)
  expect_identical(reference$datasets$AE, shuffled$datasets$AE)
  expect_identical(reference$datasets$VS, shuffled$datasets$VS)
  expect_identical(reference$metadata$id_map, shuffled$metadata$id_map)
})

test_that("findings retain strict types, offsets, and sponsor status", {
  export <- make_cdisc_sdtm()
  for (domain in c("VS", "EG", "XP")) {
    data <- export$datasets[[domain]]
    expect_identical(
      names(data),
      PhysioTrial:::.cdisc_finding_columns(domain)
    )
    expect_type(data[[paste0(domain, "SEQ")]], "integer")
    expect_type(data[[paste0(domain, "STRESN")]], "double")
    expect_true(all(grepl("\\+09:00$", data[[paste0(domain, "DTC")]])))
    expect_true(all(is.finite(data[[paste0(domain, "STRESN")]])))
  }
  xp_manifest <- export$manifest[export$manifest$dataset == "XP", ]
  expect_true(xp_manifest$sponsor_defined)

  empty <- make_cdisc_finding("PULSE", "Pulse Rate")[0, ]
  empty_export <- toSDTM(
    make_cdisc_trial(),
    make_cdisc_participants(),
    findings = list(VS = empty)
  )
  expect_equal(nrow(empty_export$datasets$VS), 0L)
  expect_type(empty_export$datasets$VS$VSSEQ, "integer")
  expect_type(empty_export$datasets$VS$VSSTRESN, "double")
})

test_that("SDTM source validation rejects ambiguous domain data", {
  participants <- make_cdisc_participants()
  duplicated <- rbind(participants, participants[1, ])
  expect_error(
    toSDTM(make_cdisc_trial(), duplicated),
    "unique"
  )

  unknown_arm <- participants
  unknown_arm$arm[[1L]] <- "other"
  expect_error(
    toSDTM(make_cdisc_trial(), unknown_arm),
    "unknown"
  )

  finding <- make_cdisc_finding("PULSE", "Pulse Rate")
  finding$test_code[[1L]] <- "lower"
  expect_error(
    toSDTM(
      make_cdisc_trial(),
      participants,
      findings = list(VS = finding)
    ),
    "uppercase"
  )
  finding <- make_cdisc_finding("PULSE", "Pulse Rate")
  finding$test_name[[1L]] <- "Different Name"
  expect_error(
    toSDTM(
      make_cdisc_trial(),
      participants,
      findings = list(VS = finding)
    ),
    "exactly one"
  )
  finding <- make_cdisc_finding("PULSE", "Pulse Rate")
  finding$participant_id[[1L]] <- "unknown"
  expect_error(
    toSDTM(
      make_cdisc_trial(),
      participants,
      findings = list(VS = finding)
    ),
    "absent"
  )
  finding <- make_cdisc_finding("PULSE", "Pulse Rate")
  finding$standard_result[[1L]] <- Inf
  expect_error(
    toSDTM(
      make_cdisc_trial(),
      participants,
      findings = list(VS = finding)
    ),
    "finite"
  )
  finding <- make_cdisc_finding("PULSE", "Pulse Rate")
  finding$date_time[[1L]] <- "2026-01-01 00:00:00"
  expect_error(
    toSDTM(
      make_cdisc_trial(),
      participants,
      findings = list(VS = finding)
    ),
    "explicit offset"
  )
  finding <- make_cdisc_finding("PULSE", "Pulse Rate")
  finding$date_time[[1L]] <- "2026-02-30T25:61:61+15:30"
  expect_error(
    toSDTM(
      make_cdisc_trial(),
      participants,
      findings = list(VS = finding)
    ),
    "explicit offset"
  )
})

test_that("constructed QS domains pass through without reimplementation", {
  participants <- make_cdisc_participants()
  qs <- data.frame(
    STUDYID = "TRIAL01",
    DOMAIN = "QS",
    USUBJID = "TRIAL01-p1",
    QSSEQ = 1L,
    QSTESTCD = "SCORE",
    QSTEST = "Clinical Score",
    QSCAT = "SPONSOR",
    QSORRES = "10",
    QSORRESU = NA_character_,
    QSSTRESC = "10",
    QSSTRESN = 10,
    QSSTRESU = NA_character_,
    VISITNUM = 1,
    VISIT = "Baseline",
    QSDTC = "2026-01-01T09:00:00+09:00",
    stringsAsFactors = FALSE
  )
  export <- toSDTM(
    make_cdisc_trial(),
    participants,
    extra_domains = list(QS = qs)
  )
  expect_identical(export$datasets$QS, qs)
  expect_true(validateCDISC(export)$valid)
  shuffled_qs <- rbind(
    transform(qs, QSSEQ = 2L, QSTESTCD = "SCORE2"),
    qs
  )
  ordered <- toSDTM(
    make_cdisc_trial(),
    participants,
    extra_domains = list(QS = shuffled_qs)
  )
  reversed <- toSDTM(
    make_cdisc_trial(),
    participants,
    extra_domains = list(QS = shuffled_qs[2:1, ])
  )
  expect_identical(ordered$datasets$QS, reversed$datasets$QS)
  expect_error(
    toSDTM(
      make_cdisc_trial(),
      participants,
      findings = list(VS = make_cdisc_finding("PULSE", "Pulse Rate")),
      extra_domains = list(VS = qs)
    ),
    "collides"
  )
})

make_cdisc_sets <- function() {
  trial <- make_cdisc_trial()
  participants <- make_cdisc_participants()
  list(
    participants = participants,
    itt = intentionToTreat(trial, participants),
    pp = perProtocol(trial, participants, adherent_col = "adherent")
  )
}

make_cdisc_bds <- function() {
  data.frame(
    participant_id = rep(c("p1", "p2", "p5"), each = 2),
    param_code = rep(c("MOBILITY", "BALANCE", "MOBILITY"), each = 2),
    param_label = rep(c("Mobility", "Balance", "Mobility"), each = 2),
    value = c(10, 13, 20, 22, 30, 34),
    analysis_date = rep(
      as.Date(c("2026-01-01", "2026-02-01")),
      3
    ),
    visit = rep(c("Baseline", "Week 4"), 3),
    visit_number = rep(c(1, 2), 3),
    baseline_value = c(10, 10, 20, 20, 30, 30),
    change_value = c(0, 3, 0, 2, 0, 4),
    analysis_value = c(10, 13, 20, 22, 30, 34),
    stringsAsFactors = FALSE
  )
}

test_that("toADaM preserves population flags and BDS arithmetic", {
  fixture <- make_cdisc_sets()
  export <- toADaM(
    fixture$itt,
    fixture$participants,
    bds = make_cdisc_bds(),
    population_flags = list(PP = fixture$pp)
  )
  expect_s3_class(export, "adam_export")
  expect_identical(export$standard, "ADaM-shaped")
  adsl <- export$datasets$ADSL
  adbds <- export$datasets$ADBDS
  expect_identical(
    names(adsl)[seq_along(PhysioTrial:::.cdisc_adsl_columns)],
    PhysioTrial:::.cdisc_adsl_columns
  )
  expect_identical(names(adbds), PhysioTrial:::.cdisc_adbds_columns)
  expect_equal(sum(adsl$ITTFL == "Y"), 8L)
  expect_equal(sum(adsl$PPROTFL == "Y"), 6L)
  expect_true(all(is.na(adsl$TRT01P[adsl$SUBJID == "screen"])))
  expect_identical(
    adsl$TRT01PN[match(c("p1", "p5"), adsl$SUBJID)],
    c(1, 2)
  )
  expect_true("randomized" %in% names(adsl))
  expect_equal(adbds$CHG, adbds$AVAL - adbds$BASE)
  expect_identical(
    adbds$ANL01FL,
    ifelse(
      sub("^TRIAL01-", "", adbds$USUBJID) %in%
        fixture$itt$members$participant_id,
      "Y",
      "N"
    )
  )
  expect_identical(
    export$metadata$analysis_exclusions$PP,
    fixture$pp$excluded
  )
  expect_true(validateCDISC(export)$valid)
})

test_that("ADaM output is deterministic and accepts linked ADQS", {
  fixture <- make_cdisc_sets()
  bds <- make_cdisc_bds()
  direct <- toADaM(
    fixture$pp,
    fixture$participants,
    bds = bds,
    population_flags = list(ITT = fixture$itt)
  )
  shuffled <- toADaM(
    fixture$pp,
    fixture$participants[c(9, 7, 5, 3, 1, 8, 6, 4, 2), ],
    bds = bds[c(6, 2, 4, 1, 5, 3), ],
    population_flags = list(ITT = fixture$itt)
  )
  expect_identical(direct$datasets$ADSL, shuffled$datasets$ADSL)
  expect_identical(direct$datasets$ADBDS, shuffled$datasets$ADBDS)

  adqs <- data.frame(
    STUDYID = "TRIAL01",
    USUBJID = "TRIAL01-p1",
    PARAMCD = "SCORE",
    PARAM = "Clinical Score",
    PARCAT1 = "SPONSOR",
    AVAL = 10,
    AVALC = "10",
    AVISIT = "Baseline",
    AVISITN = 1,
    ADT = "2026-01-01",
    stringsAsFactors = FALSE
  )
  with_adqs <- toADaM(
    fixture$itt,
    fixture$participants,
    bds = list(ADBDS = bds, ADQS = adqs),
    population_flags = list(PP = fixture$pp)
  )
  expect_identical(with_adqs$datasets$ADQS, adqs)
  expect_true(validateCDISC(with_adqs)$valid)

  empty_optional <- toADaM(
    fixture$itt,
    fixture$participants,
    bds = list(),
    population_flags = list()
  )
  expect_equal(nrow(empty_optional$datasets$ADBDS), 0L)
  expect_true(validateCDISC(empty_optional)$valid)
})

test_that("ADaM rejects inconsistent sets, keys, and changes", {
  fixture <- make_cdisc_sets()
  bds <- make_cdisc_bds()
  duplicated <- rbind(bds, bds[1, ])
  expect_error(
    toADaM(
      fixture$itt,
      fixture$participants,
      duplicated,
      population_flags = list(PP = fixture$pp)
    ),
    "unique"
  )
  inconsistent <- bds
  inconsistent$change_value[[2L]] <- 99
  expect_error(
    toADaM(
      fixture$itt,
      fixture$participants,
      inconsistent,
      population_flags = list(PP = fixture$pp)
    ),
    "must equal"
  )
  unknown <- bds
  unknown$participant_id[[1L]] <- "unknown"
  expect_error(
    toADaM(
      fixture$itt,
      fixture$participants,
      unknown,
      population_flags = list(PP = fixture$pp)
    ),
    "absent"
  )
  other <- fixture$itt
  other$trial_id <- "OTHER"
  expect_error(
    toADaM(
      fixture$itt,
      fixture$participants,
      bds,
      population_flags = list(ITT = other)
    ),
    "different trial_id"
  )
})

test_that("validateCDISC reports deterministic structural rule IDs", {
  valid <- make_cdisc_sdtm(include_xp = FALSE)
  expect_true(validateCDISC(valid)$valid)
  mutations <- list(
    REQUIRED_VARIABLES = function(x) {
      x$datasets$AE$AESEQ <- NULL
      x
    },
    DUPLICATE_KEY = function(x) {
      x$datasets$AE$AESEQ[[2L]] <- x$datasets$AE$AESEQ[[1L]]
      x$datasets$AE$USUBJID[[2L]] <- x$datasets$AE$USUBJID[[1L]]
      x
    },
    SUBJECT_LINK = function(x) {
      x$datasets$AE$USUBJID[[1L]] <- "TRIAL01-unknown"
      x
    },
    DOMAIN_CONSTANT = function(x) {
      x$datasets$AE$DOMAIN[[1L]] <- "VS"
      x
    },
    TEST_CODE = function(x) {
      x$datasets$VS$VSTESTCD[[1L]] <- "lower"
      x
    },
    ISO_DATE = function(x) {
      x$datasets$AE$AESTDTC[[1L]] <- "not-a-date"
      x
    },
    NONFINITE_NUMERIC = function(x) {
      x$datasets$VS$VSSTRESN[[1L]] <- Inf
      x
    },
    CONTROLLED_TERM = function(x) {
      x$datasets$DM$SEX[[1L]] <- "OTHER"
      x
    },
    VARIABLE_TYPE = function(x) {
      x$datasets$AE$AESEQ <- as.numeric(x$datasets$AE$AESEQ)
      x
    }
  )
  for (rule in names(mutations)) {
    report <- validateCDISC(mutations[[rule]](valid))
    expect_false(report$valid, info = rule)
    expect_true(rule %in% report$issues$rule_id, info = rule)
  }
  report <- validateCDISC(
    valid,
    controlled_terms = list(COUNTRY = "USA")
  )
  expect_false(report$valid)
  expect_true("CONTROLLED_TERM" %in% report$issues$rule_id)
  expect_s3_class(as.data.frame(report), "data.frame")
})

test_that("XP is a warning and defineXML is deterministic and escaped", {
  export <- make_cdisc_sdtm(include_xp = TRUE)
  report <- validateCDISC(export)
  expect_true(report$valid)
  expect_true("SPONSOR_DOMAIN" %in% report$issues$rule_id)
  xml_one <- defineXML(
    export,
    study_name = paste0("Trial & ", intToUtf8(29983L)),
    protocol_name = "Protocol <1>",
    standard_version = "3.4"
  )
  xml_two <- defineXML(
    export,
    study_name = paste0("Trial & ", intToUtf8(29983L)),
    protocol_name = "Protocol <1>",
    standard_version = "3.4"
  )
  expect_identical(xml_one, xml_two)
  expect_match(xml_one, "Trial &amp; ", fixed = TRUE)
  expect_match(xml_one, "Protocol &lt;1&gt;", fixed = TRUE)
  expect_match(xml_one, "ItemGroupDef", fixed = TRUE)
  expect_false(grepl("CreationDateTime", xml_one, fixed = TRUE))

  path <- tempfile(fileext = ".xml")
  returned <- defineXML(export, file = path)
  expect_identical(returned, normalizePath(path))
  expect_identical(readChar(path, file.info(path)$size, useBytes = TRUE),
                   defineXML(export))

  expect_error(
    defineXML(
      export,
      study_name = paste0("bad", intToUtf8(1L))
    ),
    "control character"
  )
})

test_that("print methods identify structural rather than certified output", {
  sdtm <- make_cdisc_sdtm(include_xp = FALSE)
  fixture <- make_cdisc_sets()
  adam <- toADaM(
    fixture$itt,
    fixture$participants,
    population_flags = list(PP = fixture$pp)
  )
  expect_output(print(sdtm), "structural SDTM-shaped")
  expect_output(print(adam), "structural ADaM-shaped")
  expect_output(print(validateCDISC(sdtm)), "valid")
})
