.resolve_seed <- function(seed) {
  if (is.null(seed)) {
    return(as.integer(sample.int(.Machine$integer.max, 1L)))
  }
  if (!.whole_number(seed) || length(seed) != 1L || seed < 1) {
    stop("seed must be one positive 31-bit integer.", call. = FALSE)
  }
  as.integer(seed)
}

.validate_block_sizes <- function(block_sizes, ratio) {
  ratio_sum <- sum(as.double(ratio))
  if (!is.finite(ratio_sum) || ratio_sum > .Machine$integer.max) {
    stop("The sum of allocation weights is too large.", call. = FALSE)
  }
  if (is.null(block_sizes)) {
    default <- 2 * ratio_sum
    if (default > .Machine$integer.max) {
      stop("The default block size exceeds the integer limit.", call. = FALSE)
    }
    block_sizes <- default
  }
  if (!is.numeric(block_sizes) || !length(block_sizes) ||
      !.whole_number(block_sizes) || any(block_sizes < 1) ||
      any(block_sizes %% ratio_sum != 0)) {
    stop("block_sizes must be positive integer multiples of sum(ratio).",
         call. = FALSE)
  }
  unique(as.integer(block_sizes))
}

.validate_factor_weights <- function(weights, factors) {
  if (!length(factors)) {
    if (!is.null(weights) && length(weights)) {
      stop("weights cannot be supplied for an unstratified trial.",
           call. = FALSE)
    }
    return(numeric(0))
  }
  weights <- weights %||% rep.int(1, length(factors))
  if (!is.numeric(weights) || length(weights) != length(factors) ||
      any(!is.finite(weights)) || any(weights <= 0)) {
    stop("weights must contain one finite positive value per factor.",
         call. = FALSE)
  }
  if (!is.null(names(weights))) {
    if (anyNA(names(weights)) || any(!nzchar(names(weights))) ||
        !setequal(names(weights), factors)) {
      stop("weights names must match the trial's strata factors.",
           call. = FALSE)
    }
    weights <- weights[factors]
  }
  stats::setNames(as.numeric(weights), factors)
}

.participant_frame <- function(trial, n, participants) {
  if (is.null(n) == is.null(participants)) {
    stop("Supply exactly one of n or participants.", call. = FALSE)
  }
  factors <- names(trial@strata)
  if (!is.null(n)) {
    if (!.whole_number(n) || length(n) != 1L || n < 1) {
      stop("n must be one positive integer.", call. = FALSE)
    }
    if (length(factors)) {
      stop("participants are required when the trial defines strata.",
           call. = FALSE)
    }
    return(data.frame(
      id = paste0("slot_", seq_len(as.integer(n))),
      stringsAsFactors = FALSE
    ))
  }

  if (is.list(participants) && !is.data.frame(participants)) {
    if (!length(participants) ||
        !all(vapply(participants, methods::is, logical(1), "Participant"))) {
      stop("participants must be a non-empty list of Participant objects.",
           call. = FALSE)
    }
    ids <- vapply(participants, methods::slot, character(1), "id")
    frame <- data.frame(id = ids, stringsAsFactors = FALSE)
    for (factor in factors) {
      frame[[factor]] <- vapply(participants, function(participant) {
        participant_strata <- participant@strata
        if (!factor %in% names(participant_strata)) {
          stop("Every participant must provide the '", factor,
               "' stratum.", call. = FALSE)
        }
        participant_strata[[factor]]
      }, character(1))
    }
    extra <- unique(unlist(lapply(
      participants,
      function(participant) setdiff(names(participant@strata), factors)
    )))
    if (length(extra)) {
      stop("Participant strata are not defined by this trial: ",
           paste(extra, collapse = ", "), call. = FALSE)
    }
  } else if (is.data.frame(participants)) {
    required <- c("id", factors)
    missing_columns <- setdiff(required, names(participants))
    if (length(missing_columns)) {
      stop("participants is missing required columns: ",
           paste(missing_columns, collapse = ", "), call. = FALSE)
    }
    if (!nrow(participants)) {
      stop("participants must contain at least one row.", call. = FALSE)
    }
    frame <- data.frame(
      id = as.character(participants$id),
      stringsAsFactors = FALSE
    )
    for (factor in factors) {
      frame[[factor]] <- as.character(participants[[factor]])
    }
  } else {
    stop("participants must be a data.frame or a list of Participant objects.",
         call. = FALSE)
  }

  if (anyNA(frame$id) || any(!nzchar(frame$id)) || anyDuplicated(frame$id)) {
    stop("Participant ids must be unique, non-empty, and non-missing.",
         call. = FALSE)
  }
  for (factor in factors) {
    values <- frame[[factor]]
    if (anyNA(values) || any(!nzchar(values)) ||
        any(!values %in% trial@strata[[factor]])) {
      stop("Invalid level found for stratum '", factor, "'.",
           call. = FALSE)
    }
  }
  frame
}

.stratum_keys <- function(frame, factors) {
  if (!length(factors)) {
    return(rep.int("", nrow(frame)))
  }
  vapply(seq_len(nrow(frame)), function(i) {
    paste0(factors, "=", unlist(frame[i, factors], use.names = FALSE),
           collapse = ";")
  }, character(1))
}

.sample_one <- function(x) {
  x[[sample.int(length(x), 1L)]]
}

.block_allocate <- function(n, arms, ratio, block_sizes) {
  assigned <- character(0)
  block_id <- integer(0)
  current_block <- 0L
  ratio_sum <- sum(as.double(ratio))
  while (length(assigned) < n) {
    current_block <- current_block + 1L
    block_size <- if (length(block_sizes) == 1L) {
      block_sizes[[1L]]
    } else {
      .sample_one(block_sizes)
    }
    counts <- as.integer(as.double(ratio) * (block_size / ratio_sum))
    block <- sample(rep.int(arms, counts), size = block_size, replace = FALSE)
    assigned <- c(assigned, block)
    block_id <- c(block_id, rep.int(current_block, block_size))
  }
  list(
    arm = assigned[seq_len(n)],
    block_id = as.integer(block_id[seq_len(n)])
  )
}

.stratum_seed <- function(seed, key) {
  hex <- substr(digest::digest(key, algo = "crc32", serialize = FALSE), 1L, 6L)
  offset <- strtoi(hex, base = 16L)
  as.integer((as.double(seed) + offset) %% (.Machine$integer.max - 1) + 1)
}

.allocate_stratified <- function(keys, arms, ratio, block_sizes, seed) {
  assigned <- rep.int(NA_character_, length(keys))
  block_id <- rep.int(NA_integer_, length(keys))
  block_offset <- 0L
  for (key in sort(unique(keys))) {
    index <- which(keys == key)
    set.seed(.stratum_seed(seed, key))
    stream <- .block_allocate(length(index), arms, ratio, block_sizes)
    assigned[index] <- stream$arm
    block_id[index] <- stream$block_id + block_offset
    block_offset <- max(block_id[index])
  }
  list(arm = assigned, block_id = as.integer(block_id))
}

.discrepancy <- function(values, measure) {
  if (measure == "range") {
    return(max(values) - min(values))
  }
  variance <- mean((values - mean(values))^2)
  if (measure == "variance") variance else sqrt(variance)
}

#' Score Pocock-Simon Imbalance
#'
#' Internal RNG-free scorer used by minimization and its numeric validation.
#'
#' @param state Nested count list indexed by factor, level, and arm.
#' @param levels Named factor-level vector for one participant.
#' @param arm Candidate arm.
#' @param arms Ordered arm labels.
#' @param ratio Named allocation weights.
#' @param weights Named factor weights.
#' @param measure One of `"range"`, `"variance"`, or `"sd"`.
#' @return Numeric imbalance score for the candidate arm.
#' @keywords internal
.ps_imbalance <- function(state, levels, arm, arms, ratio, weights,
                          measure = c("range", "variance", "sd")) {
  measure <- match.arg(measure)
  factors <- names(state)
  if (!length(factors) || !identical(names(levels), factors) ||
      !identical(names(weights), factors)) {
    stop("state, levels, and weights must describe the same factors.",
         call. = FALSE)
  }
  if (!arm %in% arms || !identical(names(ratio), arms) ||
      any(!is.finite(ratio)) || any(ratio <= 0)) {
    stop("arm and ratio must match arms.", call. = FALSE)
  }
  total <- 0
  for (factor in factors) {
    level <- levels[[factor]]
    if (!level %in% names(state[[factor]])) {
      stop("Unknown level '", level, "' for factor '", factor, "'.",
           call. = FALSE)
    }
    counts <- state[[factor]][[level]]
    if (!identical(names(counts), arms)) {
      stop("State arm counts must be named in arms order.", call. = FALSE)
    }
    projected <- (counts + as.numeric(arms == arm)) / ratio
    total <- total + weights[[factor]] * .discrepancy(projected, measure)
  }
  as.numeric(total)
}

.initial_minimization_state <- function(strata, arms) {
  lapply(strata, function(levels) {
    stats::setNames(
      lapply(levels, function(level) stats::setNames(
        rep.int(0, length(arms)), arms
      )),
      levels
    )
  })
}

.allocate_minimization <- function(frame, trial, ratio, weights, measure,
                                   p_bias) {
  factors <- names(trial@strata)
  state <- .initial_minimization_state(trial@strata, trial@arms)
  assigned <- character(nrow(frame))
  for (i in seq_len(nrow(frame))) {
    levels <- stats::setNames(
      vapply(factors, function(factor) frame[[factor]][[i]], character(1)),
      factors
    )
    scores <- vapply(trial@arms, function(arm) {
      .ps_imbalance(state, levels, arm, trial@arms, ratio, weights, measure)
    }, numeric(1))
    best <- trial@arms[scores == min(scores)]
    non_best <- setdiff(trial@arms, best)
    candidates <- if (stats::runif(1L) < p_bias || !length(non_best)) {
      best
    } else {
      non_best
    }
    chosen <- .sample_one(candidates)
    assigned[[i]] <- chosen
    for (factor in factors) {
      level <- levels[[factor]]
      state[[factor]][[level]][[chosen]] <-
        state[[factor]][[level]][[chosen]] + 1
    }
  }
  list(arm = assigned, block_id = rep.int(NA_integer_, nrow(frame)))
}

.sequence_fingerprint <- function(sequence) {
  audit_columns <- setdiff(
    names(sequence@audit),
    c("time", "agent", "n_revealed")
  )
  semantic <- list(
    trial_id = sequence@trial_id,
    method = sequence@method,
    arms = sequence@arms,
    ratio = sequence@ratio,
    seed = sequence@seed,
    strata = sequence@strata,
    block_sizes = sequence@block_sizes,
    p_bias = sequence@p_bias,
    weights = sequence@weights,
    imbalance = sequence@imbalance,
    table = as.list(sequence@table),
    audit = as.list(sequence@audit[audit_columns])
  )
  digest::digest(as.list(semantic), algo = "xxhash64")
}

.new_audit_row <- function(event, seed, n_revealed, agent) {
  data.frame(
    event = as.character(event),
    time = as.POSIXct(Sys.time()),
    seed = as.integer(seed),
    n_revealed = as.integer(n_revealed),
    agent = as.character(agent),
    stringsAsFactors = FALSE
  )
}

#' Generate a Concealed Randomization Sequence
#'
#' @param trial A [Trial] object.
#' @param method Allocation method. Simple randomization matches the allocation
#'   ratio only in expectation. Block methods enforce the ratio in every
#'   completed block.
#' @param n Number of anonymous slots. Valid only for an unstratified trial and
#'   the simple or permuted-block methods.
#' @param participants A data frame with `id` and trial-factor columns, or a
#'   list of [Participant] objects.
#' @param block_sizes Positive block lengths, each a multiple of the allocation
#'   weight sum.
#' @param seed Positive integer seed. If `NULL`, a seed is drawn and captured.
#' @param p_bias High-probability branch of minimization's biased coin.
#' @param weights Optional named prognostic-factor weights.
#' @param imbalance Pocock-Simon discrepancy measure.
#' @param requester Optional audit identity recorded at sealing.
#' @return A concealed [RandomizationSequence-class] object.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' randomize(trial, "permuted_block", n = 8, seed = 2026)
#' @export
randomize <- function(
  trial,
  method = c("simple", "permuted_block", "stratified_block", "minimization"),
  n = NULL,
  participants = NULL,
  block_sizes = NULL,
  seed = NULL,
  p_bias = 0.8,
  weights = NULL,
  imbalance = c("range", "variance", "sd"),
  requester = NA_character_
) {
  if (!methods::is(trial, "Trial")) {
    stop("trial must be a Trial object.", call. = FALSE)
  }
  method <- match.arg(method)
  imbalance <- match.arg(imbalance)
  if (!.is_scalar_string(requester, allow_na = TRUE)) {
    stop("requester must be one string or NA.", call. = FALSE)
  }
  if (!is.numeric(p_bias) || length(p_bias) != 1L ||
      !is.finite(p_bias) || p_bias < 0 || p_bias > 1) {
    stop("p_bias must be between zero and one.", call. = FALSE)
  }
  frame <- .participant_frame(trial, n, participants)
  if (method %in% c("stratified_block", "minimization") &&
      is.null(participants)) {
    stop("participants are required for this randomization method.",
         call. = FALSE)
  }
  if (method == "minimization" && !length(trial@strata)) {
    stop("minimization requires at least one trial stratum.", call. = FALSE)
  }

  seed <- .resolve_seed(seed)
  ratio <- stats::setNames(as.integer(trial@allocation_ratio), trial@arms)
  block_sizes <- .validate_block_sizes(block_sizes, ratio)
  weights <- .validate_factor_weights(weights, names(trial@strata))
  keys <- .stratum_keys(frame, names(trial@strata))

  set.seed(seed)
  allocation <- switch(
    method,
    simple = list(
      arm = sample(
        trial@arms,
        size = nrow(frame),
        replace = TRUE,
        prob = ratio / sum(as.double(ratio))
      ),
      block_id = rep.int(NA_integer_, nrow(frame))
    ),
    permuted_block = .block_allocate(
      nrow(frame), trial@arms, ratio, block_sizes
    ),
    stratified_block = .allocate_stratified(
      keys, trial@arms, ratio, block_sizes, seed
    ),
    minimization = .allocate_minimization(
      frame, trial, ratio, weights, imbalance, p_bias
    )
  )

  assignment_table <- data.frame(
    order = seq_len(nrow(frame)),
    participant_id = frame$id,
    stratum = keys,
    block_id = as.integer(allocation$block_id),
    arm = as.character(allocation$arm),
    stringsAsFactors = FALSE
  )
  audit <- .new_audit_row("seal", seed, 0L, requester)
  sequence <- methods::new(
    "RandomizationSequence",
    trial_id = trial@id,
    method = method,
    arms = trial@arms,
    ratio = ratio,
    seed = seed,
    strata = trial@strata,
    block_sizes = block_sizes,
    p_bias = as.numeric(p_bias),
    weights = weights,
    imbalance = imbalance,
    table = assignment_table,
    audit = audit,
    fingerprint = "<pending>",
    revealed = 0L
  )
  sequence@fingerprint <- .sequence_fingerprint(sequence)
  methods::validObject(sequence)
  sequence
}

#' Revealed Assignments
#'
#' @param seq A [RandomizationSequence-class] object.
#' @return Assignment table with every unrevealed arm replaced by `NA`.
#' @examples
#' sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
#' assignments(sequence)
#' @export
methods::setGeneric(
  "assignments",
  function(seq) standardGeneric("assignments")
)

#' @rdname assignments
#' @export
methods::setMethod("assignments", "RandomizationSequence", function(seq) {
  table <- seq@table
  if (seq@revealed < nrow(table)) {
    table$arm[seq.int(seq@revealed + 1L, nrow(table))] <- NA_character_
  }
  table
})

.sealed_assignments <- function(seq) {
  if (!methods::is(seq, "RandomizationSequence")) {
    stop("seq must be a RandomizationSequence.", call. = FALSE)
  }
  seq@table
}

#' Allocation Counts
#'
#' Counts use revealed rows only. The `stratum` value `"__overall__"` denotes
#' totals across all revealed strata.
#'
#' @param seq A [RandomizationSequence-class] object.
#' @return Data frame with `stratum`, `arm`, and `n` columns.
#' @examples
#' sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
#' allocationTable(sequence)
#' @export
methods::setGeneric(
  "allocationTable",
  function(seq) standardGeneric("allocationTable")
)

#' @rdname allocationTable
#' @export
methods::setMethod(
  "allocationTable", "RandomizationSequence",
  function(seq) {
    revealed <- assignments(seq)
    revealed <- revealed[!is.na(revealed$arm), , drop = FALSE]
    count_rows <- function(data, label) {
      counts <- tabulate(match(data$arm, seq@arms), nbins = length(seq@arms))
      data.frame(
        stratum = rep.int(label, length(seq@arms)),
        arm = seq@arms,
        n = as.integer(counts),
        stringsAsFactors = FALSE
      )
    }
    output <- count_rows(revealed, "__overall__")
    for (key in sort(unique(revealed$stratum))) {
      output <- rbind(
        output,
        count_rows(revealed[revealed$stratum == key, , drop = FALSE], key)
      )
    }
    rownames(output) <- NULL
    output
  }
)

#' Reveal the Next Allocation
#'
#' @param seq A [RandomizationSequence-class] object.
#' @return A list containing `participant_id`, `arm`, and the modified
#'   `sequence`.
#' @examples
#' sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
#' nextAllocation(sequence, requester = "site")
#' @export
methods::setGeneric(
  "nextAllocation",
  function(seq, requester = NA_character_) standardGeneric("nextAllocation")
)

#' @rdname nextAllocation
#' @param requester Optional audit identity.
#' @export
methods::setMethod(
  "nextAllocation", "RandomizationSequence",
  function(seq, requester = NA_character_) {
    if (!.is_scalar_string(requester, allow_na = TRUE)) {
      stop("requester must be one string or NA.", call. = FALSE)
    }
    if (seq@revealed >= nrow(seq@table)) {
      stop("The allocation sequence is exhausted.", call. = FALSE)
    }
    next_index <- seq@revealed + 1L
    seq@revealed <- next_index
    seq@audit <- rbind(
      seq@audit,
      .new_audit_row("reveal", seq@seed, next_index, requester)
    )
    seq@fingerprint <- .sequence_fingerprint(seq)
    methods::validObject(seq)
    list(
      participant_id = seq@table$participant_id[[next_index]],
      arm = seq@table$arm[[next_index]],
      sequence = seq
    )
  }
)

#' Captured Randomization Seed
#'
#' @param seq A [RandomizationSequence-class] object.
#' @return Positive integer seed.
#' @examples
#' sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 19)
#' randomSeed(sequence)
#' @export
methods::setGeneric(
  "randomSeed",
  function(seq) standardGeneric("randomSeed")
)

#' @rdname randomSeed
#' @export
methods::setMethod("randomSeed", "RandomizationSequence", function(seq) {
  seq@seed
})

#' Randomization Audit Log
#'
#' @param x A [RandomizationSequence-class] object.
#' @return Append-only audit data frame.
#' @examples
#' sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
#' auditLog(sequence)
#' @export
methods::setGeneric(
  "auditLog",
  function(x) standardGeneric("auditLog")
)

#' @rdname auditLog
#' @export
methods::setMethod("auditLog", "RandomizationSequence", function(x) {
  x@audit
})

#' Trial Object Fingerprint
#'
#' @param x A [RandomizationSequence-class] or [BlindingManager-class] object.
#' @return Deterministic `xxhash64` semantic-content fingerprint.
#' @examples
#' sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
#' trialFingerprint(sequence)
#' @export
methods::setGeneric(
  "trialFingerprint",
  function(x) standardGeneric("trialFingerprint")
)

#' @rdname trialFingerprint
#' @export
methods::setMethod(
  "trialFingerprint", "RandomizationSequence",
  function(x) x@fingerprint
)

methods::setMethod("show", "RandomizationSequence", function(object) {
  cat(
    "RandomizationSequence:", object@trial_id, "\n",
    "  method:", object@method, "\n",
    "  revealed:", object@revealed, " of ", nrow(object@table), "\n",
    sep = ""
  )
  print(assignments(object), row.names = FALSE)
})
