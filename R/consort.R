.consort_required_columns <- c(
  "participant_id",
  "eligible",
  "randomized",
  "arm",
  "received_allocated",
  "followup_complete",
  "analysed",
  "pre_exclusion_reason",
  "not_received_reason",
  "followup_reason",
  "analysis_exclusion_reason"
)

.consort_character_columns <- c(
  "participant_id",
  "arm",
  "pre_exclusion_reason",
  "not_received_reason",
  "followup_reason",
  "analysis_exclusion_reason"
)

.consort_logical_columns <- c(
  "eligible",
  "randomized",
  "received_allocated",
  "followup_complete",
  "analysed"
)

.consort_reason_valid <- function(reason, required) {
  present <- !is.na(reason) & nzchar(reason)
  all(ifelse(required, present, is.na(reason)))
}

.validate_consort_flow <- function(trial, flow) {
  if (!methods::is(trial, "Trial")) {
    stop("trial must be a Trial object.", call. = FALSE)
  }
  if (!is.data.frame(flow) || !nrow(flow)) {
    stop("flow must be a non-empty data.frame.", call. = FALSE)
  }
  missing_columns <- setdiff(.consort_required_columns, names(flow))
  if (length(missing_columns)) {
    stop(
      "flow is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  flow <- flow[, .consort_required_columns, drop = FALSE]
  for (column in .consort_character_columns) {
    flow[[column]] <- as.character(flow[[column]])
  }
  for (column in .consort_logical_columns) {
    if (!is.logical(flow[[column]])) {
      stop(column, " must be logical.", call. = FALSE)
    }
  }
  if (anyNA(flow$participant_id) || any(!nzchar(flow$participant_id)) ||
      anyDuplicated(flow$participant_id)) {
    stop("participant_id must be unique, non-empty, and non-missing.",
         call. = FALSE)
  }
  if (anyNA(flow$eligible) || anyNA(flow$randomized)) {
    stop("eligible and randomized must be non-missing.", call. = FALSE)
  }
  if (any(flow$randomized & !flow$eligible)) {
    stop("Every randomized participant must be eligible.", call. = FALSE)
  }

  randomized <- flow$randomized
  if (any(is.na(flow$arm[randomized])) ||
      any(!nzchar(flow$arm[randomized])) ||
      any(!flow$arm[randomized] %in% trial@arms)) {
    stop("Every randomized participant must have a known trial arm.",
         call. = FALSE)
  }
  if (any(!is.na(flow$arm[!randomized]))) {
    stop("Non-randomized participants must not have an arm.", call. = FALSE)
  }
  for (column in c("received_allocated", "followup_complete", "analysed")) {
    if (anyNA(flow[[column]][randomized])) {
      stop(column, " must be recorded for every randomized participant.",
           call. = FALSE)
    }
    if (any(!is.na(flow[[column]][!randomized]))) {
      stop(column, " must be NA for non-randomized participants.",
           call. = FALSE)
    }
  }

  if (!.consort_reason_valid(
    flow$pre_exclusion_reason,
    !randomized
  )) {
    stop(
      "pre_exclusion_reason is required only for non-randomized participants.",
      call. = FALSE
    )
  }
  if (!.consort_reason_valid(
    flow$not_received_reason,
    randomized & !flow$received_allocated
  )) {
    stop(
      "not_received_reason is required only when allocated treatment was not received.",
      call. = FALSE
    )
  }
  if (!.consort_reason_valid(
    flow$followup_reason,
    randomized & !flow$followup_complete
  )) {
    stop(
      "followup_reason is required only when follow-up is incomplete.",
      call. = FALSE
    )
  }
  if (!.consort_reason_valid(
    flow$analysis_exclusion_reason,
    randomized & !flow$analysed
  )) {
    stop(
      "analysis_exclusion_reason is required only when a participant is not analysed.",
      call. = FALSE
    )
  }
  flow
}

.consort_node <- function(node_id, stage, arm, label, n) {
  data.frame(
    node_id = node_id,
    stage = stage,
    arm = arm,
    label = label,
    n = as.integer(n),
    stringsAsFactors = FALSE
  )
}

.consort_nodes <- function(trial, flow) {
  randomized <- flow$randomized
  nodes <- rbind(
    .consort_node(
      "assessed", "assessed", NA_character_,
      "Assessed for eligibility", nrow(flow)
    ),
    .consort_node(
      "excluded", "excluded_before_randomization", NA_character_,
      "Excluded before randomization", sum(!randomized)
    ),
    .consort_node(
      "randomized", "randomized", NA_character_,
      "Randomized", sum(randomized)
    )
  )
  for (index in seq_along(trial@arms)) {
    arm <- trial@arms[[index]]
    selected <- randomized & flow$arm == arm
    prefix <- paste0("arm", index)
    nodes <- rbind(
      nodes,
      .consort_node(
        paste0(prefix, "_allocated"), "allocated", arm,
        paste0("Allocated to ", arm), sum(selected)
      ),
      .consort_node(
        paste0(prefix, "_received"), "received", arm,
        "Received allocated intervention",
        sum(selected & flow$received_allocated)
      ),
      .consort_node(
        paste0(prefix, "_not_received"), "did_not_receive", arm,
        "Did not receive allocated intervention",
        sum(selected & !flow$received_allocated)
      ),
      .consort_node(
        paste0(prefix, "_followup"), "followup_complete", arm,
        "Follow-up complete",
        sum(selected & flow$followup_complete)
      ),
      .consort_node(
        paste0(prefix, "_followup_incomplete"), "followup_incomplete", arm,
        "Follow-up incomplete",
        sum(selected & !flow$followup_complete)
      ),
      .consort_node(
        paste0(prefix, "_analysed"), "analysed", arm,
        "Analysed",
        sum(selected & flow$analysed)
      ),
      .consort_node(
        paste0(prefix, "_excluded_analysis"), "excluded_analysis", arm,
        "Excluded from analysis",
        sum(selected & !flow$analysed)
      )
    )
  }
  rownames(nodes) <- NULL
  nodes
}

.consort_edges <- function(trial) {
  edges <- data.frame(
    from = c("assessed", "assessed"),
    to = c("excluded", "randomized"),
    stringsAsFactors = FALSE
  )
  for (index in seq_along(trial@arms)) {
    prefix <- paste0("arm", index)
    allocated <- paste0(prefix, "_allocated")
    children <- paste0(
      prefix,
      c(
        "_received", "_not_received",
        "_followup", "_followup_incomplete",
        "_analysed", "_excluded_analysis"
      )
    )
    edges <- rbind(
      edges,
      data.frame(
        from = c("randomized", rep.int(allocated, length(children))),
        to = c(allocated, children),
        stringsAsFactors = FALSE
      )
    )
  }
  rownames(edges) <- NULL
  edges
}

.count_consort_reasons <- function(reason, stage, arm) {
  if (!length(reason)) {
    return(data.frame(
      stage = character(0),
      arm = character(0),
      reason = character(0),
      n = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  counts <- as.data.frame(table(reason), stringsAsFactors = FALSE)
  counts <- counts[order(counts$reason), , drop = FALSE]
  data.frame(
    stage = rep.int(stage, nrow(counts)),
    arm = rep.int(arm, nrow(counts)),
    reason = as.character(counts$reason),
    n = as.integer(counts$Freq),
    stringsAsFactors = FALSE
  )
}

.consort_reasons <- function(trial, flow) {
  randomized <- flow$randomized
  reasons <- .count_consort_reasons(
    flow$pre_exclusion_reason[!randomized],
    "excluded_before_randomization",
    NA_character_
  )
  for (arm in trial@arms) {
    selected <- randomized & flow$arm == arm
    reasons <- rbind(
      reasons,
      .count_consort_reasons(
        flow$not_received_reason[selected & !flow$received_allocated],
        "did_not_receive",
        arm
      ),
      .count_consort_reasons(
        flow$followup_reason[selected & !flow$followup_complete],
        "followup_incomplete",
        arm
      ),
      .count_consort_reasons(
        flow$analysis_exclusion_reason[selected & !flow$analysed],
        "excluded_analysis",
        arm
      )
    )
  }
  rownames(reasons) <- NULL
  reasons
}

.validate_consort_counts <- function(trial, nodes, reasons) {
  node_n <- stats::setNames(nodes$n, nodes$node_id)
  if (node_n[["assessed"]] !=
      node_n[["excluded"]] + node_n[["randomized"]]) {
    stop("CONSORT root counts do not reconcile.", call. = FALSE)
  }
  allocated_total <- 0L
  for (index in seq_along(trial@arms)) {
    arm <- trial@arms[[index]]
    prefix <- paste0("arm", index)
    allocated <- node_n[[paste0(prefix, "_allocated")]]
    allocated_total <- allocated_total + allocated
    pairs <- list(
      c("_received", "_not_received"),
      c("_followup", "_followup_incomplete"),
      c("_analysed", "_excluded_analysis")
    )
    for (pair in pairs) {
      if (allocated !=
          node_n[[paste0(prefix, pair[[1L]])]] +
            node_n[[paste0(prefix, pair[[2L]])]]) {
        stop("CONSORT arm counts do not reconcile for arm '", arm, "'.",
             call. = FALSE)
      }
    }
    complementary <- c(
      did_not_receive = "_not_received",
      followup_incomplete = "_followup_incomplete",
      excluded_analysis = "_excluded_analysis"
    )
    for (stage in names(complementary)) {
      reason_total <- sum(reasons$n[
        reasons$stage == stage & !is.na(reasons$arm) & reasons$arm == arm
      ])
      if (reason_total != node_n[[paste0(prefix, complementary[[stage]])]]) {
        stop("CONSORT reason counts do not reconcile for arm '", arm, "'.",
             call. = FALSE)
      }
    }
  }
  if (allocated_total != node_n[["randomized"]]) {
    stop("Allocated arm counts do not reconcile with randomized participants.",
         call. = FALSE)
  }
  pre_reason_total <- sum(
    reasons$n[reasons$stage == "excluded_before_randomization"]
  )
  if (pre_reason_total != node_n[["excluded"]]) {
    stop("Pre-randomization reason counts do not reconcile.", call. = FALSE)
  }
  invisible(TRUE)
}

.dot_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\r", "", x, fixed = TRUE)
  gsub("\n", "\\n", x, fixed = TRUE)
}

.consort_dot <- function(nodes, edges, reasons, show_reasons) {
  labels <- vapply(seq_len(nrow(nodes)), function(i) {
    node <- nodes[i, , drop = FALSE]
    label <- paste0(.dot_escape(node$label), "\\n(n=", node$n, ")")
    if (show_reasons) {
      selected <- reasons$stage == node$stage &
        ((is.na(reasons$arm) & is.na(node$arm)) |
           (!is.na(reasons$arm) & !is.na(node$arm) &
              reasons$arm == node$arm))
      if (any(selected)) {
        escaped_reasons <- vapply(
          reasons$reason[selected],
          .dot_escape,
          character(1)
        )
        detail <- paste0(
          escaped_reasons, " (", reasons$n[selected], ")",
          collapse = "\\n"
        )
        label <- paste0(label, "\\n", detail)
      }
    }
    paste0(
      "  ", node$node_id,
      " [label=\"", label, "\"];"
    )
  }, character(1))
  edge_lines <- paste0("  ", edges$from, " -> ", edges$to, ";")
  paste(
    c(
      "digraph consort {",
      "  graph [rankdir=TB, nodesep=0.35, ranksep=0.55];",
      "  node [shape=box, style=\"rounded\", fontname=\"Helvetica\"];",
      labels,
      edge_lines,
      "}"
    ),
    collapse = "\n"
  )
}

#' CONSORT Participant-Flow Diagram
#'
#' Builds reconciled CONSORT participant counts and Graphviz DOT. DiagrammeR
#' rendering is optional; the inspectable counts and DOT are always returned.
#'
#' @param trial A [Trial] object.
#' @param flow One row per assessed participant using the documented flow
#'   schema.
#' @param render Whether to create a `DiagrammeR::grViz()` widget.
#' @param show_reasons Whether exclusion/disposition reasons appear in labels.
#' @return A `consort_diagram` object containing nodes, edges, reason counts,
#'   DOT, and an optional widget.
#' @references Schulz KF, Altman DG, Moher D (2010). CONSORT 2010 Statement.
#' @examples
#' trial <- Trial("T1", c("active", "control"))
#' flow <- data.frame(
#'   participant_id = c("p1", "p2", "p3"),
#'   eligible = c(TRUE, TRUE, FALSE),
#'   randomized = c(TRUE, TRUE, FALSE),
#'   arm = c("active", "control", NA),
#'   received_allocated = c(TRUE, TRUE, NA),
#'   followup_complete = c(TRUE, TRUE, NA),
#'   analysed = c(TRUE, TRUE, NA),
#'   pre_exclusion_reason = c(NA, NA, "ineligible"),
#'   not_received_reason = c(NA, NA, NA),
#'   followup_reason = c(NA, NA, NA),
#'   analysis_exclusion_reason = c(NA, NA, NA)
#' )
#' consortDiagram(trial, flow, render = FALSE)
#' @export
consortDiagram <- function(
  trial,
  flow,
  render = requireNamespace("DiagrammeR", quietly = TRUE),
  show_reasons = TRUE
) {
  if (!is.logical(render) || length(render) != 1L || is.na(render) ||
      !is.logical(show_reasons) || length(show_reasons) != 1L ||
      is.na(show_reasons)) {
    stop("render and show_reasons must be one non-missing logical.",
         call. = FALSE)
  }
  flow <- .validate_consort_flow(trial, flow)
  nodes <- .consort_nodes(trial, flow)
  edges <- .consort_edges(trial)
  reasons <- .consort_reasons(trial, flow)
  .validate_consort_counts(trial, nodes, reasons)
  dot <- .consort_dot(nodes, edges, reasons, show_reasons)
  widget <- NULL
  if (render) {
    if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
      stop(
        "DiagrammeR is required for render = TRUE; install it or use render = FALSE.",
        call. = FALSE
      )
    }
    widget <- DiagrammeR::grViz(dot)
  }
  structure(
    list(
      trial_id = trial@id,
      nodes = nodes,
      edges = edges,
      reasons = reasons,
      dot = dot,
      widget = widget
    ),
    class = "consort_diagram"
  )
}

#' CONSORT Node Counts
#'
#' @param x A `consort_diagram`.
#' @return Node data frame used by the rendered diagram.
#' @examples
#' # See consortDiagram() for a complete flow example.
#' @export
consortCounts <- function(x) {
  if (!inherits(x, "consort_diagram")) {
    stop("x must be a consort_diagram.", call. = FALSE)
  }
  x$nodes
}

#' @export
format.consort_diagram <- function(x, ...) {
  x$dot
}

#' @export
as.data.frame.consort_diagram <- function(x, ...) {
  x$nodes
}

#' @export
print.consort_diagram <- function(x, ...) {
  cat(
    "<consort_diagram> ", x$trial_id, ": ",
    x$nodes$n[x$nodes$stage == "assessed"], " assessed, ",
    x$nodes$n[x$nodes$stage == "randomized"], " randomized\n",
    sep = ""
  )
  print(x$nodes, row.names = FALSE, ...)
  invisible(x)
}
