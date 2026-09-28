# ============================================================
# build_model.R
# ============================================================

#' Build a lavaan mediation model string
#'
#' @description
#' Generates a complete lavaan SEM syntax string for genetic mediation through
#' repeated mediator measurements. Fully generic — no variable names are
#' assumed. The user has full control over which covariates enter which
#' equations.
#'
#' @param outcome Character scalar. Name of the outcome variable.
#'
#' @param mediator_vars Named character vector of mediator variable names **in
#'   chronological order**. Any length >= 1. Names are used as parameter label
#'   stems; if unnamed, labels t1, t2, ... are used automatically.
#'
#' @param prs_child Character scalar. Column name of the child PRS variable.
#' @param prs_parent1 Character scalar. Column name of the first parent PRS.
#' @param prs_parent2 Character scalar. Column name of the second parent PRS.
#'
#' @param covariates_mediator Covariates for the mediator regressions. Either:
#'   \itemize{
#'     \item A **character vector** — the same covariates enter every mediator
#'       equation.
#'     \item A **named list** — each element is a character vector for one
#'       timepoint. Names must match the names of `mediator_vars`. Timepoints
#'       not listed receive no covariates. Use `NULL` for a timepoint
#'       explicitly to suppress covariates there.
#'   }
#'   Default `NULL` — no covariates in any mediator equation.
#'
#' @param covariates_outcome Character vector of covariates for the outcome
#'   regression. Default `NULL` — no covariates in the outcome equation.
#'
#' @return A single character string of lavaan model syntax.
#'
#' @details
#' **Model structure:**
#'
#' *Mediator regressions* — each timepoint is regressed on the immediately
#' preceding timepoint (autoregressive path), all three PRS variables, and
#' whatever covariates were specified for that timepoint via
#' `covariates_mediator`.
#'
#' *Outcome regression* — the outcome is regressed on all mediator timepoints,
#' all three PRS variables, and `covariates_outcome`.
#'
#' *Exogenous (co)variances* — the PRS variables and all covariates appear
#' only as predictors, so they are exogenous. Under lavaan's default
#' `fixed.x = TRUE`, their variances and covariances are included
#' automatically; they are therefore not declared explicitly in the syntax.
#'
#' *Indirect effects* — every consecutive chain from `prs_parent1` through
#' one or more mediator timepoints to the outcome is defined via `:=`.
#' For `n` timepoints this yields `n*(n+1)/2` indirect paths.
#'
#' *Total indirect and total effect* — `total_ind` sums all indirect paths;
#' `total` adds the direct `prs_parent1` effect on the outcome.
#'
#' @examples
#' # Same covariates at every mediator timepoint
#' cat(build_mediation_model(
#'   outcome             = "child_outcome",
#'   mediator_vars       = c(w1 = "med_w1", w2 = "med_w2", w3 = "med_w3"),
#'   prs_child           = "prs_c",
#'   prs_parent1         = "prs_p1",
#'   prs_parent2         = "prs_p2",
#'   covariates_mediator = c("age", "sex"),
#'   covariates_outcome  = c("age", "sex", "child_age")
#' ))
#'
#' # Different covariates per timepoint (named list)
#' cat(build_mediation_model(
#'   outcome             = "child_outcome",
#'   mediator_vars       = c(w1 = "med_w1", w2 = "med_w2", w3 = "med_w3"),
#'   prs_child           = "prs_c",
#'   prs_parent1         = "prs_p1",
#'   prs_parent2         = "prs_p2",
#'   covariates_mediator = list(
#'     w1 = c("age", "sex"),
#'     w2 = c("age", "sex"),
#'     w3 = c("age", "sex", "extra_cov")  # extra covariate at wave 3 only
#'   ),
#'   covariates_outcome  = c("age", "sex", "child_age")
#' ))
#'
#' # No covariates anywhere
#' cat(build_mediation_model(
#'   outcome       = "outcome",
#'   mediator_vars = c(t1 = "m1", t2 = "m2"),
#'   prs_child     = "prs_c",
#'   prs_parent1   = "prs_p1",
#'   prs_parent2   = "prs_p2"
#' ))
#'
#' @seealso [run_mediation()]
#' @export
build_mediation_model <- function(outcome,
                                  mediator_vars,
                                  prs_child,
                                  prs_parent1,
                                  prs_parent2,
                                  covariates_mediator = NULL,
                                  covariates_outcome  = NULL) {
  .build_model_string(
    outcome             = outcome,
    mediator_vars       = mediator_vars,
    prs_child           = prs_child,
    prs_parent1         = prs_parent1,
    prs_parent2         = prs_parent2,
    covariates_mediator = covariates_mediator,
    covariates_outcome  = covariates_outcome
  )
}


# ============================================================
# Core internal builder
# ============================================================

#' @keywords internal
.build_model_string <- function(outcome,
                                mediator_vars,
                                prs_child,
                                prs_parent1,
                                prs_parent2,
                                covariates_mediator,
                                covariates_outcome) {
  
  # ---- input checks ------------------------------------------------
  if (!is.character(mediator_vars) || length(mediator_vars) < 1L)
    stop("`mediator_vars` must be a non-empty character vector.")
  
  n <- length(mediator_vars)
  
  # ---- timepoint label stems ---------------------------------------
  tp_labels <- if (!is.null(names(mediator_vars)) && all(nzchar(names(mediator_vars))))
    make.names(names(mediator_vars))
  else
    paste0("t", seq_len(n))
  
  # ---- resolve covariates_mediator to a list of length n -----------
  # Result: med_covs[[i]] is the character vector of covariates for timepoint i
  # (may be character(0) if none specified)
  med_covs <- .resolve_mediator_covs(covariates_mediator, tp_labels, n)
  
  # ---- outcome covariates ------------------------------------------
  out_covs <- if (is.null(covariates_outcome)) character(0)
  else as.character(covariates_outcome)
  
  # ---- parameter label helpers -------------------------------------
  bp1 <- function(i) paste0("bp1_", i)   # parent1 PRS -> mediator i
  bp2 <- function(i) paste0("bp2_", i)   # parent2 PRS -> mediator i
  bc  <- function(i) paste0("bc_",  i)   # child   PRS -> mediator i
  a   <- function(i) paste0("a",    i)   # autoregressive t_i -> t_{i+1}
  g   <- function(i) paste0("g",    i)   # mediator i -> outcome
  
  lines <- character(0)
  
  # ------------------------------------------------------------------
  # 1. Mediator regressions
  # ------------------------------------------------------------------
  lines <- c(lines,
             "# -------------------------------------------------------",
             "# Mediator regressions",
             "# -------------------------------------------------------"
  )
  
  for (i in seq_len(n)) {
    parts <- character(0)
    
    # autoregressive predecessor
    if (i > 1L)
      parts <- c(parts, paste0(a(i - 1L), "*", mediator_vars[i - 1L]))
    
    # PRS variables (always included)
    parts <- c(parts,
               paste0(bp1(i), "*", prs_parent1),
               paste0(bp2(i), "*", prs_parent2),
               paste0(bc(i),  "*", prs_child)
    )
    
    # covariates for this timepoint
    covs_i <- med_covs[[i]]
    if (length(covs_i) > 0L) {
      cov_labels <- paste0("cov_", tp_labels[i], "_",
                           make.names(covs_i))
      parts <- c(parts, paste0(cov_labels, "*", covs_i))
    }
    
    rhs <- paste(parts, collapse = " + ")
    lines <- c(lines, paste0(mediator_vars[i], " ~ ", rhs))
  }
  lines <- c(lines, "")
  
  # ------------------------------------------------------------------
  # 2. Outcome regression
  # ------------------------------------------------------------------
  lines <- c(lines,
             "# -------------------------------------------------------",
             "# Outcome regression",
             "# -------------------------------------------------------"
  )
  
  parts <- character(0)
  
  # mediator terms
  parts <- c(parts, paste0(g(seq_len(n)), "*", mediator_vars))
  
  # direct PRS effects
  parts <- c(parts,
             paste0("gp1*", prs_parent1),
             paste0("gp2*", prs_parent2),
             paste0("gc*",  prs_child)
  )
  
  # outcome covariates
  if (length(out_covs) > 0L) {
    out_cov_labels <- paste0("gcov_", make.names(out_covs))
    parts <- c(parts, paste0(out_cov_labels, "*", out_covs))
  }
  
  lines <- c(lines,
             paste0(outcome, " ~ ", paste(parts, collapse = " + ")),
             ""
  )
  
  # ------------------------------------------------------------------
  # 3. Indirect effects — all consecutive chains
  # ------------------------------------------------------------------
  lines <- c(lines,
             "# -------------------------------------------------------",
             "# Indirect effects (all consecutive chains)",
             "# -------------------------------------------------------"
  )
  
  all_ind_labels <- character(0)
  
  for (i in seq_len(n)) {
    for (j in i:n) {
      label <- paste0("ind_", paste(tp_labels[i:j], collapse = "_"))
      
      ar_chain <- if (j > i)
        paste(sapply(i:(j - 1L), a), collapse = " * ")
      else ""
      
      path <- if (nzchar(ar_chain))
        paste(bp1(i), ar_chain, g(j), sep = " * ")
      else
        paste(bp1(i), g(j), sep = " * ")
      
      lines <- c(lines, paste0(label, " := ", path))
      all_ind_labels <- c(all_ind_labels, label)
    }
  }
  lines <- c(lines, "")
  
  # ------------------------------------------------------------------
  # 4. Total indirect and total effect
  # ------------------------------------------------------------------
  lines <- c(lines,
             "# -------------------------------------------------------",
             "# Total indirect and total effect",
             "# -------------------------------------------------------",
             paste0("total_ind := ",
                    .wrap_sum(all_ind_labels, indent = "              ")),
             "total_p1     := gp1 + total_ind"
  )
  
  paste(lines, collapse = "\n")
}


# ============================================================
# Internal: resolve covariates_mediator to a list of length n
# ============================================================

#' @keywords internal
.resolve_mediator_covs <- function(covariates_mediator, tp_labels, n) {
  
  # Case 1: NULL — no covariates anywhere
  if (is.null(covariates_mediator))
    return(vector("list", n))
  
  # Case 2: plain character vector — same covariates for every timepoint
  if (is.character(covariates_mediator)) {
    covs <- as.character(covariates_mediator)
    return(rep(list(covs), n))
  }
  
  # Case 3: named list — per-timepoint specification
  if (is.list(covariates_mediator)) {
    lst_names <- names(covariates_mediator)
    if (is.null(lst_names) || any(!nzchar(lst_names)))
      stop("`covariates_mediator` list must be fully named. ",
           "Names must match the names of `mediator_vars`.")
    
    bad <- setdiff(lst_names, tp_labels)
    if (length(bad) > 0L)
      stop("Names in `covariates_mediator` list do not match `mediator_vars` ",
           "names: ", paste(bad, collapse = ", "), ".\n",
           "  `mediator_vars` names are: ", paste(tp_labels, collapse = ", "))
    
    # build length-n list; timepoints not mentioned get character(0)
    out <- vector("list", n)
    for (i in seq_len(n)) {
      key <- tp_labels[i]
      if (key %in% lst_names) {
        val <- covariates_mediator[[key]]
        out[[i]] <- if (is.null(val)) character(0) else as.character(val)
      } else {
        out[[i]] <- character(0)
      }
    }
    return(out)
  }
  
  stop("`covariates_mediator` must be NULL, a character vector, or a named list.")
}


# ============================================================
# Helper: wrap a long sum across lines
# ============================================================
.wrap_sum <- function(terms, indent = "  ", width = 70L) {
  if (length(terms) == 0L) return("0")
  current <- terms[1L]
  out <- character(0)
  for (term in terms[-1L]) {
    candidate <- paste0(current, " + ", term)
    if (nchar(candidate) > width) {
      out     <- c(out, paste0(current, " +"))
      current <- paste0(indent, term)
    } else {
      current <- candidate
    }
  }
  paste(c(out, current), collapse = "\n")
}