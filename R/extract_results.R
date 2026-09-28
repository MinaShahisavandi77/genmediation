#' Extract indirect and total effects from a fitted model
#'
#' @description
#' Filters the parameter table of a fitted lavaan object to rows containing
#' only the defined (`:=`) indirect and total effect parameters, and returns
#' them as a tidy data frame with bootstrap confidence intervals.
#'
#' @param fit A `lavaan` object returned by [run_mediation()] or
#'   [lavaan::sem()].
#' @param ci_level Numeric. Confidence level for bootstrap CIs. Default `0.95`.
#' @param boot_ci_type Character. Bootstrap CI type passed to
#'   [lavaan::parameterEstimates()]. One of `"perc"` (default), `"norm"`,
#'   or `"bca.simple"`.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{label}{Parameter label (e.g. `"ind_m_pre"`, `"total_m"`)}
#'     \item{est}{Point estimate}
#'     \item{se}{Standard error}
#'     \item{ci_lower}{Lower confidence bound}
#'     \item{ci_upper}{Upper confidence bound}
#'     \item{pvalue}{Two-sided p-value}
#'     \item{std_all}{Fully standardised estimate}
#'   }
#'
#' @examples
#' \dontrun{
#' fit <- run_mediation(df, prs_type = "MDD", bootstrap = 500)
#' extract_indirect_effects(fit)
#' }
#'
#' @seealso [tidy_sem_results()]
#' @export
extract_indirect_effects <- function(fit,
                                      ci_level      = 0.95,
                                      boot_ci_type  = "perc") {
  .check_lavaan(fit)

  pe <- lavaan::parameterEstimates(
    fit,
    ci           = TRUE,
    level        = ci_level,
    boot.ci.type = boot_ci_type,
    standardized = TRUE
  )

  # Keep only defined (:=) parameters
  indirect <- pe[pe$op == ":=", ]

  data.frame(
    label     = indirect$label,
    est       = indirect$est,
    se        = indirect$se,
    ci_lower  = indirect$ci.lower,
    ci_upper  = indirect$ci.upper,
    pvalue    = indirect$pvalue,
    std_all   = indirect$std.all,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


#' Return a tidy data frame of all SEM parameter estimates
#'
#' @description
#' Wraps [lavaan::parameterEstimates()] and returns all parameters (regression
#' paths, covariances, defined effects) as a clean data frame, suitable for
#' further processing or export.
#'
#' @inheritParams extract_indirect_effects
#' @param include_defined Logical. Include `:=` defined parameters?
#'   Default `TRUE`.
#'
#' @return A `data.frame` with one row per parameter and columns: `lhs`, `op`,
#'   `rhs`, `label`, `est`, `se`, `ci_lower`, `ci_upper`, `pvalue`, `std_all`.
#'
#' @examples
#' \dontrun{
#' fit    <- run_mediation(df, prs_type = "MDD", bootstrap = 500)
#' result <- tidy_sem_results(fit)
#' write.csv(result, "sem_results.csv", row.names = FALSE)
#' }
#'
#' @seealso [extract_indirect_effects()]
#' @export
tidy_sem_results <- function(fit,
                              ci_level        = 0.95,
                              boot_ci_type    = "perc",
                              include_defined = TRUE) {
  .check_lavaan(fit)

  pe <- lavaan::parameterEstimates(
    fit,
    ci           = TRUE,
    level        = ci_level,
    boot.ci.type = boot_ci_type,
    standardized = TRUE
  )

  if (!include_defined) {
    pe <- pe[pe$op != ":=", ]
  }

  data.frame(
    lhs      = pe$lhs,
    op       = pe$op,
    rhs      = pe$rhs,
    label    = pe$label,
    est      = pe$est,
    se       = pe$se,
    ci_lower = pe$ci.lower,
    ci_upper = pe$ci.upper,
    pvalue   = pe$pvalue,
    std_all  = pe$std.all,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


#' Print a formatted summary of a genetic mediation fit
#'
#' @description
#' Convenience wrapper around `lavaan::summary()` with sensible defaults for
#' this package (standardised estimates, fit measures, bootstrap CIs).
#'
#' @param fit A `lavaan` object.
#' @param ... Additional arguments passed to `lavaan::summary()`.
#'
#' @return Invisibly returns `fit`. Called for its side-effect of printing.
#'
#' @examples
#' \dontrun{
#' fit <- run_mediation(df, prs_type = "MDD", bootstrap = 500)
#' print_sem_summary(fit)
#' }
#'
#' @export
print_sem_summary <- function(fit, ...) {
  .check_lavaan(fit)
  lavaan::summary(
    fit,
    standardized  = TRUE,
    fit.measures  = TRUE,
    ci            = TRUE,
    ...
  )
  invisible(fit)
}


# ----------------------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------------------
.check_lavaan <- function(fit) {
  if (!methods::is(fit, "lavaan")) {
    stop("`fit` must be a lavaan object returned by run_mediation().")
  }
}
