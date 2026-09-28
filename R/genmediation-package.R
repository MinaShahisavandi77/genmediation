#' genmediation: Genetic Mediation Analysis via Longitudinal SEM
#'
#' @description
#' Fits structural equation models (SEM) to estimate genetic mediation of
#' polygenic risk scores (PRS) on outcomes through repeated mediator
#' measurements. Fully generic — no variable names are assumed.
#'
#' @section Main functions:
#' - [standardise_vars()]: Z-score any set of columns
#' - [build_mediation_model()]: Generate lavaan syntax for any mediator structure
#' - [run_mediation()]: Fit the SEM with bootstrap inference
#' - [extract_indirect_effects()]: Pull indirect/total effects as a data frame
#' - [tidy_sem_results()]: All parameters as a tidy data frame
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lavaan sem summary parameterEstimates fitMeasures
#' @importFrom methods is
## usethis namespace: end
NULL
