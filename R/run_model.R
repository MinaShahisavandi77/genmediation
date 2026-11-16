#' Fit a genetic mediation SEM
#'
#' @description
#' Fits a lavaan SEM for genetic mediation. Pass a model string built by
#' [build_mediation_model()] or any custom lavaan syntax.
#'
#' @param data A `data.frame`. PRS variables should already be Z-scored
#'   (see [standardise_vars()]).
#' @param model Character scalar. lavaan model syntax string, typically from
#'   [build_mediation_model()].
#' @param se Character. SE method passed to [lavaan::sem()]. Default
#'   `"standard"`.
#' @param bootstrap Integer. Number of bootstrap draws. Default `1000`.
#' @param missing Character. Missing data handling. Default `"fiml"`.
#' @param seed Integer or `NULL`. If supplied, sets the random seed for
#'   reproducible bootstrap draws. Default `NULL`.
#' @param ... Additional arguments forwarded to [lavaan::sem()].
#'
#' @return A fitted `lavaan` object.
#'
#' @examples
#' \dontrun{
#' # Standard errors with FIML (recommended default)
#' model <- build_mediation_model(
#'   outcome       = "child_outcome",
#'   mediator_vars = c(w1 = "med_w1", w2 = "med_w2"),
#'   prs_child     = "prs_c",
#'   prs_parent1   = "prs_p1",
#'   prs_parent2   = "prs_p2"
#' )
#' fit <- run_mediation(data = df, model = model)
#'
#' # Bootstrap CIs for indirect effects (requires listwise deletion)
#' fit_boot <- run_mediation(
#'   data      = df,
#'   model     = model,
#'   se        = "bootstrap",
#'   bootstrap = 5000,
#'   missing   = "listwise",
#'   seed      = 2024
#' )
#' }
#'
#' @seealso [build_mediation_model()], 
#'   [extract_indirect_effects()], [tidy_sem_results()]
#' @export
run_mediation <- function(data,
                          model,
                          se        = c("robust", "standard", "bootstrap"),
                          bootstrap = 1000L,
                          missing   = "fiml.x",
                          cluster   = NULL,
                          seed      = NULL,
                          ...) {
  
  se <- match.arg(se)
  
  # ---- input checks --------------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data.frame.")
  if (!is.character(model) || length(model) != 1L)
    stop("`model` must be a single character string of lavaan syntax.")
  if (!is.null(seed)) set.seed(seed)
  
  if (!is.null(cluster)) {
    if (!is.character(cluster) || length(cluster) != 1L)
      stop("`cluster` must be a single column name.")
    if (!cluster %in% names(data))
      stop(sprintf("Cluster variable '%s' not found in `data`.", cluster))
    if (anyNA(data[[cluster]]))
      stop(sprintf("Cluster variable '%s' has missing values; give every row a cluster ID.", cluster))
    if (se == "bootstrap")
      stop("lavaan's bootstrap resamples individuals, not clusters. ",
           "Use se = 'robust' (cluster-robust SEs) with `cluster`.")
  }
  
  # ---- translate se option -------------------------------------------------
  lav_se <- switch(se,
                   robust    = if (is.null(cluster)) "robust.huber.white" else "robust.cluster",
                   standard  = "standard",
                   bootstrap = "bootstrap"
  )
  
  message(sprintf(
    "Fitting SEM with se = '%s', missing = '%s'%s%s ...",
    lav_se, missing,
    if (!is.null(cluster)) paste0(", cluster = '", cluster, "'") else "",
    if (se == "bootstrap") paste0(", bootstrap = ", bootstrap) else ""
  ))
  
  sem_args <- list(
    model   = model,
    data    = data,
    se      = lav_se,
    missing = missing,
    ...
  )
  if (se == "robust")      sem_args$test      <- "yuan.bentler.mplus"  # robust chi-square (as MLR)
  if (!is.null(cluster))   sem_args$cluster   <- cluster
  if (se == "bootstrap")   sem_args$bootstrap <- bootstrap
  
  do.call(lavaan::sem, sem_args)
}
