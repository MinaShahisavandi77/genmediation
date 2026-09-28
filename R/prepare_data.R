#' Z-score standardise variables
#'
#' @description
#' Replaces selected columns in a data frame with their Z-score standardised
#' counterparts (mean = 0, SD = 1). Fully generic — pass any column names.
#'
#' @param data A `data.frame`.
#' @param vars Character vector of column names to standardise.
#'
#' @return The input `data.frame` with the specified columns Z-scored.
#'   All other columns are unchanged.
#'
#' @examples
#' df <- data.frame(x = rnorm(100, 5, 2), y = rnorm(100, 10, 3), z = 1:100)
#' df <- standardise_vars(df, vars = c("x", "y"))
#' round(colMeans(df[c("x", "y")]), 10)  # both 0
#'
#' @export
standardise_vars <- function(data, vars) {
  if (!is.data.frame(data))
    stop("`data` must be a data.frame.")
  if (!is.character(vars) || length(vars) < 1L)
    stop("`vars` must be a non-empty character vector.")

  missing_cols <- setdiff(vars, names(data))
  if (length(missing_cols) > 0L)
    stop("Columns not found in `data`: ",
         paste(missing_cols, collapse = ", "))

  data[vars] <- lapply(data[vars], function(x) as.numeric(scale(x)))
  data
}
