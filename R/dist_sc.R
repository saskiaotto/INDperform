#' Score-based distance matrix
#'
#' The function computes a distance matrix based on the \code{\link{scoring}} output
#' tibble (or the output tibble from the \code{\link{expect_resp}} function).
#'
#' @param scores_mat A data frame or matrix containing the scores. This could be
#' the \code{$scores_matrix} output of the \code{\link{summary_sc}} function.
#' @param method_dist Dissimilarity index used in the \code{vegdist} function
#'  to calculate the dissimilarity matrix based on the scores.
#'  Default is `euclidean`, for alternatives see \code{\link[vegan]{vegdist}}.
#' @param ... Further arguments to be passed to the method \code{vegdist}.
#'
#' @return
#' The function returns a \code{\link[stats]{dist}} object.
#'
#' @seealso \code{\link[vegan]{vegdist}} for the computation of the
#' dissimilarity index
#' @family score-based IND performance functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data
#' scores_tbl <- scoring(trend_tbl = model_trend_ex,
#'   mod_tbl = all_results_ex, press_type = press_type_ex)
#' scores_mat <- summary_sc(scores_tbl)$scores_matrix
#' dist_matrix <- dist_sc(scores_mat, method_dist = "euclidean")
dist_sc <- function(scores_mat, method_dist = "euclidean", ...) {

  # Data input validation -----------------------
  if (!is.data.frame(scores_mat) & !is.matrix(scores_mat)) {
    stop("scores_mat is not a data frame or matrix.")
  }

  # Calculate distance matrix (default Euclidean) ---
  scores_dist <- vegan::vegdist(scores_mat, method = method_dist,
    ...)

  return(scores_dist)

}
