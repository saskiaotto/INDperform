#' Score-based cluster analysis
#'
#' \code{clust_sc} computes a hierarchical cluster analysis for the identification
#'  of indicator redundancies.
#'
#' @param dist_mat The distance matrix computed by the \code{\link{dist_sc}} function.
#' @param method_clust The agglomeration method to be used in the \code{hclust} function.
#'  Default is "average", for alternatives see \code{\link[stats]{hclust}}.
#' @param ... Further arguments to be passed to the method \code{hclust}.
#'
#' @return An object of class \code{hclust} is returned, which describes the tree produced by the
#' clustering process. See for more details \code{\link[stats]{hclust}}. Additionally,
#' the cophenetic correlation coefficient and the Gower distance are printed in the
#' console as guidance for selecting the best agglomeration method.
#'
#' @seealso \code{\link{hclust}}
#' @family score-based IND performance functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data
#' scores_tbl <- scoring(trend_tbl = model_trend_ex,
#'   mod_tbl = all_results_ex, press_type = press_type_ex)
#' dist_matrix <- dist_sc(scores_tbl, method_dist = "euclidean")
#' clust_analysis <- clust_sc(dist_matrix, method_clust = "complete")
clust_sc <- function(dist_mat, method_clust = "average",
  ...) {

	 # Data input validation --------
	 if (class(dist_mat) != "dist") {
	 	 stop("'dist_mat' is not an object of class 'dist'! Is this the correct output from the dist_sc() function?")
	 }
	 # --------------------

  score_hc <- stats::hclust(dist_mat, method = method_clust)

  # Calculate cophenetic correlation coefficient and
  # Gower distance
  coph.corr <- round(stats::cor(dist_mat,
    stats::cophenetic(score_hc)), 3)
  gow.dist <- round(sum((dist_mat - stats::cophenetic(score_hc))^2),
    2)
  # Print output to console
  message(paste0("With the selected agglomeration method (",
    score_hc$method, "), the cophonetic correlation coeffient is ",
    coph.corr, " and the Gower distance is ", gow.dist,
    "."))

  return(score_hc)

}
