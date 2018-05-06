#' Dendrogram of the cluster analysis.
#'
#' \code{plot_clust_sc} generates from the cluster analysis a dendrogram based on
#' the \code{ggplot2} and \code{ggdendro} packages.
#'
#' @param x Output from the cluster analysis (object of class \code{\link{hclust}}).
#' @param rotate If TRUE, rotates plot by 90 degrees.
#' @param text_size Size of the title and axes labels.
#'
#' @return The function returns a \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[ggdendro]{dendro_data}}
#' for the data extraction from the \code{hclust} object to produce the dendrogram
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
#' plot_clust_sc(clust_analysis)
#'
#' # To modify the plot:
#' plot_clust_sc(clust_analysis, rotate = TRUE, text_size = 20) +
#'  ggplot2::theme(title = ggplot2::element_text(colour = "blue", size = 10))
plot_clust_sc <- function(x, rotate = FALSE, text_size = 15) {

	 # Data input validation --------
	 # 'dendro_data' works only with specific objects:
	 if (!class(x) %in% c("hclust", "dendrogram",
	 	"tree", "rpart", "agnes", "diana", "twins")) {
	 	 stop("'x' is not an object than contains dendrogram information (should be of class hclust, dendrogram. tree, rpwart, agnes, diana, twins). Use the output of clust_sc() function here!")
	 }
	 # ------------------------------

  dataClass <- if (inherits(x, "dendro")) {
    x$class
  } else {
    class(x)
  }
  angle <- if (dataClass %in% c("dendrogram", "hclust")) {
    ifelse(rotate, 0, 90)
  } else {
    ifelse(rotate, 90, 0)
  }
  hjust <- if (dataClass %in% c("dendrogram", "hclust")) {
    ifelse(rotate, 0, 1)
  } else {
    0.5
  }

  # Extract the data (for rectangular lines)
  if (!ggdendro::is.dendro(x))
    data <- ggdendro::dendro_data(x)


  p <- ggplot2::ggplot() + ggplot2::geom_blank()
  p <- p + ggplot2::geom_segment(
  	 data = ggdendro::segment(data),
    ggplot2::aes_string(x = "x", y = "y", xend = "xend",
      yend = "yend"))
  p <- p + ggplot2::scale_x_continuous(
  	 breaks = seq_along(data$labels$label),
    labels = data$labels$label)
  if (rotate) {
    p <- p + ggplot2::coord_flip()
    p <- p + ggplot2::scale_y_continuous()
  } else {
    p <- p + ggplot2::scale_y_continuous()
  }
  p <- p + ggplot2::labs(title = "Similarities in performance across indicators")

  # Modify plot elements
  p <- p + ggplot2::theme(
  	 title = ggplot2::element_text(size = text_size),
    axis.text.x = ggplot2::element_text(angle = angle,
      hjust = 1, size = text_size),
  	 axis.text.y = ggplot2::element_text(angle = 0,
      hjust = 1, size = text_size),
  	 axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
  	 axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
  	 panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank()
  	 )


  ### END OF FUNCTION
  return(p)
}




