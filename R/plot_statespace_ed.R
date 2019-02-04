#' Time series plot of Euclidean distance
#'
#' \code{plot_statespace_ed} generates a time series plot of the Euclidean
#' distance in indicator state space from a defined reference conditions.
#'
#' @param x The output tibble from the \code{\link{statespace_ed}} function.
#'
#' @return The function returns a \code{\link[ggplot2]{ggplot}} object.
#'
#' @family state assessment functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in the package
#' ind_sel <- ind_ex[,c(2,3,4,8,10,11)]
#'   # --> selection of complementary and well performing indicators
#' ed <- statespace_ed(x = ind_sel, time = ind_ex$Year, ref_time = ind_ex$Year[1])
#' plot_statespace_ed(x = ed)
#'
#' # To modify the plot:
#' p <- plot_statespace_ed(x = ed)
#' p + ggplot2::geom_point(col = "red") +
#'   ggplot2::ylab("Eucl. Distance") +
#'   ggplot2::geom_smooth(col="blue") +
#'   ggplot2::theme(axis.text = ggplot2::element_text(size = 16),
#'     axis.title=ggplot2::element_text(size = 18))
plot_statespace_ed <- function(x) {

  # Data input validation ---------
  if (missing(x)) {
    stop("Argument x is missing.")
  }
  x <- check_input_tbl(x, tbl_name = "x", parent_func = "statespace_ed()")
  # -------------------------------

  # Set general layout theme
  ggplot2::theme_set(ggplot2::theme_bw())

  edplot <- ggplot2::ggplot(x,
    ggplot2::aes(x = !!rlang::sym("time"), y = !!rlang::sym("ed"))) +
    ggplot2::geom_smooth(col = "firebrick3",
    fill = "cadetblue", alpha = 0.2) + ggplot2::geom_line(col = "black",
    size = 0.5) + ggplot2::geom_point(shape = 16,
    col = "black", size = 1.5) + ggplot2::ylab(paste0("Euclidean distance s from reference point (",
    x$time[which(x$ref_time == TRUE)], ")")) +
    ggplot2::xlab("") + # General layout
  		ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14))

  return(edplot)
}
