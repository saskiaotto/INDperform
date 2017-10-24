#' Convex hull plot
#'
#' \code{plot_statespace_ch} generates a scatter plot of all observed combinations
#' in the 2-dimensional indicator space including the convex hull from defined
#' reference conditions and the current period.
#'
#' @param x An object from the \code{\link{statespace_ch}} function.
#' @param col_ch_ref Colour of reference period (for points, path, and labels).
#' @param col_ch_cur Colour of current period (for points, path, and labels).
#' @param size_time Text size of the time labels (both periods).
#'
#' @return The function returns a \code{\link[ggplot2]{ggplot}} object.
#'
#' @family state assessment functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in the package
#' x <- statespace_ch(x = ind_ex$TZA, y = ind_ex$MS,
#'   time = ind_ex$Year, period_ref = 1979:1983,
#'   period_current = 2004:2008)
#' plot_statespace_ch(x)
#'
#' # To modify the plot:
#' p <- plot_statespace_ch(x, col_ch_ref = "green4", col_ch_cur = "orange3",
#'   size_time = 6)
#' p + ggplot2::xlab("TZA") + ggplot2::ylab("MS") +
#'   ggplot2::ggtitle("Deviation of current state from reference period") +
#'   ggplot2::theme(plot.title = ggplot2::element_text(size = 10)) +
#'   ggplot2::theme(axis.text = ggplot2::element_text(size = 6),
#'     axis.title = ggplot2::element_text(size = 20))
plot_statespace_ch <- function(x, col_ch_ref = "red",
  col_ch_cur = "blue", size_time = 4) {

  # Get indices of selected periods
  index_ref <- match(x$period_ref, x$time)
  index_current <- match(x$period_current, x$time)

  # Data preparation for plots
  time_char <- as.character(x$time)
  xy_full <- x$xy
  xy_ref <- xy_full %>% dplyr::slice(index_ref)
  xy_cur <- xy_full %>% dplyr::slice(index_current)
  xrange <- c((min(x$xy$x) - abs(min(x$xy$x) * 0.025)),
    (max(x$xy$x) + abs(max(x$xy$x)) * 0.025))
  yrange <- c((min(x$xy$y) - abs(min(x$xy$y) * 0.025)),
    (max(x$xy$y) + abs(max(x$xy$y)) * 0.025))
  xnudge <- diff(xrange)/50
  ynudge <- diff(yrange)/50

  # Proportion shown as annotation
  in_prop <- round((sum(x$inside_ch_ref) /
    length(x$inside_ch_ref) * 100), 0)


  # Plot -----------------------------------

  # Set general layout theme
  ggplot2::theme_set(ggplot2::theme_bw())

  # Scatter plot
  chplot <- ggplot2::ggplot(xy_full,
  	   ggplot2::aes_(~x, ~y)) +
  	 ggplot2::geom_point(shape = 1, col = "black") +
    ggplot2::xlab("x") + ggplot2::ylab("y") +
  	 ggplot2::xlim(xrange) +
    ggplot2::ylim(yrange)

  # Add convex hull of reference period
  chplot <- chplot +
  	 ggplot2::geom_point(data = xy_ref, shape = 19,
      col = col_ch_ref) +
  	 ggplot2::geom_path(data = xy_ref[x$ch_ref, ],
      col = col_ch_ref) +
  	 ggplot2::geom_text(data = xy_ref,
      label = time_char[index_ref], col = col_ch_ref,
      nudge_x = xnudge, nudge_y = ynudge, size = size_time)

  # Add convex hull of current period
  chplot <- chplot +
  	 ggplot2::geom_point(data = xy_cur, shape = 19,
      col = col_ch_cur) +
  	 ggplot2::geom_path(data = xy_cur[x$ch_cur, ],
      col = col_ch_cur) +
  	 ggplot2::geom_text(data = xy_cur,
      label = time_char[index_current], col = col_ch_cur,
      nudge_x = xnudge, nudge_y = ynudge, size = size_time)

  # General layout
  chplot <- chplot +
  	 ggplot2::ggtitle(paste0("Proportion of recent time period within reference space: ",
      in_prop, " %")) +
  	 ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(lineheight = 0.8,
      face = "plain", size = 16),
  	 	 axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14)
  	 	)

  ### end of function ###
  return(chplot)

}
