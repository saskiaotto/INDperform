#' Place annotations to a specified position
#'
#' \code{place_text} is a helper function for \code{\link{plot_trend}}
#' and \code{\link{plot_model}} to generate the x and y coordinates
#' for placing annotations in the ggplot depending on the specified
#' position.
#'
#' @param x The complete vector of x values in the ggplot.
#' @param y The complete vector of y values in the ggplot.
#' @param x_range The required width of the text box. Numeric value
#'  between 0 and 1; default is 0.1.
#' @param y_range The required height of the text box. Numeric value
#'  between 0 and 1; default is 0.2.
#' @param pos Specifies the corner in which to place the annotation.
#'  This must be one of "topleft", "topright", "bottomleft" and
#'  "bottomright"; default is "topleft".
#'
#' @return
#' The function returns a vector with the x and y coordinates
#' depending on the selected position.
#'
#' @seealso \code{\link{plot_trend}}, \code{\link{plot_model}}
#'
#' @keywords internal
#' @export
#'
place_text <- function(x, y, x_prop = 0.1, y_prop = 0.1, pos) {
	 if (!pos %in% c("topleft", "topright", "bottomleft", "bottomright") ) {
	 	 stop("'pos' has to be one of the following: topleft, topright, bottomleft or bottomright.")
	 } else {
				if(pos == "topleft") {
						out <- data.frame(x = x[1] + (x[2] - x[1]) * x_prop,
																							 y = y[2] - (y[2] - y[1]) * y_prop )
				}
				if(pos == "topright") {
					 out <- data.frame(x = x[2] - (x[2] - x[1]) * x_prop,
																							 y = y[2] - (y[2] - y[1]) * y_prop )
				}
				if(pos == "bottomleft") {
					 out <- data.frame(x = x[1] + (x[2] - x[1]) * x_prop,
																							 y = y[1] + (y[2] - y[1]) * y_prop )
				}
				if(pos == "bottomright") {
					 out <- data.frame(x = x[2] - (x[2] - x[1]) * x_prop,
																							 y = y[1] + (y[2] - y[1]) * y_prop )
				}
	 }
		return(out)
}
