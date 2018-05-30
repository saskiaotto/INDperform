#' Convex hull in 2-dimensional space
#'
#' \code{statespace_ch} calculates the convex hull in 2-dimensional space, e.g.
#' for two selected indicators, using the \code{\link[tripack]{tri.mesh}}
#' function.
#'
#' @param x The coordinates of points in the first dimension
#'  (e.g. indicator 1 or PC1 scores from a PCA).
#' @param y The coordinates of points in the second dimension
#'  (e.g. indicator 2 or the PC2 scores from a PCA).
#' @param time A vector containing the actual time series.
#' @param period_ref Vector of time units (e.g. years) used as reference
#'  period (minimum of 3 time units required).
#' @param period_current Vector of time units (e.g. years) used as current
#'  period to compare with the reference period (minimum of 3 time units
#'  required).
#'
#' @details
#' \code{statespace_ch} implements a second state space approach to assess
#' the development of a suite of ecological state indicators
#' (Otto \emph{et al.} 2018, Tett \emph{et al.} 2008). While unidimensional
#' approaches such as the Euclidean distance (see \code{\link{statespace_ed}})
#' feature the disadvantage of defining one particular year or time step as
#' reference condition, this approach accounts for inter-annual variation by
#' defining a reference domain in state space based on several years: more
#' recent observations might be characterized as either within or outside this
#' domain.
#'
#' The reference domain can be described by a convex hull, which is a multivariate
#' measure derived from computational geometry representing the smallest convex set
#' containing all the points in Euclidean plane or in Euclidean space
#' (de Berg \emph{et al.}, 2008). While the convex hull can be calculated for
#' high-dimensional data, reducing the space to two dimensions allows for an easier
#' visualization and interpretation. Therefore, the `statespace_ch` function only
#' calculates the convex hull for two dimensions, i.e. for two indicators or principal
#' axes obtained by multivariate analysis such as a Principal Component Analysis
#' (PCA).
#'
#' @return
#' The function returns a list with the following elements
#' \describe{
#'   \item{\code{ch_ref}}{A vector of the position of the convex hull of the
#'   reference period.}
#'   \item{\code{ch_cur}}{A vector of the position of the convex hull of the
#'   current period.}
#'   \item{\code{inside_ch_ref}}{A logical vector indicating whether each
#'   year (time step) of the current period lies inside (TRUE) or outside
#'   (FALSE) the state space domain of the reference period.}
#'   \item{\code{xy}}{A dataframe of the x and y coordinates.}
#'   \item{\code{time}}{A vector of the full time series.}
#'   \item{\code{period_ref}}{A vector of years (time steps) defined as
#'   the reference period.}
#'   \item{\code{period_current}}{A vector of years (time steps) defined as
#'   the current period.}
#' }
#'
#'
#' @references
#' de Berg, M., Cheong, O., van Kreveld, M., Overmars, M. (2008) Computational
#' Geometry - Algorithms and Applications. Springer Berlin Heidelberg, 386pp.
#'
#' Otto, S.A., Kadin, M., Casini, M., Torres, M.A., Blenckner, T. (2018)
#' A quantitative framework for selecting and validating food web indicators.
#' \emph{Ecological Indicators}, 84: 619-631,
#' doi: https://doi.org/10.1016/j.ecolind.2017.05.045
#'
#' Tett, P., Carreira, C., Mills, D.K., van Leeuwen, S., Foden, J., Bresnan, E.,
#' Gowen, R.J. (2008) Use of a Phytoplankton Community Index to assess the
#' health of coastal waters. \emph{ICES Journal of Marine Science} 65, 1475-1482.
#'
#' @seealso \code{\link[tripack]{tri.mesh}} for the computation of the convex hull.
#' @family state assessment functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in the package
#' time <- ind_ex$Year
#' period_ref <- 1979:1983
#' period_current <- 2004:2008
#'
#' # Apply function on 2 indicators
#' ch <- statespace_ch(x = ind_ex$TZA, y = ind_ex$MS,
#'  time, period_ref, period_current)
#'
#' # Conduct PCA on selected indicators using the correlation matrix (scale=T)
#' ind_sel <- ind_ex[,c(2,3,4,8,10,11)]
#' pca_out <- vegan::rda(ind_sel, scale=TRUE)
#' pca_sum <- summary(pca_out)
#' prop_expl <- as.vector(pca_sum$cont$importance[2,])
#' scores_unsc <- vegan::scores(pca_out, scaling = 0)
#' scores_sites <- as.data.frame(scores_unsc$sites)
#' x <- scores_sites$PC1
#' y <- scores_sites$PC2
#' # Apply function
#' ch <- statespace_ch(x, y, time, period_ref, period_current)
statespace_ch <- function(x, y, time, period_ref, period_current) {

  # Data input validation -----------------------

	 # Testing for required package installation
  if (!requireNamespace("tripack", quietly = TRUE)) {
    stop("The package 'tripack' is needed for this function to work. Please install it.",
      call. = FALSE)
  }

		if (missing(x)) {
	 	stop("Argument 'x' is missing.")
		}
		if (missing(y)) {
	 	stop("Argument 'y' is missing.")
		}
	 if (missing(time)) {
	 	stop("Argument 'time' is missing.")
		}
	 if (missing(period_ref)) {
	 	stop("Argument 'period_ref' is missing.")
	 }
	 if (missing(period_current)) {
	 	stop("Argument 'period_current' is missing.")
	 }

  # Check input vectors
	 x <- check_input_vec(x, "x")
	 y <- check_input_vec(y, "y")
	 time <- check_input_vec(time, "time")

	 if (length(x) != length(y) | length(x) != length(time)) {
	 	 stop("One of the x, y, or time vectors has a different length!")
	 }

  # Testing input of both periods
  if (any(period_ref %in% time == FALSE) | any(period_current %in%
    time == FALSE)) {
    stop("At least one of the defined periods is (to some extent) outside the given time series.")
  }

  if (length(period_ref) <= 2 | length(period_current) <=
    2) {
    stop("At least one of the defined periods has not a minimum of 3 time units.")
  }

	 # Check if periods contain any NA
	 if (any(is.na(x[match(period_ref, time)])) | any(is.na(y[match(period_ref, time)]))) {
	 	stop(paste0("One of your indicators (x and/or y) has missing values in the reference ",
    "period. Please select another period or fill the missing values (e.g. with the mean, ",
				"median or interpolate)."))
	 }
	 if (any(is.na(x[match(period_current, time)])) | any(is.na(y[match(period_current, time)]))) {
	 	stop(paste0("One of your indicators (x and/or y) has missing values in the current ",
    "period. Please select another period or fill the missing values (e.g. with the mean, ",
				"median or interpolate)."))
	 }


  # --------------------------------------------

  index_ref <- match(period_ref, time)
  index_current <- match(period_current, time)

  # Convex hull computation
  tr <- tripack::tri.mesh(x = x[index_ref], y = y[index_ref])
  inside_ch_ref <- tripack::in.convex.hull(tr, x = x[index_current],
    y = y[index_current])

  # Helper function 'convexhull' (returns convex hull
  # around data points)
  convexhull <- function(xcoord, ycoord) {
    # xcoords = x ycoord = y
    hpts <- grDevices::chull(x = xcoord, y = ycoord)
    hpts <- c(hpts, hpts[1])
    return(hpts)
  }

  # Get position of convex hull
  ch_ref <- convexhull(xcoord = x[index_ref], ycoord = y[index_ref])
  ch_cur <- convexhull(xcoord = x[index_current],
    ycoord = y[index_current])

  # Export list for plot
  out <- list(ch_ref = ch_ref, ch_cur = ch_cur, inside_ch_ref = inside_ch_ref,
    xy = data.frame(x, y), time = time, period_ref = period_ref,
    period_current = period_current)

  ### end of function ###
  return(out)
}

