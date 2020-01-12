#' Euclidean distance in state space
#'
#' \code{statespace_ed} generates a time series of Euclidean distances from
#'  defined reference conditions to assess the development of a suite of
#'  ecological state indicators.
#'
#' @param x A data frame, tibble, matrix or vector of selected indicator(s).
#' @param time A vector containing the actual time series.
#' @param ref_time The reference time (single point in time, e.g. specific
#'  year) on which to base the Euclidean distance. Default is set to the
#'  first time point.
#' @param na_rm A logical value indicating whether rows with NA values should
#'  be stripped before the computation proceeds. Default is set to TRUE. If set
#'  to FALSE and data contains NAs, the function returns NA.
#'
#' @details
#' This function implements an approach adopted from Tett \emph{et al.} (2013) to
#' assess changes in the ecosystem state by studying trajectories in state space.
#' State space is defined here as the n-dimensional space of possible locations of
#' state variables or indicators. For a robust suite of indicators the unidimensional
#' Euclidean distance between each year (or any other time step) and a reference
#' year in state space is calculated. That means, the function calculates the square
#' root of the sum of squared distances between each standardized indicator value
#' in a specific year and its reference value, which is defined by the user.
#'
#' @return
#' \code{statespace_ed} returns a tibble with the time vector \code{time}, the
#' Euclidean distance  \code{ed}, and a logical vector  \code{ref_time} indicating
#' the time step defined as reference.
#'
#' @references
#' Tett, P., Gowen, R.J., Painting, S.J., Elliott, M., Forster, R., Mills, D.K.,
#' Bresnan, E., Capuzzo, E., Fernandes, T.F., Foden, J., Geider, R.J., Gilpin, L.C.,
#' Huxham, M., McQuatters-Gollop, A.L., Malcolm, S.J., Saux-Picart, S., Platt, T.,
#' Racault, M.F., Sathyendranath, S., van der Molen, J., Wilkinson, M. (2013)
#' Framework for understanding marine ecosystem health. \emph{Marine Ecology
#' Progress Series.} 494, 1-27.
#'
#' @family state assessment functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in the package
#' ind_sel <- ind_ex[,c(2,3,4,8,10,11)]
#' # --> selection of complementary and well performing indicators
#' # There are different ways to define the reference time step:
#' ed <- statespace_ed(x = ind_sel, time = ind_ex$Year, ref_time = ind_ex$Year[1])
#' ed <- statespace_ed(x = ind_sel, time = ind_ex$Year, ref_time = 1987)
#' ed <- statespace_ed(x = ind_sel, time = ind_ex$Year, ref_time = "1987")
statespace_ed <- function(x, time, ref_time = NULL, na_rm = TRUE) {

  # Data input validation --------------------
	 if (missing(x)) {
	 	stop("Argument x is missing.")
	 }
	 if (missing(time)) {
	 	stop("Argument time is missing.")
	 }
	 # Check for correct input format
	 if (any(class(x) == "list")) {
	 	 stop("x cannot be a list.")
	 }
	 time <- check_input_vec(time, "argument time")
	 # (I added explicitly 'argument' to NOT test for regularity in time steps!)

	 if (na_rm == FALSE) {
	 	return(NA)
	 } else {
				y <- tibble::tibble(time = time)
	 	 keep <- stats::complete.cases(x)
	 	 x <- x[keep, ]
	 	 time <- time[keep]
	 	 if (is.null(ref_time)) {
    		id <- 1
  		} else {
	 	 		# Check if reference time still included and get new position
	 	 		if (!ref_time %in% time) {
	 		  		stop("The defined reference time is outside the time series OR by removing rows with NAs (if argument 'na.rm' set to TRUE) the reference time got also removed. Please choose another reference time or impute missing values.")
	 	 		} else {
	 		  		id <- which(time == ref_time)
	 	 		}
  		}
	 }

	 # --------------------------------

  # Standardize indicator time series
  x_stand <- scale(x)  # without year and sd

  # Calculate the n-dimensional Euclidean distance
  # between each time unit and the reference time
  # (can be starting point)
  df <- x_stand
  for (i in 1:nrow(x_stand)) {
    df[i, ] <- df[i, ] - x_stand[id, ]
  }
  calc_euc <- function(x) sqrt(sum(x^2))
  ed <- apply(df, FUN = calc_euc, MARGIN = 1)
  out <- tibble::tibble(time = time, ed = ed, ref_time = FALSE)
  out$ref_time[id] <- TRUE
  out <- suppressMessages( dplyr::left_join(y, out) )
  out$ref_time[is.na(out$ref_time)] <- FALSE


  ### END OF FUNCTION ###
  return(out)
}
