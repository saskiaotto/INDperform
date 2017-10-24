#' Compute (partial) autocorrelation functions and test for significance
#'
#' \code{test_tac} is a helper function for \code{\link{model_gam}},
#' \code{\link{model_gamm}}, and \code{\link{plot_diagnostics}} to compute
#' the (partial) autocorrelation functions. It also tests whether residuals
#' show temporal autocorrelation (see details).
#'
#' @param model_resid A list of residuals from one or many Generalized
#'  Additive (Mixed) Models (GAM(M).
#'
#' @details
#' NOTE: if the time series on which the GAM(M) is fitted contains missing values,
#' they need to be accounted for in the residual vector. Observations with
#' one or more NAs inbetween will be otherwise considered as having a lag of 1.
#'
#' The test for temporal autocorrelation is based on the following condition: If
#' any of the acf \strong{and} any of the pacf values of lag 1 - 5 is greater or
#' lower than 0.4, a TRUE is returned.
#'
#' @return
#' The function returns a tibble with one row for each model and three columns:
#' \describe{
#'   \item{\code{acf}}{A list-column with values from the autocorrelation function.}
#'   \item{\code{pacf}}{A list-column with values from the partial autocorrelation
#'              function.}
#'   \item{\code{tac}}{logical; if TRUE, temporal autocorrelation was detected}
#' }
#'
#' @seealso \code{\link{model_gam}}, \code{\link{model_gamm}},
#' \code{\link{plot_diagnostics}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Using models of the Baltic Sea demo data:
#' # Get model residuals of GAMs
#' model_resid <- purrr::map(model_gam_ex$model,
#'   ~mgcv::residuals.gam(., type = "deviance"))
#' # test whether model residuals show significant TAC
#' test_tac(model_resid)
#'
#' # Works also with GAMMs
#' model_resid <- purrr::map_if(model_gamm_ex$model,
#'   !is.na(model_gamm_ex$model),
#'   ~as.numeric(mgcv::residuals.gam(.$gam, type = "deviance")))
#'   # (exclude those GAMMs that were not fitted)
#' # test whether model residuals show significant TAC
#' test_tac(model_resid)$tac
test_tac <- function(model_resid) {

	# Check first for which models do we have residuals
	# (i.e. where do we have models)
	choose <- purrr::map_lgl(model_resid, is.numeric)
	# Get acf values
	acf_val  <- purrr::map_if(model_resid, choose, ~as.vector(stats::acf(., na.action = stats::na.pass, plot = FALSE)$acf))
	# Get pacf values
	pacf_val <- purrr::map_if(model_resid, choose, ~as.vector(stats::pacf(., na.action = stats::na.pass, plot = FALSE)$acf))

	# Is there temporal autocorrelation? TRUE = tac occurs
	tac <- purrr::map2_lgl(.x = acf_val, .y = pacf_val,
		~any((abs(.x[2:6]) > .4) &	(abs(.y[1:5]) > .4), na.rm = TRUE))
 # NAs are automatically defined as FALSE --> convert manually to NAs
	tac[which(is.na(model_resid))] <- NA

	# Create output tibble
	res <- tibble::tibble(acf = acf_val,
																							pacf = pacf_val,
																							tac = tac)
	return(res)
}


test_tac <- function(model_resid) {

  # Check first for which models do we have residuals
  # (i.e. where do we have models)
  choose <- purrr::map_lgl(model_resid, is.numeric)
  # Get acf values
  acf_val <- purrr::map_if(model_resid, choose,
  	 ~as.vector(stats::acf(., na.action = stats::na.pass,
      plot = FALSE)$acf))
  # Get pacf values
  pacf_val <- purrr::map_if(model_resid, choose,
    ~as.vector(stats::pacf(., na.action = stats::na.pass,
      plot = FALSE)$acf))

  # Is there temporal autocorrelation? TRUE = tac occurs
  tac <- purrr::map2_lgl(.x = acf_val, .y = pacf_val,
    ~any((abs(.x[2:6]) > 0.4) & (abs(.y[1:5]) >
      0.4), na.rm = TRUE))
  # NAs are automatically defined as FALSE -->
  # convert manually to NAs
  tac[which(is.na(model_resid))] <- NA

  # Create output tibble
  res <- tibble::tibble(acf = acf_val, pacf = pacf_val,
    tac = tac)
  return(res)
}

