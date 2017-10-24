#' Compare changes in t_var based on different threshold levels.
#'
#' \code{compare_thresholds} is a helper function for \code{\link{thresh_gam}}
#' and \code{\link{thresh_gamm}}. It test whether a specific treshold value
#' leads to a new splitting of trainings observations in the threshold
#' variable in comparison to the previous treshold value. Returns FALSE if
#' the change in the t-t_val does not result in a new grouping of threshold
#' values (above and below the t_val).
#'
#' @param t_val A vector with threshold values.
#' @param t_var A vector with pressure values of the threshold variable.
#'
#' @return
#' The function returns a tibble with one row for each t-val and two columns:
#' \describe{
#'   \item{\code{t_val}}{The input vector of threshold values.}
#'   \item{\code{change}}{logical; if TRUE, the respective threshold value
#'    lead to a new splitting of trainings observations in the threshold
#'    variable.}
#' }
#'
#' @seealso\code{\link{thresh_gam}}, \code{\link{thresh_gamm}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' t_var <- rnorm(20)
#' lower <- stats::quantile(t_var, prob = .2, na.rm = TRUE)
#' upper <- stats::quantile(t_var, prob = .8, na.rm = TRUE)
#' t_val <- seq(from = lower, to = upper, by = (upper - lower) / 20)
#' compare_thresholds(t_val, t_var)
#'
compare_thresholds <- function(t_val, t_var) {
  comp <- do.call(rbind, purrr::map(t_val,
  	~t_var <=  .))
  # If there is no change in the positions of TRUE in
  # comp in comparison to the last threshold, than
  # change will be set to FALSE
  thresh_gams <- tibble::tibble(t_val = t_val)
  thresh_gams$change <- TRUE
  for (i in 2:nrow(thresh_gams)) {
    if (sum(comp[i, ] - comp[i - 1, ],
    	 na.rm = TRUE) == 0) {
      thresh_gams$change[i] <- FALSE
    }
  }
  return(thresh_gams)
}
