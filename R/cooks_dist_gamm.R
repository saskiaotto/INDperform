#' Calculates the Cook`s distance for Generalized Additive Mixed Models (GAMM)
#'
#' \code{cooks_dist_gamm} is a helper function for \code{\link{model_gamm}} and
#' \code{\link{plot_diagnostics}} and calculates the Cook`s distance for objects
#' of class `gamm`. The Cook's distance D is a leave-one-out deletion diagnostics
#' to measure the influence of each observation. The generic \code{cooks.distance}
#' function does not work on the class `gamm`.
#'
#' @param gamm_model The ‘gam‘ sublist from a model object of class \code{gamm}.
#'
#' @return
#' The function returns a numerical vector of the same length then the
#' data used for the GAMM fitting, with one D value for each IND observation.
#'
#' @seealso \code{\link{plot_diagnostics}}, \code{\link{model_gamm}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Using a model of the Baltic Sea demo data
#' cooks_dist_gamm(gamm_model = model_gamm_ex$model[[49]]$gam)
cooks_dist_gamm <- function(gamm_model) {

  suppressWarnings(
  	if ( all(is.na(gamm_model)) | is.null(gamm_model) ) {
    # Set cook for missing models to NA
    D <- NA
  } else {
    # Get x values
    mdf <- stats::model.frame(gamm_model)[, 2]
    # Count x values, calculate sd, mean, resids
    n <- length(mdf)
    k <- 1
    SS_x <- sum((mdf - mean(mdf, na.rm = TRUE))^2,
      na.rm = TRUE)
    mean_x <- mean(mdf, na.rm = TRUE)
    res <- mgcv::residuals.gam(gamm_model)
    df_error <- n - k - 1
    SS_error <- sum(res^2, na.rm = TRUE)
    MS_error <- SS_error/df_error
    h <- ((mdf - mean_x)^2/SS_x) + 1/n
    D <- (res^2/((k + 1) * MS_error)) * (h/(1 -
      h)^2)
  })

  D <- as.vector(D)

  return(D)
}
