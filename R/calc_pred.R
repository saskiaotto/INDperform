#' Calculate prediced values and their confidence intervals for GAM(M)s
#'
#' \code{calc_pred} is a helper function for \code{\link{model_trend}},
#' \code{\link{model_gam}}, \code{\link{model_gamm}} and
#' \code{\link{plot_model}}. It calculates for a list of Generalized Additive
#' (Mixed) Models the predicted IND values as well as the upper and lower
#' 95\% confidence intervals based on a list of corresponding
#' pressure values (observed or a regular spaced sequence).
#'
#' @param model_list A list of model objects of class `gam` or `gamm`.
#' @param obs_press A list of vectors with pressure values (observed or an
#'  artifical sequence).
#'
#' @return
#' The function returns a tibble with one row for each model and three columns:
#' \describe{
#'   \item{\code{pred}}{A list-column with the predicted IND values
#'              given the input pressure values.}
#'   \item{\code{ci_up}}{A list-column with the upper 95\% confidence limit
#'              of theIND predictions.}
#'   \item{\code{ci_low}}{A list-column with the lower 95\% confidence limit
#'              of the IND predictions.}
#' }
#'
#' @seealso \code{\link{model_trend}}, \code{\link{model_gam}},
#' \code{\link{model_gamm}}, \code{\link{plot_model}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data
#' model_list <- model_gam_ex$model
#' obs_press <- ind_init_ex$press_train
#' x <- calc_pred(model_list, obs_press)
calc_pred <- function(model_list, obs_press) {

  # Data input validation ------------------------------------
  # Test input length
  if (length(model_list) != length(obs_press)) {
    stop("obs_press has to be the same length as model_list!")
  }
  # ----------------------------------------------------------

  temp <- vector(mode = "list", length = length(obs_press))
  # for (i in 1:length(obs_press)) {
  for (i in seq_along(obs_press)) {
    # Create input for predict.gam
    dat <- tibble::tibble(ind = NA, press = obs_press[[i]])
    model <- model_list[i]

    # Check if model is avaliable
    if (is.null(model[[1]])) {
      temp[[i]] <- NA
    } else {
      # Create predictions for gams
      if (class(model[[1]])[1] == "gam") {
        # dat has to have the same names as used in formula
        names(dat) <- all.vars(model_list[[i]]$formula)
        temp[[i]] <- mgcv::predict.gam(object = model[[1]],
          type = "response", se.fit = TRUE,
          newdata = dat)
      } else {
        if (class(model[[1]])[1] == "gamm") {
          # Create predictions for gamms
          names(dat) <- all.vars(model_list[[i]]$gam$formula)
          temp[[i]] <- try(mgcv::predict.gam(object = model[[1]]$gam,
          type = "response", se.fit = TRUE,
          newdata = dat), silent = TRUE)
        } else {
          # NA cases in model_list (had to be at end of
          # condition)
          temp[[i]] <- NA
        }
      }
    }
  }

  # Look for missing models
  choose <- purrr::map_lgl(temp, is.list)
  # Calc predictions
  pred <- purrr::map_if(temp, choose, ~.$fit)
  # Calc upper confidence interval
  ci_up <- purrr::map_if(temp, choose, ~.$fit + .$se.fit *
    1.96)
  # Calc lower confidence interval
  ci_low <- purrr::map_if(temp, choose, ~.$fit -
    .$se.fit * 1.96)

  # Create an output tibble
  tib <- tibble::tibble(pred, ci_up, ci_low)

  return(tib)
}


