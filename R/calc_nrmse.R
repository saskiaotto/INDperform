#' Calculates the Normalized Root Mean Square Error (NRMSE) for a list of models
#'
#' \code{calc_nrmse} is a wrapper function that applies the \code{\link{nrmse}} function to a
#' list of models given the input indicator and pressure observations. The function calculates
#' first the predicted values for each model, which are then used for the NRMSE
#' computation. The normalization method and transformation types required for
#' \code{\link{nrmse}} can be set for all models the same or individually.
#'
#' @param press A list of vectors containing the pressure values.
#' @param ind  A list of with vectors containing the indicator values.
#' @param model  A list containing the models.
#' @param method A character string or vector of the same length as the model list
#' indicating the value(s) to be used for the normalization of the RMSE. The default is
#' the standard deviation, alternative methods are the \code{"mean"},
#' \code{"maxmin"} (difference between the maximum and minimum observed values) or \code{"iq"}
#' (interquartile) (see also \code{\link{nrmse}}).
#' @param transformation A character string or vector of the same length as the model list
#' indicating the type of transformation applied to the observations
#' prior to the analysis. Choose one of the following: \code{"none"} (default), \code{"sqrt"},
#' \code{"4thrt"} (fourth root), \code{"log"} (natural logarithm),
#' \code{"log10"} (common, i.e. base 10, logarithm),
#' \code{"log2"} (binary logarithm), \code{"log1p"} (i.e. log(1+x)),
#' \code{"arcsine"} (if data is proportional, NOT percentage) or \code{"other"}.
#' @param trans_function If \code{transformation} is set to "\code{"other"} for some or all models,
#' the function for the back-transformation needs to be defined here as single character string
#' (applied to all models ) or as character vector (with one string per model).
#' If no special transformation was applied use default setting \code{"none"}.
#'
#' @details
#' This wrapper function is used within the \code{\link{model_gam}} and
#' \code{\link{model_gamm}} functions with the default \code{"sd"} method and no transformation.
#' If another normalization is required or indicators where standardized prior to the analysis,
#' this wrapper function should be applied to the final model output tibble to compute
#' NRMSE that are based on the original indicator scale (advides for cross-indicator
#' comparisons).
#'
#' The more common transformation types applied to the indicator can be simply specified in the
#' \code{transformation} argument, which will invoke the respective back-transformation of the
#' observed and predicted indicator values before the NRMSE calculation. Any other transformation
#' applied should be indicated with  \code{transformation = "other"} and the respective
#' back-transformation (simply the inverse of the original transformation)
#' specified under \code{trans_function}, e.g. e.g. "5^x" if observations log(x, base = 5)
#' transformed or "exp(x) - 0.001" if observations log(x + 0.001) transformed.
#'
#' Missing values in obs and pred are removed before the computation proceeds, and
#' only those positions with non-missing values in both pred and obs are considered in the
#' computation.
#'
#' @return
#' The function returns a numerical vector of the same length then the
#' input lists, with one NRMSE value for each model.
#'
#' NOTE: If NA is returned for some models it means that either no model is available or that
#' not enough observations with both indicator and pressure values are available
#' (minimum of 2 required).
#'
#' @seealso \code{\link{nrmse}}, \code{\link{model_gam}} and \code{\link{model_gamm}}
#'
#' @export
#'
#' @examples
#' # Calculate NRMSE for the indicators TZA (~Fcod), which was let's say
#' # log(x+ 0.001)-transformed, and MS (~TSum), which was not transformed:
#' calc_nrmse(press = ind_init_ex$press_test[7:8], ind = ind_init_ex$ind_test[7:8],
#' model = model_gam_ex$model[7:8], method = "sd", transformation = c("other", "none"),
#' trans_function = c("exp(x) - 0.001", "none") )
calc_nrmse <- function(press, ind, model, method = "sd",
  transformation = "none", trans_function = "none") {

  # Data input validation
  if (missing(press)) {
    stop("Argument press is missing.")
  }
  if (missing(ind)) {
    stop("Argument ind is missing.")
  }
  if (missing(model)) {
    stop("Argument model is missing.")
  }
  # Check length of input
  if ((length(press) != length(ind)) | (length(press) !=
    length(model)) | (length(ind) != length(model))) {
    stop("The length of your input lists is not the same!")
  }

  # Check if all three arguments are of length 1
  # or have the same lenght as the input lists:
  if (length(method) != 1 & length(method) != length(model)) {
    stop("Argument 'method' has to be either 1 string (one common setting) or a character vector of the same length as ind~press combinations (one setting for each)!")
  }
  if (length(transformation) != 1 & length(transformation) !=
    length(model)) {
    stop("Argument 'transformation' has to be either 1 string (one common setting) or a character vector of the same length as ind~press combinations (one setting for each)!")
  }
  if (length(trans_function) != 1 & length(trans_function) !=
    length(model)) {
    stop("Argument 'trans_function' has to be either 1 string (one common setting) or a character vector of the same length as ind~press combinations (one setting\tfor each)!")
  }
  # Check if content is correct: (if length = 1, the
  # DIV routine of nrmse_trans will take care of
  # content)
  if (length(method) == length(model)) {
    if (any(!(unique(method) %in% c("mean", "sd",
      "maxmin", "iq"))))
      stop("At least one of your methods is wrong (see help for possible types)!")
  }
  if (length(transformation) == length(model)) {
    if (any(!(unique(transformation) %in% c("none",
      "sqrt", "4thrt", "log", "log10", "log2",
      "log1p", "arcsine", "other")))) {
      stop("At least one of your transformations is wrong (see help for possible transformations)!")
    }
  }


  # Input preparation ----------------

  if (length(method) == 1) {
    method_v <- rep(method, length(model))
  } else {
    method_v <- method
  }

  if (length(transformation) == 1) {
    transformation_v <- rep(transformation, length(model))
  } else {
    transformation_v <- transformation
  }

  if (length(trans_function) == 1) {
    transf_function_v <- rep(trans_function, length(model))
  } else {
    transf_function_v <- trans_function
  }

  # Now check again if transformation_v and
  # transf_function_v match
  if (any(transformation_v == "other" & transf_function_v ==
    "none")) {
    stop("You need to specify the trans_function for the back-transformation where you set transformation = 'other'!")
  }

  # Skip rows if no model available
  choose <- purrr::map(.x = model, ~!is.na(.)) %>%
    purrr::map(any) %>% unlist()


  # Calculation ----------------
  pred_ind <- calc_pred(model_list = model, obs_press = press)$pred

  out_nrmse <- vector(length = length(ind))

  for (i in seq_along(ind)) {
    if (choose[i]) {
      out_nrmse[i] <- suppressMessages(nrmse(pred = pred_ind[[i]],
        obs = ind[[i]], method_v[i], transformation_v[i],
        transf_function_v[i]))
    } else {
      out_nrmse[i] <- NA
    }
  }


  return(out_nrmse)

}

