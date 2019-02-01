#' Normalized Root Mean Square Error
#'
#' \code{nrmse} is a function that allows the user to calculate the normalized root
#' mean square error (NRMSE) as absolute value between predicted
#' and observed values using different type of normalization methods.
#' It further allows the NRMSE calculation on the scale of the untransformed
#' indicator, which is advisable for a comparison across indicators.
#'
#' @param pred A vector of predicted values.
#' @param obs A vector of observed values.
#' @param method A character string indicating the value to be used for the normalization of the RMSE.
#' The default is the standard deviation. Alternatively, you can choose the \code{"mean"},
#' \code{"maxmin"} (difference between the maximum and minimum observed values) or \code{"iq"}
#' (interquartile)
#' @param transformation The type of transformation applied to the observations
#' prior to the analysis. Choose one of the following: \code{"none"} (default), \code{"sqrt"},
#' \code{"4thrt"} (fourth root),
#' \code{"log"} (natural logarithm), \code{"log10"} (common, i.e. base 10, logarithm),
#' \code{"log2"} (binary logarithm), \code{"log1p"} (i.e. log(1+x)),
#' \code{"arcsine"} (if data is proportional, NOT percentage) or \code{"other"}.
#' @param trans_function If \code{transformation} is set to "\code{"other"}, the function
#' for the back-transformation needs to be defined here as character string (simply the inverse of
#' the original transformation), e.g. "5^x" if observations log(x, base = 5) transformed or
#' "exp(x) - 0.001" if observations log(x + 0.001) transformed. Default is \code{"none"}.
#'
#' @details
#' The for most common normalization methods are implemented here:
#'
#' - the **mean**:   NRMSE = RMSE / mean(obs)
#' - the **standard deviation**:  NRMSE = RMSE / sd(obs)
#' - the **difference between maximum and minimum**:  NRMSE = RMSE / (max(obs)-min(obs)
#' - the **interquartile range**; NRMSE = RMSE / (Q1-Q3), i.e. the
#' difference between the 25th and 75th percentile of observations
#'
#' Missing values in obs and pred are removed before the computation proceeds, and
#' only those positions with non-missing values in both pred and obs are considered in the
#' computation.
#'
#' @return
#' The function returns a single NRMSE value (expressed as absolute value). In case the
#' number of positions with non-missing values in both pred and obs is less then 2, NA is returned
#' with a message.
#'
#' @seealso \code{\link{calc_nrmse}}
#'
#' @export
#'
#' @examples
#' obs <- c(10, 14, 20)
#' pred <- c(9, 12, 13)
#' # Calculating the sd-based NRMSE for untransformed data
#' nrmse(pred, obs)
#' # Calculating the iq-based NRMSE for log(x+0.001) transformed data
#' nrmse(pred, obs, method = "iq", transformation = "other", trans_function = "exp(x)-0.001")
nrmse <- function(pred, obs, method = "sd", transformation = "none",
  trans_function = "none") {

  # Data input validation ---------------------
  if (length(pred) != length(obs))
    stop(paste(length(obs), "-", length(pred),
      "The observation and prediction vectors do not have the same length."))
  if (!method %in% c("mean", "sd", "maxmin", "iq"))
    stop("You specified the wrong method!")
  if (!transformation %in% c("none", "sqrt", "4thrt",
    "log", "log10", "log2", "log1p", "arcsine",
    "other"))
    stop("You specified the wrong transformation!")
  if (transformation == "other" & trans_function ==
    "none")
    stop("You need to specify trans_function for the back-transformation if you set transformation = 'other'!")

  # Backtransform if needed ---------------------
  if (transformation == "sqrt") {
    obs <- obs^2
    pred <- pred^2
  }
  if (transformation == "4thrt") {
    obs <- obs^4
    pred <- pred^4
  }
  if (transformation == "log") {
    obs <- exp(obs)
    pred <- exp(pred)
  }
  if (transformation == "log10") {
    obs <- 10^obs
    pred <- 10^pred
  }
  if (transformation == "log2") {
    obs <- 2^obs
    pred <- 2^pred
  }
  if (transformation == "log1p") {
    obs <- expm1(obs)
    pred <- expm1(pred)
  }
  if (transformation == "arcsine") {
    obs <- sin(obs)^2
    pred <- sin(pred)^2
  }
  if (transformation == "other") {
    x <- obs
    obs <- eval(parse(text = trans_function))
    x <- pred
    pred <- eval(parse(text = trans_function))
  }

  # Data preparation -----------------

  # Use only those oberservations where obs and pred
  # have jointly no NAs!
  excl <- is.na(obs) | is.na(pred)
  obs_in <- obs[!excl]
  pred_in <- pred[!excl]

  if (length(obs_in) < 2) {
    out <- NA
    message("Not enough observations/predictions to calculate the NRMS, hence, NA returned.")
  } else {

    # Calculation of NRMSE ---------------------
    sq_sums <- sum((obs_in - pred_in)^2, na.rm = TRUE)
    mse <- sq_sums/length(obs_in)
    rmse <- sqrt(mse)

    # Normalization
    if (method == "sd")
      out <- rmse/stats::sd(obs_in, na.rm = TRUE)
    if (method == "mean")
      out <- rmse/mean(obs_in, na.rm = TRUE)
    if (method == "maxmin")
      out <- rmse/(max(obs_in, na.rm = TRUE) -
        min(obs_in, na.rm = TRUE))
    if (method == "iq") {
      out <- rmse/(stats::quantile(obs_in, 0.75, na.rm = TRUE) -
        stats::quantile(obs_in, 0.25, na.rm = TRUE))
      names(out) <- NULL
    }

    out <- abs(out)
  }

  return(out)
}

