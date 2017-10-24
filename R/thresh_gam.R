#' Fits a GAM with a threshold-formulation
#'
#' \code{thresh_gam} fits a Generalized Additive Model (GAM) with a threshold
#' formulation using the \code{by} argument in the smoothing function
#' \code{\link[mgcv]{s}}:
#' gam(IND ~ s(pressure1, by = threshold_variable_low) +
#'  s(pressure 1, by = threshold threshold_variable_high)).
#' The threshold value is estimated from the data and chosen by minimizing
#' the GCV score (termed 'gcvv' in the threshold-GAM object) over an interval
#' defined by the lower and upper quantiles (see the \code{a} and \code{b}
#' arguments respectively) of the threshold variable.
#'
#' @param model A single GAM object from the model tibble needed to extract
#'  the family and the link function.
#' @param ind_vec A vector with the IND training observations (including or excluding
#'  defined outliers).
#' @param press_vec A vector with the training observations (including or excluding
#'  defined outliers) of pressure 1 (i.e. the orginal significant pressure in the
#'  GAM(M)).
#' @param t_var A vector with the training observations (including or excluding
#'  defined outliers) of the threshold variable (i.e. a second pressure variable).
#' @param name_t_var The name of the threshold variable (pressure 2). t_var will be
#'  named after this string in the model formula.
#' @param k Choice of knots (for the smoothing function \code{\link{s}}); the
#'   default is 4 to avoid over-parameterization.
#' @param a The lower quantile value of the selected threshold variable, which
#'   the estimated threshold is not allowed to exceed; the default is 0.2.
#' @param b The upper quantile value of the selected threshold variable, which
#'   the estimated threshold is not allowed to exceed; the default is 0.8.
#'
#' @details
#' \code{thresh_gam} creates first a sequence of evenly spaced threshold values
#' within theboundaries set by the lower and upper quantiles (defined by a and b).
#' For each threshold value that leads to a new splitting of the threshold
#' variables a threshold-GAM is applied: one smoothing function is applied
#' to only those observations where the threshold variable has been below the threshold
#' value for the given time step (year). A second smoothing function is applied to
#' observations where the threshold variable is above the prior defined threshold.
#' From the list of computed models the threshold-GAM with the lowest Generalized
#' Cross Validation (GCV) and its threshold value are selected and returned. For more
#' infos on thershold-GAMs see also the details section in \code{\link{test_interaction}}.
#'
#' @return
#' The function returns a \code{gam} object with the additional class "\code{tgam}".
#' All method functions for \code{gam} can be applied to this function. The object
#' has four additional elements:
#' \describe{
#'   \item{\code{mr}}{The threshold value of the best threshold-GAM.}
#'   \item{\code{mgcv}}{The GCV of the best threshold-GAM.}
#'   \item{\code{gcvv}}{A vector of the GCV values of all fitted threshold-GAMs.}
#'   \item{\code{t_val}}{A vector of all tested threshold values within the
#'              boundaries set by the lower and upper quantiles.}
#'   \item{\code{train_na}}{A logical vector indicating missing values.}
#' }
#'
#' @seealso \code{\link{test_interaction}} and \code{\link{loocv_thresh_gam}}
#'  which apply the function
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data in this package
#' test <- thresh_gam(model = model_gam_ex$model[[1]],
#'                     ind_vec = ind_init_ex$ind_train[[1]],
#'                     press_vec = ind_init_ex$press_train[[1]],
#'                     t_var = ind_init_ex$press_train[[2]],
#'                     name_t_var = "Ssum", k = 4, a = 0.2, b = 0.8)
thresh_gam <- function(model, ind_vec, press_vec, t_var, name_t_var, k, a, b) {

	nthd <- length(press_vec)

		lower <- stats::quantile(t_var, prob = a, na.rm = TRUE)
		upper <- stats::quantile(t_var, prob = b, na.rm = TRUE)

		t_val <- seq(from = lower, to = upper, by = (upper - lower) / nthd)
		#family and link
		family <- mgcv::summary.gam(model)$family[[1]]
		link <- mgcv::summary.gam(model)$family[[2]]

		thresh_gams <- compare_thresholds(t_val, t_var)
		#create input for the model
		dat <- data.frame(ind = ind_vec,
																				press = press_vec,
																				t_var = t_var)
		names(dat) <- c(all.vars(model$formula), name_t_var)
		thresh_gams$model <- vector(length = nrow(thresh_gams), mode = "list")

		for(i in 1:nrow(thresh_gams)) {
			if(thresh_gams$change[i]) {
				#create the model formula
				formula <- paste0(names(dat)[1],                                  #ind
                      " ~ 1 + s(", names(dat)[2],                     #press
                      ", by = I(1 * (", names(dat)[3],                #t_var
                      " <= ", round(thresh_gams$t_val[i], digits = 3),#level t_var
                      ")), k = ", k, ") + s(", names(dat)[2],          #press
																						", by = I(1 * (", names(dat)[3],                #t_var
																						" > ", round(thresh_gams$t_val[i], digits = 3), #level_t_var
																						")), k = ", k, ")")
				#create the model
				mod <- mgcv::gam(formula = stats::as.formula(formula), na.action = "na.omit",
																																								family = paste0(family,"(link = ",link,")"),
																																								nthd = nthd, a = a, b = b, data = dat)
				mod$original_data <- dat       #add original data
				mod$mr <- thresh_gams$t_val[i] #save mr here already because its to late later
				thresh_gams$model[[i]] <- mod
			}
		}

		# Extract gcvv from models and add gcvv for each model not generated
		gcvv <- vector(mode = "numeric", length = nrow(thresh_gams))
		gcvv[thresh_gams$change] <- purrr::map_dbl(thresh_gams$model[thresh_gams$change], ~.$gcv.ubre)
		for(i in 1:length(gcvv)) {
			if(gcvv[i] == 0) {
				gcvv[i] <- gcvv[i-1]
			}
		}
		# find best model
		thresh_gams <- thresh_gams[thresh_gams$change,]
		thresh_gams$gcvv <- purrr::map_dbl(thresh_gams$model, ~.$gcv.ubre)
		# Extract model with the lowest gcvv score overall. In case of identical values, select chronologically.
		# We can easily debug this code!
		best_model_id <- min(which(thresh_gams$gcvv == min(thresh_gams$gcvv)))
		#create output
		if (length(best_model_id) == 1) {
			 res <- thresh_gams$model[[best_model_id]]
			 res$mgcv    <- thresh_gams$gcvv[best_model_id]
			 res$gcvv    <- gcvv[order(t_val)]
			 res$t_val <- sort(t_val)
			 class(res) <- c("thresh_gam", "gam", "glm", "lm")
				return(res)
		} else {
			stop("No thresh_gam available!")
		}
}
