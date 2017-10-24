#' Fits a GAMM with a threshold-formulation - NOT WORKING YET!
#'
#' \code{thresh_gamm} fits a Generalized Additive Mixed Model (GAMM) with a threshold
#' formulation. The threshold value is estimated from the data and chosen by minimizing
#' the GCV score (termed 'gcvv' in the threshold-GAMM object) over an interval defined
#' by the lower and upper quantiles (see the \code{a} and \code{b} arguments
#' respectively) of the threshold variable. The correlation structure included is based
#' on the original IND~pressure GAMM.
#'
#' @inheritParams thresh_gam
#' @param time A vector containing the actual time series.
#'
#' @seealso \code{\link{test_interaction}} which applies thresh_gamm
#'  and \code{\link{loocv_thresh_gamm}}
#'
#' @return
#' The function returns a \code{gamm} object with the additional class "\code{tgamm}".
#' The object has four additional elements in the \code{gam} list :
#' \describe{
#'   \item{\code{mr}}{The threshold value of the best threshold-GAM.}
#'   \item{\code{mgcv}}{The GCV of the best threshold-GAM.}
#'   \item{\code{gcvv}}{A vector of the GCV values of all fitted threshold-GAMs.}
#'   \item{\code{t_val}}{A vector of all tested threshold values within the
#'              boundaries set by the lower and upper quantiles.}
#'   \item{\code{train_na}}{A logical vector indicating missing values.}
#' }
#'
#' @examples
#' #rm(list = ls())
#' \dontrun{
#' ind_vec <- ind_init_ex$ind_train[[43]]
#' press_vec <- ind_init_ex$press_train[[43]]
#' t_var <- ind_init_ex$press_train[[2]]
#' name_t_var <- "Ssum"
#' time <- ind_init_ex$time_train[[43]]
#' model <- model_gamm_ex$model[[2]]
#' k <- 4; a <- 0.2; b <- 0.8
#' dat <- thresh_gamm(model, ind_vec, press_vec, t_var, name_t_var, k, a, b, time)
#' }

thresh_gamm <- function (model, ind_vec, press_vec, t_var, name_t_var, k, a, b, time) {
	lmc <- nlme::lmeControl(niterEM = 5000, msMaxIter = 1000)

	#get correlation from the input
	pass_p <- attr(model$lme$modelStruct$corStruct, "p")
	pass_q <- attr(model$lme$modelStruct$corStruct, "q")
	#family and link
	family <- mgcv::summary.gam(model$gam)$family[[1]]
	link <- mgcv::summary.gam(model$gam)$family[[2]]

	value_tib <- tibble::tibble(p = c(1, 2, 1, 1, 2),
																													q = c(0, 0, 1, 2, 1),
																													values = list(c(-0.3), c(-0.3, 0.3), c(0.3, 0.3), c(0.3, -0.3, 0.3), c(0.3, 0.3, -0.3)))
	values <- value_tib$values[value_tib$p == pass_p & value_tib$q == pass_q][[1]]

	nthd <- length(t_var)    #length of x values!!!
	lower <- stats::quantile(t_var, prob = a, na.rm = TRUE)
	upper <- stats::quantile(t_var, prob = b, na.rm = TRUE)

	#create some dummy values to start with
	rv <- NULL
	rsqv <- NULL
	mrsq <- 1e-06
	mr <- NA

	#create interaction values
	t_val <- seq(lower, upper, (upper - lower) / nthd)

	thresh_gamms <- compare_thresholds(t_val, t_var)
	thresh_gamms$model <- vector(mode = "list", length = nrow(thresh_gamms))
	thresh_gamms$r_sq <- NA


	#ein thresh_gamm für jeden t_val
	for(i in 1:nrow(thresh_gamms)) {
		#create input tbl
		input <- tibble::tibble(ind = ind_vec,
																										press = press_vec,
																										t_var = t_var,
																										t_val = rep(round(t_val[i], digits = 3), times = length(t_val) - 1),
																										time = time)
		#change names
		names(input)[1:3] <- c(all.vars(model$gam$formula), name_t_var)
		if(thresh_gamms$change[i]) {
			#create formula
			formula <- paste0(names(input)[1],
                     " ~ 1 + s(", names(input)[2],
                     ", by = I(1 * (", names(input)[3],
                     " <= t_val)), k = ", k, ") + s(", names(input)[2],
																					", by = I(1 * (", names(input)[3],
																					" > t_val)), k = ", k, ")")
			#create model
			mod <- try(mgcv::gamm(formula = stats::as.formula(formula),
																									na.action = "na.omit", nthd = nthd, a = a, b = b,
																									family = paste0(family,"(link = ",link,")"),
																									correlation = nlme::corARMA(value = values,
																																																					form = stats::as.formula("~time"),
																																																					p = pass_p, q = pass_q),
																									control = lmc, data = input), silent = TRUE)
			#add original data
			try(mod$gam$original_data <- input, silent = TRUE)
			thresh_gamms$model[[i]] <- mod
		}
	}
	choose <- purrr::map_lgl(thresh_gamms$model, ~class(.)[1] == "gamm")
	for(i in 1:length(choose)) {
		if(choose[i]) {
			#only models that could be fitted are selected by choose
			thresh_gamms$r_sq[i] <- mgcv::summary.gam(thresh_gamms$model[[i]]$gam)$r.sq
		}
		if(!thresh_gamms$change[i]) {
			#each model not performed will get the r_sq value from the last model before
			thresh_gamms$r_sq[i] <- thresh_gamms$r_sq[i-1]
		}
	}

	#get best thresh_gamm
	best_model_id <- suppressWarnings(min(which(thresh_gamms$r_sq == max(thresh_gamms$r_sq, na.rm = TRUE))))
	#create output
	out <- thresh_gamms$model[best_model_id]
	out$mr <- thresh_gamms$t_val[best_model_id]
	out$mrsq <- thresh_gamms$r_sq[best_model_id]
	out$rv <- sort(thresh_gamms$t_val)
	out$rsqv <- thresh_gamms$r_sq[order(thresh_gamms$t_val)]

	class(out) <- c("thresh_gamm", "gamm", "gam", "lme")

	return(out)
}
