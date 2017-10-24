#' Leave-One-Out Cross-Validation (LOOCV) procedure for (threshold-)GAMMs
#' - NOT WORKING YET!
#'
#' \code{loocv_thresh_gamm} applies a LOOCV on a threshold-GAMM and its
#' corresponding GAMM and returns TRUE if the threshold-GAMM has a lower
#' estimate, else FALSE (see for more infos on the LOOCV procedure
#' the details section in \code{\link{test_interaction}}).
#'
#' @inheritParams thresh_gamm
#'
#' @seealso \code{\link{thresh_gamm}} which creates a threshold-GAMM object and
#' \code{\link{test_interaction}} which applies thresh_gamm and loocv_thresh_gamm
#'
#' @examples
#' #rm(list = ls())
#' \dontrun{
#' ind_vec <- ind_init_ex$ind_train[[67]]
#' press_vec <- ind_init_ex$press_train[[67]]
#' t_var <- ind_init_ex$press_train[[63]]
#' name_t_var <- "Fher"
#' time <- ind_init_ex$time_train[[67]]
#' model <- merge_models_ex$model[[67]]
#' k <- 4; a <- 0.2; b <- 0.8
#'
#' #takes some time!
#' dat <- loocv_thresh_gamm(model, ind_vec, press_vec, t_var, name_t_var, k, a, b, time)
#' }
#'

loocv_thresh_gamm <- function(model, ind_vec, press_vec, t_var, name_t_var, k, a, b, time) {
	# Combine input vectors and create subsamples!
	data <- tibble::tibble(ind = ind_vec, press = press_vec, t_var = t_var, time = time)
	#split data as in loocv_thresh_gam
	data_train <- purrr::map(1:nrow(data), ~data[-., ])
	data_test <- purrr::map(1:nrow(data), ~data[., ])

	# Check if data was split correctly!
	if (!all(purrr::map2_lgl(.x = data_train, .y = data_test, ~all(c(.x$time, .y$time) %in% time)))) {
		stop("something bad happened!")
	}

	# Create gamms and thresh_gamms and predict them!
	#create tibble to save output
	dat <- tibble::tibble(gamms_pred   = vector(mode = "numeric", length = length(time)),
																							thresh_gamms_pred  = vector(mode = "numeric", length = length(time)),
																							observation  = vector(mode = "numeric", length = length(time)))


	#get model structure from input
	pass_p <- attr(model$lme$modelStruct$corStruct, "p")
	pass_q <- attr(model$lme$modelStruct$corStruct, "q")
	lmc <- nlme::lmeControl(niterEM = 5000, msMaxIter = 1000)
	value_tib <- tibble::tibble(p = c(1, 2, 1, 1, 2),
																													q = c(0, 0, 1, 2, 1),
																													values = list(c(-0.3), c(-0.3, 0.3), c(0.3, 0.3), c(0.3, -0.3, 0.3), c(0.3, 0.3, -0.3)))
	values <- value_tib$values[value_tib$p == pass_p & value_tib$q == pass_q][[1]]
	#family and link
	family <- mgcv::summary.gam(model$gam)$family[[1]]
	link <- mgcv::summary.gam(model$gam)$family[[2]]


	# Initialise progress bar
	pb <- dplyr::progress_estimated(length(dat$gamms_pred))

	for (i in seq_along(dat$gamms_pred)) {
		#create model input (formula cannot handle [[]])
		ind <- data_train[[i]]$ind
		press <- data_train[[i]]$press
		t_var <- data_train[[i]]$t_var
		time <- data_train[[i]]$time
		test <- data_test[[i]]
		temp <- data.frame(ind = ind,
																				press = press,
																				time = time)

		#model gamm
		gamm <- try(mgcv::gamm(formula = stats::as.formula(paste0("ind ~ 1 + s(press, k = ", k, ")")),
																					correlation = nlme::corARMA(value = values,
																																																	form = stats::as.formula("~time"),
																																																	p = pass_p, q = pass_q),
																					family = paste0(family,"(link = ",link,")"),
																					data = temp, control = lmc), silent = TRUE)

		#model thresh_gamm
		thresh_gamm <- thresh_gamm(model = model, ind_vec = ind, press_vec = press, t_var = t_var,
																													name_t_var = name_t_var, time = time, a = a, b = b)

		#caputre output
		dat$gamms_pred[[i]] <- try(mgcv::predict.gam(gamm$gam, newdata = test), silent = TRUE)
		test$modifier_level <- thresh_gamm$mr
		names(test)[1:3] <- c(all.vars(model$gam$formula), name_t_var)
		dat$thresh_gamms_pred[[i]] <- try(mgcv::predict.gam(thresh_gamm$gam, newdata = test), silent = TRUE)
		dat$observation[i] <- as.numeric(test[,1])
		# Increment progress bar
		pb$tick()$print()
	}
	# Stop progress bar
	pb$stop()

	#force values for missing models to be NA
	dat$gamms_pred  <- suppressWarnings(as.numeric(dat$gamms_pred))
	dat$thresh_gamms_pred <- suppressWarnings(as.numeric(dat$thresh_gamms_pred))
	calc_output <- function(pred, obs) {
		#difference between observated and predicted values
		res_diff <- pred - obs
		#square the diffrences
		res_squ <- res_diff^2
		#get a mean value
		res_mean <- mean(res_squ, na.rm = TRUE)
		#get the square root to the 4th decimal place
		res_root <- signif(res_mean^.5, 4)
		return(res_root)
	}
	if(is.na(calc_output(dat$thresh_gamms_pred, dat$observation))) {
		out <- FALSE
	} else {
		if(calc_output(dat$gamms_pred, dat$observation) > calc_output(dat$thresh_gamms_pred, dat$observation)) {
			out <- TRUE
		} else {
			out <- FALSE
		}
	}
	return(out)
}
