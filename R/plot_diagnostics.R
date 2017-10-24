#' Diagnostic plots for a fitted GAM, GAMM or threshold-GAM(M)
#'
#' \code{plot_diagnostics} takes a list of models of class 'gam', 'gamm',
#' 'thresh_gam' or 'thresh_gamm' or a mix of those and produces some diagnostic
#' information of the fitting procedure and results. The function returns a
#' tibble with 6 list-columns containing individual plots (ggplot2 objects)
#' and one list-column containing a plot that shows all diagnostic plots
#' together.
#'
#' @param model_list A list with models of class gam(m) and/or thresh_gam(m),
#'  e.g. the list-column \code{model} from the \code{\link{model_gam}} output
#'  tibble.
#'
#' @details
#' The function can deal with any model of the classes 'gam', 'gamm',
#' 'thresh_gam' or 'thresh_gamm as long as the input is a flat list. That means:
#' \itemize{
#'   \item If only one model is provided as input coerce the model explicitely
#'         to class 'list'. An input such as model_gam_ex[1, "model"] will not work
#'         as the class is a tibble. Use instead model_gam_ex$model[1].
#'   \item If the input are one or more treshold-GAMs selected from
#'         the \code{\link{test_interaction}} output (variable \code{thresh_models}
#'         the model list features a nested structure:
#'         each IND~pressure pair (row) might have more than one threshold-GAM.
#'         To remove the nested structure use e.g. the \code{\link[purrr]{flatten}}
#'         function (see examples).
#' }
#'
#' @return
#' The function returns a \code{\link[tibble]{tibble}}, which is a trimmed down version
#' of the data.frame(), including the following elements:
#' \describe{
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{cooks_dist}}{A list-column of ggplot2 objects that show the
#'               cooks distance of all observations, which is a leave-one-out
#'               deletion diagnostics to measure the influence of each
#'               observation. Data points with a large Cook's distance (> 1)
#'               are considered to merit closer examination in the analysis.}
#'   \item{\code{acf_plot}}{A list-column of ggplot2 objects that show  the
#'               autocorrelation function for the residuals. NAs in the time
#'               series due to real missing values, test data extraction or
#'               exlusion of outliers are explicitely considered.}
#'   \item{\code{pacf_plot}}{A list-column of ggplot2 objects that show the
#'               partial autocorrelation function for the residuals. NAs are
#'               explicitely considered.}
#'   \item{\code{resid_plot}}{A list-column of ggplot2 objects that show residuals
#'               vs. fitted values.}
#'   \item{\code{qq_plot}}{A list-column of ggplot2 objects that show  the
#'              quantile-quantile plot for normality.}
#'   \item{\code{gcvv_plot}}{A list-column of ggplot2 objects that show  for a
#'              threshold-GAM(M) the development of the generalized cross-validation
#'              value at different thresholds level of the modifying pressure
#'              variable. The GCV value of the final chosen threshold should be
#'              distinctly lower than for all other potential thresholds, i.e.,
#'              the line should show a pointy negative peak at this threshold.
#'              If this is not the case, e.g. the trough is very wide with similar
#'              GCV values for nearby thresholds, the threshold-GAM(M) is not
#'              optimal and should not be favoured over a GAM despite the better
#'              LOOCV (leave-one-out cross-validation value).}
#'   \item{\code{all_plots}}{A list-column of ggplot2 objects that show all
#'              five (six if threshold-GAM) plots together. For this plot,
#'              drawing canvas from the \code{cowplot} package were added
#'              on top of ggplot2.}
#' }
#'
#' @seealso \code{\link[stats]{cooks.distance}}, \code{\link[stats]{acf}},
#' \code{\link[stats]{pacf}}, \code{\link[stats]{qqnorm}}, and
#' \code{\link[purrr]{flatten}} for removing a level hierarchy from a list
#' @family IND~pressure modelling functions
#'
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data:
#' # Apply function to a list of various model types
#' model_list <- c(all_results_ex$thresh_models[[5]],
#'   model_gam_ex$model[39], all_results_ex$model[76])
#' plots <- plot_diagnostics(model_list)
#' plots$cooks_dist[[1]]
#' plots$acf_plot[[2]]
#' plots$pacf_plot[[3]]
#' plots$resid_plot[[1]]
#' plots$qq_plot[[1]]
#' plots$gcvv_plot[[1]] # for threshold models
#' plots$all_plots[[1]] # shows all 5-6 plots
#'
#' # Make sure that thresh_models have not a nested list structure:
#' model_list <- all_results_ex$thresh_models[5:6] %>% purrr::flatten(.)
#' plots <- plot_diagnostics(model_list)
plot_diagnostics <- function(model_list) {

  # Data input validation --------------------

  # Check input and return warning if not a model
  if ("data.frame" %in% class(model_list)) {
    stop("You must provide a model or a list of models")
  }
  # If only 1 model used as input, which is not a
  # list -> conversion
  if (sum(c("gam", "gamm") %in% class(model_list)) > 0) {
    model_list <- list(model_list)
  }

  # In case model_list is a list of many models, but
  # some are empty (NULL) or some are nested
  # thresh_models --> flatten and discard
  model_list <- model_list %>% purrr::discard(.,
    is.na(.))

  # ---------------------------------------------

  # Get model type
  check <- purrr::map_chr(model_list, ~class(.x)[1])
	 # Create many empty models
  model_pred <- gcvv <- t_val <- best_t_val <- vector(mode = "list",
    length = length(model_list))
  cooks_dist <- model_resid <- model_resid_na <- vector(mode = "list",
    length = length(model_list))

  # Get model type
  for (i in seq_along(model_list)) {
    if (grepl("thresh", check[i])) {
      pass_model <- model_list[[i]]
      if (class(pass_model)[1] == "thresh_gam") {
        cooks_dist[[i]] <- stats::cooks.distance(pass_model)
        gcvv[[i]] <- pass_model$gcvv
        t_val[[i]] <- pass_model$t_val
        best_t_val[[i]] <- pass_model$mr
        model_resid[[i]] <- mgcv::residuals.gam(pass_model,
          type = "deviance")
        # for testing autocorrelation incl. NAs
        model_resid_na[[i]] <- rep(NA, length(pass_model$train_na))
        model_resid_na[[i]][!pass_model$train_na] <- model_resid[[i]]
      }
      # Not yet implemented
      if(class(pass_model)[1] == 'thresh_gamm') {
      	 pass_model <- pass_model$gam
        cooks_dist[[i]] <- stats::cooks.distance(pass_model)
        gcvv[[i]] <- pass_model$gcvv
        t_val[[i]] <- pass_model$t_val
        best_t_val[[i]] <- pass_model$mr
        model_resid[[i]] <- stats::residuals(model_list[[i]]$lme,
        		type = 'normalized')
        # for testing autocorrelation incl. NAs
        model_resid_na[[i]] <- rep(NA, length(model_list[[i]]$train_na))
          # (or should it be length(pass_model$train_na))???)
        model_resid_na[[i]][ !model_list[[i]]$train_na ] <- model_resid[[i]] }
    } else {
      if (check[i] == "gam") {
        pass_model <- model_list[[i]]
        cooks_dist[[i]] <- stats::cooks.distance(pass_model)
        model_resid[[i]] <- mgcv::residuals.gam(pass_model,
          type = "deviance")
        # for testing autocorrelation incl. NAs
        model_resid_na[[i]] <- rep(NA, length(model_list[[i]]$train_na))
        model_resid_na[[i]][!model_list[[i]]$train_na] <- model_resid[[i]]
      }
      if (check[i] == "gamm") {
        pass_model <- model_list[[i]]$gam
        cooks_dist[[i]] <- cooks_dist_gamm(pass_model)
        model_resid[[i]] <- stats::residuals(model_list[[i]]$lme,
          type = "normalized")
        # for testing autocorrelation incl. NAs
        model_resid_na[[i]] <- rep(NA, length(model_list[[i]]$gam$train_na))
        model_resid_na[[i]][!model_list[[i]]$gam$train_na] <- model_resid[[i]]
      }
    }
  	# Independent of the model class
  	model_pred[[i]] <- mgcv::predict.gam(pass_model,
  		 type = "response")
  }

  # Get quantiles for the q-q-plot
  quan_normal <- purrr::map(model_resid, ~quantile(x = rnorm(length(.))))
  # (throws an error if 0% and 100% quantile are
  # equal (in case of a missing model) - it's not
  # elegant but working..)
  theo_quan <- purrr::map2(.x = quan_normal, .y = model_resid,
    ~seq(from = .x[1], to = .x[5], by = (.x[5] -
      .x[1])/(length(.y) - 1)))

  # Get acf/ pacf values
  tac <- test_tac(model_resid_na)
  # Get the lags from the acf/pacf functions (also
  # used in external function test_tac())
  acf_lag <- purrr::map(model_resid_na, ~as.vector(stats::acf(.,
    na.action = stats::na.pass, plot = FALSE)$lag))
  pacf_lag <- purrr::map(model_resid_na, ~as.vector(stats::pacf(.,
    na.action = stats::na.pass, plot = FALSE)$lag))

	 # Helper function to create title including the corrstruct
	 # and threshold variable depending on the model
		create_title <- function(x) {
				if(grepl("thresh", class(x)[1])) {   # add interacting variable for thresh_models
					 if(grepl("gamm", class(x)[1])) {    # i.e. the thresh_gamm
						  t <- paste0(all.vars(x$gam$formula)[1],   # ind
										" ~ ", all.vars(x$gam$formula)[2], # press
										" | ", all.vars(x$gam$formula)[3], # threshold variable
										" (", toupper(class(x)[1]),    # model class
										")" )
					 } else {                            # thresh_gam
						  t <- paste0(all.vars(x$formula)[1],   # ind
										" ~ ", all.vars(x$formula)[2], # press
										" | ", all.vars(x$formula)[3], # threshold variable
										" (", toupper(class(x)[1]),    # model class
										")" )
		 			}
				} else {
					 if (grepl("gamm", class(x)[1])) {
					   temp <- strsplit(
					   	 utils::capture.output(x$lme$modelStruct$corStruct)[1],
						    "cor")[[1]][2]
					   corrstruct <- strsplit(temp, " ")[[1]][1]
					   if(grepl("ARMA", corrstruct)) {
						    corrstruct <- paste0(corrstruct,
							     attr(x$lme$modelStruct$corStruct, "p"),
								    ",", attr(x$lme$modelStruct$corStruct, "q"))
					   }
					   t <- paste0(all.vars(x$gam$formula)[1],   # ind
										" ~ ", all.vars(x$gam$formula)[2], # press
										" (", toupper(class(x)[1]),        # model class
										" [", toupper(corrstruct), "])")
				  } else {
					   t <- paste0(all.vars(x$formula)[1],   # ind
										" ~ ", all.vars(x$formula)[2], # press
										" (", toupper(class(x)[1]),    # model class
										")" )
				  }
				}
				return(t)
	 }

	 title <- purrr::map(model_list, ~create_title(.x))

  # Apply helper plot functions to each model
  cook_plots <- purrr::map(cooks_dist, plot_cook)
  acf_plots <- purrr::map2(acf_lag, tac$acf,
  		~plot_acf(x_var = .x, y_var = .y))
  pacf_plots <- purrr::map2(pacf_lag, tac$pacf,
  	 ~plot_pacf(x_var = .x, y_var = .y))
  resid_plots <- purrr::map2(model_pred, model_resid,
    ~plot_resid(model_resid = .y[!is.na(.y)], model_fitted = .x))
  qq_plots <- purrr::map2(theo_quan, model_resid,
    ~plot_qq(model_resid = .y[!is.na(.y)], theo_quan = .x[!is.na(.y)]))
  gcvv_plots <- purrr::pmap(list(t_val, gcvv, model_list,
    best_t_val), plot_gcvv)
  # Plot everything on 1 page (the absolutely longest
  # step)
  all_plots <- suppressWarnings(purrr::pmap(list(p1 = cook_plots,
    p2 = acf_plots, p3 = pacf_plots, p4 = resid_plots,
    p5 = qq_plots, p6 = gcvv_plots, title = title),
    plot_all_diag))

  # Get ind and press for output_tbl
  get_names <- function(x) {
    if (grepl("gamm", class(x)[1])) {
      ind <- all.vars(x$gam$formula)[1]
      press <- all.vars(x$gam$formula)[2]
    } else {
      ind <- all.vars(x$formula)[1]
      press <- all.vars(x$formula)[2]
    }
    return(data.frame(ind = ind, press = press))
  }
  dat <- suppressWarnings(purrr::map_df(model_list,
    get_names))

  # Combine all together
  out <- tibble::tibble(ind = dat$ind, press = dat$press,
    cooks_dist = cook_plots, acf_plot = acf_plots,
    pacf_plot = pacf_plots, resid_plot = resid_plots,
    qq_plot = qq_plots, gcvv_plot = gcvv_plots,
    all_plots = all_plots)

  # Insert NA in $gcvv_plot if model not a
  # thresh_gam(m)
  out$gcvv_plot[check %in% c("gam", "gamm")] <- NA  # NULL does not work


  ### END OF FUNCTION
  return(out)
}

