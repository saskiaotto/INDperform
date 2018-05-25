#' Test for interactions between 2 pressure variables
#'
#' \code{test_interaction} tests for each significant GAM(M) whether any other
#' pressure modifies the IND response to the original pressure using a threshold
#' formulation of the GAM.
#'
#' @param init_tbl The full output tibble of the \code{\link{ind_init}} function.
#' @param mod_tbl A model output tibble from \code{\link{model_gam}},
#'  \code{\link{select_model}} or \code{\link{merge_models}} representing the
#'  best model for each IND~pressure pair.
#' @param interactions A tibble of all potential pressure combinations to test
#'  for interactions for specific INDs.
#' @param sign_level Significance level for selecting models on which to test for
#'  interactions. Only models with a p value <= sign_level will be selected;
#'  the default is 0.05.
#' @param k Choice of knots (for the smoothing function \code{\link{s}}); the
#'  default is here 4 to avoid over-parameterization.
#' @param a The lower quantile value of the selected threshold variable, which the
#'  estimated threshold is not allowed to exceed; the default is 0.2.
#' @param b The upper quantile value of the selected threshold variable, which the
#'  estimated threshold is not allowed to exceed; the default is 0.8.
#' @param excl_outlier logical; if TRUE, the outliers excluded in the original
#'  models will be also excluded in the interaction models.
#'
#' @details
#'
#' \strong{Threshold-GAMs}
#'
#' To identify potential interactions between pressures (relevant for sub-crit. 10.4),
#' threshold formulations are applied to every significant IND-pressure GAM.
#' threshold-GAMs or TGAMs represent a special type of varying-coefficient models
#' and were first introduced by Ciannelli \emph{et al.} (2004). In varying-coefficient
#' models coefficients are allowed to vary as a smooth functions of other variables
#' (Hastie and Tibshirani, 1993), which allows the detect interactions between two
#' external pressures. Threshold-GAMs are particularly useful if the response to the
#' interacting pressure variables is not continuous but rather step-wise. They have
#' been applied to data from real ecosystems to model population dynamics
#' (e.g. Otto \emph{et al.}, 2014) up to food web dynamics in response to
#' climate, nutrient, and fishing interactions (e.g. Llope \emph{et al.}, 2011).
#' The following threshold formulation is applied to every sign. IND-Pressure GAM:
#'
#' \deqn{ IND_t = \alpha_1 + s_1(press1_t) + \epsilon_t   if press2 <= r}
#' \deqn{ IND_t = \alpha_2 + s_2(press1_t) + \epsilon_t   if press2 > r}
#'
#' where the thin-plate regression spline function \emph{s} can differ depending on
#' whether pressure 2 is below or above a threshold \emph{r} with possible changes
#' in the intercept (from \emph{a_1} to \emph{a_2}). The index \emph{t} represents
#' each observation in the training data. The threshold formulation can be
#' implemented in the GAM by including two smoothing functions for the same pressure
#' and using the \code{by} argument in the smoothing function \code{\link[mgcv]{s}}
#' to select specific observations. The threshold itself is estimated from the data and
#' chosen by minimizing the GCV score (termed 'gcvv' in the threshold-GAM object)
#' over an interval defined by the lower and upper quantiles (see the
#' \code{a} and \code{b} arguments respectively) of Pressure 2.
#'
#' \strong{Selection of threshold-GAMs}
#'
#' To compare the performance of a GAM without interactions with the threshold-GAM we
#' implemented a leave-one-out cross-validation (LOOCV) approach as suggested by
#' Ciannelli \emph{et al.} (2004). LOOCV involves splitting the full set of observations
#' n (i.e. the training data defined in \code{\link{ind_init}}) into two parts: a new
#' training set containing n-1 observations and a test set containing 1 observations.
#' The threshold-GAM and corresponding GAM are then fit on the new training data
#' and a prediction is made for the excluded test observation using the corresponding
#' pressure 1 and pressure 2 values for that time step. A square prediction error
#' is then calculated for each predicted test observation. Repeating this approach
#' n times produces n squared errors, MSE_1, . . . , MSE_n. The LOOCV estimate for the
#' test MSE, also termed (genuine) cross-validatory squared prediction error, is the
#' root of the average of these n test error estimates. This approach properly accounts
#' for the estimation of the threshold value as well as the effective degrees of
#' freedom of all model terms.
#'
#'\strong{Implementation of threshold modelling}
#'
#' For each IND~pressure pair specific pressures to test for interactions can be selected
#' by creating a tibble containing the IND (termed 'ind'), the pressure 1 (termed 'press')
#' and the pressure 2 (termed 't_var'). The easiest is to use the helper function
#' \code{\link{select_interaction}}: it creates all combinations of IND~press pairs and
#' the threshold variables based on the input model tibble. If specific combinations should
#' not be modelled simply delete them from this dataframe.
#'
#' \code{test_interaction} takes this dataframe and the ind_init and model tibble as input
#' and applies the following procedure:
#'
#' \itemize{
#'   \item Filters significant GAMs and the corresponding IND ~ pressure | threshold pressure
#'         combination.
#'   \item Extracts all data needed for the model, excluding outliers if set to TRUE.
#'   \item Computes the LOOCV for each IND ~ pressure | threshold pressure model
#'         (threshold-GAM and GAM).
#'   \item If the LOOCV estimate of the threshold-GAM is better than its corresponding GAM
#'         the threshold-GAM and its model output are saved in the returned model tibble.
#'         Note, however, that it is crucial to inspect the model diagnostic plots (i.e.
#'         the \code{$thresh_plot} from the \code{\link{plot_diagnostics}} function! The
#'         performance of the final model (in terms of its Generalized Cross-Validation
#'         value) might be not much lower than for models with other thresholds indicating
#'         a lack of robustness. In this case, the interaction might need to be ignored
#'         but that needs to be decided on a case-by-case basis.
#' }
#'
#' There is no threshold-GAMM implemented in this package yet. Until then, threshold-GAMs are
#' also applied to GAMMs when GAM residuals show temporal autocorrelation (TAC). However, the residuals
#' of threshold GAMs often show less TAC due to the splitting of observations into a low and high
#' regime of the threshold variable. In the case of significant TAC (indicated by the output variable
#' \code{tac_in_thresh}) the user can decide whether that interaction should be neglected and
#' modify the \code{interaction}) output variable accordingly.
#'
#' @return
#' The function returns the input model tibble mod_tbl with the following 4 columns added:
#' \describe{
#'   \item{\code{interaction}}{logical; if TRUE, at least one thresh_gam
#'              performs better than its corresponding gam based on the leave-one-out
#'              cross-validation.}
#'   \item{\code{thresh_var}}{A list-column with the threshold variables of the
#'              better performing thresh_models.}
#'   \item{\code{thresh_models}}{A list-column with nested lists containing the
#'              better performing thresh_models.}
#'   \item{\code{tac_in_thresh}}{logical vector; indicates for every listed
#'              thresh_model whether temporal autocorrelation (TAC) was
#'              detected in the residuals. TRUE if model residuals show TAC.}
#' }
#'
#' @seealso \code{\link{plot_diagnostics}} for assessing the model diagnostics
#' @family IND~pressure modelling functions
#'
#' @references
#' Ciannelli, L., Chan, K.-S., Bailey, K.M., Stenseth, N.C. (2004) Nonadditive
#' effects of the environment on the survival of a large marine fish population.
#' \emph{Ecology} 85, 3418-3427.
#'
#' Hastie, T., Tibshirani, R. (1993) Varying-Coefficient Models. \emph{Journal
#' of the Royal Statistical Society. Series B (Methodological)} 55, 757-796.
#'
#' Llope, M., Daskalov, G.M., Rouyer, T.A., Mihneva, V., Chan, K.-S., Grishin, A.N.,
#' Stenseth, N.C. (2011) Overfishing of top predators eroded the resilience of the
#' Black Sea system regardless of the climate and anthropogenic conditions.
#' \emph{Global Change Biology} 17, 1251-1265.
#'
#' Otto, S.A., Kornilovs, G., Llope, M., Möllmann, C. (2014) Interactions among
#' density, climate, and food web effects determine long-term life cycle
#' dynamics of a key copepod. \emph{Marine Ecology Progress Series} 498, 73-84.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using some models of the Baltic Sea demo data in this package
#' init_tbl <- ind_init_ex
#' mod_tbl <- merge_models_ex[c(5:7),]
#' interactions <- select_interaction(mod_tbl)
#' test <- test_interaction(init_tbl, mod_tbl, interactions)
#'
#' # if only one combination should be tested
#' interactions <- select_interaction(mod_tbl)[1:2, ]
#' test <- test_interaction(init_tbl, mod_tbl, interactions)
#'
#' # Determine manually what to test for (e.g. only TZA ~ Fsprat | Pwin)
#' interactions <- tibble::tibble(ind = "TZA",
#'                                press = "Fsprat",
#'                                t_var = "Pwin" )
#' test <- test_interaction(init_tbl, mod_tbl, interactions)
#' }
test_interaction <- function(init_tbl, mod_tbl, interactions,
  sign_level = 0.05, k = 4, a = 0.2, b = 0.8, excl_outlier = FALSE) {

  # Data input validation ----------------
	 if (missing(init_tbl)) {
	 	stop("Argument 'init_tbl' is missing.")
	 }
		 if (missing(mod_tbl)) {
	 	stop("Argument 'mod_tbl' is missing.")
		 }
		 if (missing(interactions)) {
	 	stop("Argument 'interactions' is missing.")
	 }

	 # Check input tibbles
	 init_tbl <- check_input_tbl(
	 	 init_tbl, tbl_name = "init_tbl", parent_func = "ind_init()",
	 	 var_to_check = c("id", "ind", "press", "ind_train", "press_train", "time_train",
	 	 	 "ind_test", "press_test", "time_test", "train_na"),
	 	 dt_to_check = c("integer", "character", "character", rep("list", 7))
	 )
	 mod_tbl <- check_input_tbl(
				mod_tbl, tbl_name = "mod_tbl", parent_func = "model_gam() or model_gamm()/select_model()",
				var_to_check = c("id", "ind", "press", "model_type", "p_val", "model"),
				dt_to_check = c("integer", "character", "character", "character", "numeric",
					 "list")
		)

  if ((!"excl_outlier" %in% names(mod_tbl)) &
    isTRUE(excl_outlier)) {
    stop("There is no column 'excl_outlier'. Please set excl_outlier to FALSE!")
  }
	 # As the column 'excl_outlier' is later needed, add here
	 #  list of NULLs
	 if ((!"excl_outlier" %in% names(mod_tbl)) &
    excl_outlier == FALSE)  {
    mod_tbl$excl_outlier <- vector("list", length = length(mod_tbl$id))
	 }

  if (!all(unique(interactions$t_var) %in% unique(init_tbl$press))) {
    stop("Init_tbl has to have all the observed data needed for t_var!")
  }

  if (!identical(init_tbl$id, mod_tbl$id)) {
    if (!all(mod_tbl$id %in% init_tbl$id)) {
      stop("Not all ids in mod_tbl are provided in init_tbl.")
    }
  }

	 # Test whether all ind~press combis in interactions are also in mod_tbl
	 ind_press_it <- paste(interactions$ind, interactions$press, sep = "~")
		ind_press_mod <- paste(mod_tbl$ind, mod_tbl$press, sep = "~")

	 if (any(!ind_press_it %in% ind_press_mod)) {
				missing_it <- which(!ind_press_it %in% ind_press_mod)
				stop(paste0("The following 'ind~press' combinations provided in the 'interactions' tibble are missing in 'mod_tbl': ",
						paste(missing_it, collapse = ", ")))
		}

  # Test if there are any ids with NAs in models (if
  # GAMMs manually selected and convergence errors
  # occurred)
  if (any(is.na(mod_tbl$model))) {
    stop(paste0("The following ids have missing models: ",
      paste0(mod_tbl$id[is.na(mod_tbl$model)],
        collapse = ", ")))
  }

  # Correct sign_level (between 0 and 1)?
  if (is.null(sign_level)) {
    stop("The sign_level value must be a single numeric value between 0 and 1.")
  } else {
    if (!is.numeric(sign_level) | (sign_level <
      0) | (sign_level > 1)) {
      stop("The sign_level value must be a numeric value between 0 and 1.")
    }
  }

  # Data preparation -----------------------

  data <- dplyr::left_join(mod_tbl[, c("id", "p_val",
    "model_type", "model", "excl_outlier")], init_tbl,
    by = "id")

  # Add training data of t_var to interaction tibble
  interactions <- interactions %>% dplyr::left_join(.,
    init_tbl[, c("press", "press_train")],
    by = c(t_var = "press"))
   names(interactions)[names(interactions) == "press_train"] <- "t_var_train"

  # Combine press values with press & t_var
  # combinations.
  suppressWarnings(final_tab <- dplyr::select_(data,
    .dots = c("ind", "press", "press_train", "time_train",
      "ind_train", "p_val", "model_type", "model",
      "excl_outlier")) %>% dplyr::left_join(interactions,
    ., by = c("ind", "press")))
  # Filter data for significance
  final_tab <- final_tab[is_value(final_tab$p_val) &
  		final_tab$p_val <= sign_level, ]

  # Stop if no models left
  if (nrow(final_tab) == 0) {
    stop("Not a single model has a p_val <= sign_level!")
  }
  # Create input data
  if (!excl_outlier) {
    y <- final_tab$ind_train
    x1 <- final_tab$press_train
    x2 <- final_tab$t_var_train
    time <- final_tab$time_train
  } else {
    # .. replace outlier
    y <- purrr::map2(final_tab$ind_train, final_tab$excl_outlier,
      ~replace(.x, .y, NA))
    x1 <- purrr::map2(final_tab$press_train, final_tab$excl_outlier,
      ~replace(.x, .y, NA))
    x2 <- purrr::map2(final_tab$t_var_train, final_tab$excl_outlier,
      ~replace(.x, .y, NA))
    time <- purrr::map2(final_tab$time_train, final_tab$excl_outlier,
      ~replace(.x, .y, NA))
  }

  # Get other model info for fitting input
  grab_gam <- function(x) {
    if (class(x)[1] == "gam")
      return(x)
    if (class(x)[1] == "gamm")
      return(x$gam)
  }
  model <- purrr::map(final_tab$model, ~grab_gam(.))
  name_x2 <- as.list(final_tab$t_var)
  k_list <- as.list(rep(k, length(model)))
  a_list <- as.list(rep(a, length(model)))
  b_list <- as.list(rep(b, length(model)))
  prog_now <- as.list(1:length(model))
  prog_max <- list(length(model))


  # Actual thresh-GAM fitting -----------------

  # Helper function that runs the external loocv
  # function and adds progress bar
  show_prog <- function(model, y, x1, x2, name_x2,
    k_list, a_list, b_list, time, prog_now, prog_max) {
    message(paste0(prog_now), "/", prog_max)
    res <- loocv_thresh_gam(model = model, ind_vec = y,
      press_vec = x1, t_var = x2, name_t_var = name_x2,
      k = k_list, a = a_list, b = b_list, time = time)
    return(res)
  }
  # Apply show_prog to every model (each interaction)
  suppressWarnings(temp_gam <- purrr::pmap(.l = list(model, y,
    x1, x2, name_x2, k_list, a_list, b_list, time,
    prog_now, prog_max), show_prog) %>% purrr::transpose())
  final_tab$interaction <- temp_gam$result %>% purrr::flatten_lgl()
  final_tab$thresh_error <- temp_gam$error %>%	purrr::flatten_chr()

  # Save every thresh_gam better than the
  # corresponding gam, NA vector
  final_tab$thresh_models <- vector(mode = "list",
    length = nrow(final_tab))
  final_tab$train.na <- vector(mode = "list", length = nrow(final_tab))
  final_tab$tac_in_thresh <- rep(NA, length = nrow(final_tab))
  final_tab$thresh_var <- rep(NA_character_, length = nrow(final_tab))

  # Within a loop apply the external helper function
  # thresh_gam to all models where thresh_gam was
  # better
  for (i in 1:nrow(final_tab)) {
    if (!is.na(final_tab$interaction[i]) & final_tab$interaction[i] == TRUE) {
      final_tab$thresh_models[[i]] <- thresh_gam(model = model[[i]],
        ind_vec = y[[i]], press_vec = x1[[i]],
        t_var = x2[[i]], name_t_var = name_x2[[i]],
        k = k, a = a, b = b)

      # Get the NA vector considering NAs now in ind,
      # press and the t_var
      temp <- final_tab$thresh_models[[i]]$original_data
      train_na <- data.frame(time = as.integer(rownames(temp)),
        na = as.logical(rowSums(is.na(temp))))

      # If time has missing values due to the random
      # extraction of test observations
      time_full <- data.frame(time = seq(min(train_na$time),
        max(train_na$time), 1))
      train_na_full <- dplyr::left_join(time_full,
        train_na, by = "time")
      train_na_full$na[is.na(train_na_full$na)] <- TRUE
      final_tab$train.na[[i]] <- train_na_full$na
      names(final_tab$train.na[[i]]) <- train_na_full$time

      # Calculate residuals and tac
      res <- mgcv::residuals.gam(final_tab$thresh_models[[i]],
        type = "deviance")
      res_full <- rep(NA, length(final_tab$train.na[[i]]))
      res_full[!final_tab$train.na[[i]]] <- res  #12,15,18,20,
      final_tab$tac_in_thresh[[i]] <- test_tac(list(res_full))$tac

      # Store NA vector in model for plot_diagnostics
      final_tab$thresh_models[[i]]$train_na <- final_tab$train.na[[i]]
      # Save name of threshold variable
      final_tab$thresh_var[[i]] <- final_tab$t_var[[i]]
    }
  }

  # Data aggregation for output tibble
  # -----------------------

  # Get ind~press where interaction is TRUE if any
  # thresh_gam is better max becomes 1 = TRUE -->
  # thresh_gam was better than gam
  temp <- suppressWarnings(final_tab %>% dplyr::group_by_(.dots = c("ind",
    "press")) %>% dplyr::summarise_(
    	.dots = stats::setNames(list(~as.logical(max(interaction, na.rm = TRUE))),
    "interaction")))

  # Get every thresh_gam better than the
  # corresponding gam
  out <- final_tab %>% dplyr::group_by_(.dots = c("ind", "press")) %>%
    dplyr::select_(.dots = c("ind",
    "press", "interaction", "thresh_var", "tac_in_thresh",
    "thresh_models")) %>%
  	dplyr::summarise_(thresh_var = lazyeval::interp(~list(var),
    var = as.name("thresh_var")), tac_in_thresh = lazyeval::interp(~list(var),
    var = as.name("tac_in_thresh")), thresh_models = lazyeval::interp(~list(var),
    var = as.name("thresh_models"))) %>% dplyr::left_join(temp,
    ., by = c("ind", "press"))

  # Remove all infos on thresh_gams that are NULL
  # (not as good as a gam)
  out$thresh_var <- purrr::map2(.x = out$interaction,
    .y = out$thresh_var, ~ifelse(.x, purrr::keep(.y,
      !is.na(.y)), NA))
  out$tac_in_thresh <- purrr::map2(.x = out$interaction,
    .y = out$tac_in_thresh, ~ifelse(.x, purrr::keep(.y,
      !is.na(.y)), NA))
  out$thresh_models <- purrr::map2(.x = out$interaction,
    .y = out$thresh_models, ~ifelse(.x, purrr::compact(.y),
      NA))

  # Join out to mod_tbl for final tibble
  out <- dplyr::left_join(mod_tbl, out, by = c("ind",
    "press"))
  out <- sort_output_tbl(out)
  out <- dplyr::arrange_(out, .dots = "id")


  # Warning if some models were not fitted
  if (any(is.na(final_tab$interaction))) {
  	 miss_mod <- final_tab[is.na(final_tab$interaction), c(1:3, 13)]
			 message(paste0("For the following indicators fitting procedure failed ",
				"(see also column 'thresh_error' in output tibble):"))
  	 print(miss_mod)
  }


  ### END OF FUNCTION ####
  return(out)
}

