#' Scoring of indicator performance
#'
#' Scoring of each indicator based on an internal criterion-scoring scheme
#' applied on the output of the trend and pressure model functions.
#'
#' @param trend_tbl Output tibble from the \code{\link{model_trend}} function.
#' @param mod_tbl Output tibble from the IND~pressure modelling functions.
#' @param press_type Dataframe with pressure names in first column and
#'  corresponding pressure types in second column. Needed for spiechart!
#' @param crit_scores Internal tibble of (sub)criteria and respective scores
#'  named \code{crit_scores_tmpl}; can be modified by saving this dataframe as
#'  new object and removing single (sub)criteria or assigning weights (default
#'  is 1). The variable 'condition' represents a list of single elements or
#'  vectors with various elements to base the scoring on. This can be modified
#'  but needs to follow the same syntax.
#' @param sign_level Significance level on which scoring is built; default is
#'  0.05.
#'
#' @details
#' Among the 16 common indicator selection criteria summarised in Otto \emph{et al.} (2018)
#' five criteria relate to the indicator's performances and require time series for
#' their evaluation, i.e.
#' \itemize{
#'   \item Crit. 8: Development reflects ecosystem change caused by variation in manageable pressure(s)
#'   \item Crit. 9: Sensitive or responsive to pressures
#'   \item Crit. 10: Robust, i.e. responses in a predictive fashion, and statistically sound
#'   \item Crit. 11: Links to management measures (responsiveness and specificity)
#'   \item Crit. 12: Relates where appropriate to other indicators but is not redundant (not scored)
#' }
#'
#'
#' In this function, the scoring scheme for these criteria as proposed by
#' Otto \emph{et al.} (2018) serves as basis for the quantification of the IND performance.
#' Sensitivity (criterion 9) and robustness (criterion 10) are specified into more detailed
#' sub-criteria to allow for quantification based on statistical models and rated individually
#' for every potential pressure that might affect the IND directly or indirectly. In the case
#' of non-significant relationships between a IND and a specific pressures, sub-crit. 9.1 and
#' all following pressure-specific sub-crit. in criteria 9 and 10 are scored zero for this
#' pressure.
#'
#' The template tibble \code{crit_scores_tmpl} contains all relevant informations and
#' serves as basis for the scoring in this function. See for more details
#' \code{\link{crit_scores_tmpl}} or \code{View(crit_scores_tmpl)}. The scoring scheme can
#' easily be adapted to any kind of state indicator and management scheme by modifying
#' the scores, the weighting of scores or by removing or adding (sub)criteria in the
#' \code{crit_scores_tmpl} template. The \code{condition} variable can also be modified
#' but needs to follow the same syntax.
#'
#' @return
#' The function returns a nested tibble with the following elements depending on the
#' criteria rated
#' \describe{
#'   \item{\code{ind}}{A vector of the indicator names.}
#'   \item{\code{C8} and/or \code{C11}}{A vector of IND-specific scores for criterion 8 (trend
#'          indication) and/or C11 (management application).}
#'  \item{\code{press_spec_sc}}{A list-column of IND-specific dataframes
#'  containing pressure-specific scores for the subcriteria 9.1-9.2, 10.1-10.4.}
#' }
#' The tibble can easily be unnested by using the \code{\link[tidyr]{unnest}} function.
#' That is, each element of the dataframe in the list-column \code{press_spec_sc} becomes
#' its own row in the tibble.
#'
#' @seealso \code{\link[tidyr]{unnest}} to make each element of the dataframe in the
#' list-column \code{press_spec_sc} its own row and \code{\link[tidyr]{nest}} for the
#' inverse operation
#' @family score-based IND performance functions
#' @family IND~pressure modelling functions
#'
#' @references
#' Otto, S.A., Kadin, M., Casini, M., Torres, M.A., Blenckner, T. (2018)
#' A quantitative framework for selecting and validating food web indicators.
#' \emph{Ecological Indicators}, 84: 619-631,
#' doi: https://doi.org/10.1016/j.ecolind.2017.05.045
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in this package
#' scores_tbl <- scoring(trend_tbl = model_trend_ex, mod_tbl = all_results_ex,
#'   press_type = press_type_ex)
#' \dontrun{
#'  # To see the criterion template and change it potentially
#'  View(crit_scores_tmpl)
#'  # E.g. exclude the trend criterion
#'  crit_scores_tmpl_new <- crit_scores_tmpl[crit_scores_tmpl$crit_id > 1, ]
#'  # Now the trend tibble is not needed anymore
#'  scores_tbl <- scoring(mod_tbl = all_results_ex, press_type = press_type_ex,
#'   crit_scores = crit_scores_tmpl_new)
#' }
scoring <- function(trend_tbl = NULL, mod_tbl, press_type = NULL,
  crit_scores = INDperform::crit_scores_tmpl, sign_level = 0.05) {


  # Data input validation --------------------------

	 # Check input tibbles
	 mod_tbl <- check_input_tbl(
				mod_tbl, tbl_name = "mod_tbl", parent_func = "model_gam() or model_gamm()/select_model()",
				var_to_check = c("ind", "press"), dt_to_check = c("character", "character")
		)

	 if (!is.null(trend_tbl)) {
	 	 trend_tbl <- check_input_tbl(
	 	 trend_tbl, tbl_name = "trend_tbl", parent_func = "model_trend()",
	 	   var_to_check = c("ind"), dt_to_check = c("character")
	   )
	 }

	 # Check if all ind are present in both input tibbles (if trend_tbl needed)
  if ("C8" %in% crit_scores$crit == TRUE & !is.null(trend_tbl)) {
		  if (!all(unique(mod_tbl$ind) %in% trend_tbl$ind) &
		    all(trend_tbl$ind %in% unique(mod_tbl$ind))) {
		    stop("Some indicators are only present in one input tibble.")
		  }
  }

	 # Check if all variables required according to the crit_scores table are
  # in all input tibbles
  crit_var <- unique(crit_scores$condition_var)[!is.na(unique(crit_scores$condition_var))]
  crit_var <- crit_var[crit_var != "expect"] # this var will be generated in this function
  provided_var <- c(names(trend_tbl), names(mod_tbl))
  if (any(!crit_var %in% provided_var)) {
  missing_var <- crit_var[!crit_var %in% provided_var]
  stop(paste0("The following variables required for the scoring (see your crit_scores table) are not provided in any of the input tibbles: ",
    	paste0(missing_var, collapse = ", ")))
  }

  # Criterion 8 (Trend)
  if ("C8" %in% crit_scores$crit == TRUE)  {
  	if (is.null(trend_tbl)) {
    stop("You must provide data for the 'trend_tbl' argument (output of the 'model_trend' function) if you include the trend criterion (C8)!")
   } else {
   	c8_var <- unique(crit_scores$condition_var[crit_scores$crit == "C8"])
  	 if (any(!c8_var %in% names(trend_tbl))) {
  	 	missing_c8_var <- c8_var[!c8_var %in% names(trend_tbl)]
  	 	 stop(paste0("The following variables required for scoring crit. 8 (see your crit_scores table) are not provided in 'trend_tbl': ",
    	paste0(missing_c8_var, collapse = ", ")))
  	 }
   }
  }


	 # Subcriteria in 9 (Sensitivity) and 10 (Robustness)
  d <- as.data.frame(unique(crit_scores[crit_scores$crit %in%
    c("C9", "C10") & crit_scores$subcrit != "C10_1",
    c("condition_var", "func_name")]))
  names(d) <- c("missing_variable", "function")
  d_bool <- d$missing_variable %in% names(mod_tbl)
  if (any(d_bool == FALSE)) {
    message("Given the criteria you want to score, your model tibble (mod_tbl) lacks the following variable(s), which you get when applying these functions")
    print(d[d_bool == FALSE, ])
    stop()
  }

  # Criterion 11 (Management)
  if ("C11" %in% crit_scores$crit == TRUE & is.null(press_type)) {
    message("You must provide for crit.11 a data frame or tibble with pressure types (named 'press_type') assigned to each pressure (named 'press') as follows:")
    print(data.frame(press = unique(mod_tbl$press),
    	press_type = rep("add here", length(unique(mod_tbl$press)))))
    stop()
  }

  # If press_type is not provided give note that error will occurr in spiechart function,
  # else check if all pressures in mod_tbl are also in press_type
  if (is.null(press_type)) {
  	 message("You did not provide the pressure type information for each pressure (as press_type tibble). This will lead to an error when running the spiechart function!")
  } else {
  	press_v <- unique(mod_tbl$press)
  	if (any(!press_v %in% press_type$press)) {
    missing_press <- press_v[!press_v %in% press_type$press]
    stop(paste0("The following pressure variables in 'mod_tbl' are not listed in the 'press_type' tibble: ",
    	paste0(missing_press, collapse = ", ")))
  	}
  }



  # Data preparation --------------------------

  # Prepare crit_score and press_type for scoring
  crit_scores$weighted_score <- crit_scores$score *
    crit_scores$weight
  if (tibble::is.tibble(press_type) == FALSE)
    press_type <- tibble::as_tibble(press_type)
  if (is.factor(press_type$press))
    press_type$press <- as.character(press_type$press)
  if (is.factor(press_type$press_type))
    press_type$press_type <- as.character(press_type$press_type)

  # Add logical variable 'expect' with TRUE as
  # default
  mod_tbl$expect <- TRUE

  # Add pressure type to mod_tbl for output and C11
  # (as last row)
  if (!is.null(press_type)) {
    mod_tbl <- mod_tbl %>% dplyr::left_join(press_type[,
      c("press", "press_type")], by = "press")
  }

  # Helper functions for scoring ------------------

  # Main function that checks a single value against
  # the condition set in crit_scores for that
  # particular (sub-)criterion --> it creates a
  # logical vector with TRUE/FALSE depending if the
  # condition for x is met
  score_f <- function(x, crit_df_sub, scr = NULL) {
    # x: a single value from a variable that needs to
    # be scored (the entire variable is scored by
    # applying this function through apply or purrr
    # crit_df_sub: subset depending on the
    # (sub-)criterion to be scored scr: the
    # subcriterion to score (only needed for the error
    # message)

    cond <- crit_df_sub$condition
    n <- length(cond)
    cond_bool <- vector(length = n)
    for (i in 1:n) cond_bool[i] <- eval(parse(text = cond[[i]]))  # parse() turns the the condition into an R expression which is then evaluated with eval()
    if (sum(cond_bool) > 1) {
      stop(cat(paste("The conditions set in the crit_scores table for sub-criterion",
        scr, "are not unique, i.e. conditions are met multiple times!",
        "Please correct your crit_score table before you continue.",
        sep = "\n")))
    } else {
      fin_score_scr <- crit_df_sub$weighted_score[cond_bool]
      return(fin_score_scr)
    }
  }

  # Create pressure-specific score table for criteria  ---------------
  # C9 and C10 --------

  # Get subcriteria selected by user (or default)
  subcrit_v <- unique(crit_scores$subcrit[crit_scores$crit %in%
    c("C9", "C10")])

  if (!is.null(subcrit_v)) {

      # Create empty scoring table with metadata and all
      # selected subcriteria
      if (!is.null(press_type)) {
        score_c910 <- mod_tbl[, c(1:3, ncol(mod_tbl))]  # adding press_type column
      } else {
        score_c910 <- mod_tbl[, c(1:3)]
      }
      score_c910[, subcrit_v] <- NA

      # Split the data based on whether the pressure
      # effect is significant or not (based on the alpha
      # level set a priori); in the case of the latter,
      # all sub-criteria scores are set to zero:
      mod_tbl_split <- split(mod_tbl, mod_tbl$p_val <=
        sign_level)
      score_c910_split <- split(score_c910, mod_tbl$p_val <=
        sign_level)

      # Non-significant models (`FALSE` list) scored zero
      score_c910_split$`FALSE`[, subcrit_v] <- 0


      # Score significant models (`TRUE` list) per
      # subcriterion --------

      # To get the correct variable for the respective
      # subcriterion
      sc_var <- unique(crit_scores[crit_scores$crit %in%
        c("C9", "C10"), c("subcrit", "condition_var")])
      names(sc_var) <- c("scr", "var")

      # Wrapper function for Crit 9 and 10 that applies
      # score_f() vectorwise for each element of the
      # variable of interest using purrr
      apply_score <- function(var, crit_df, scr) {
        crit_df_sub <- crit_df[crit_df$subcrit ==
          scr, ]
        temp <- purrr::map_dbl(.x = var, .f = score_f,
          crit_df_sub = crit_df_sub, scr = scr)
        return(temp)
      }
      # Loop where the apply_score function is applied
      # for each subcriterion that needs to be scored
      for (i in 1:length(subcrit_v)) {
        var_ch <- sc_var$var[sc_var$scr ==
          subcrit_v[i]]
        var_num <- mod_tbl_split$`TRUE` %>%
          dplyr::select_(.dots = var_ch) %>%
          .[[1]]  # .[[1]] needed to get vector, not 1D tibble
        score_c910_split$`TRUE`[, subcrit_v[i]] <- apply_score(var = var_num,
          crit_df = crit_scores, scr = subcrit_v[i])
      }

      # Combine both lists and sort by id
      score_c910 <- dplyr::bind_rows(score_c910_split$`FALSE`,
        score_c910_split$`TRUE`)
      score_c910 <- score_c910 %>% dplyr::arrange_(.dots = "id")

    }  # end of if statement for C9/10 selection


  # Create indicator-specific score table for
  # criteria C8 and C11 ----------------

  # Get criteria selected by user (or default)
  crit_v <- unique(crit_scores$crit)
  # check whether C8 and C11 were selected and save
  # these additionally
  toMatch <- c("C8", "C11")
  crit_v_811 <- unique(grep(paste(toMatch, collapse = "|"),
    crit_v, value = TRUE))

  # Create empty scoring table and do the scoring
  # ONLY if C8 and/or C11 are selected
  if (!is.null(crit_v_811)) {
	  	# Create scoring table including indicator name and
	  	# selected criteria
	  	score_c811 <- tibble::tibble(ind = unique(mod_tbl$ind))
	  	score_c811[, crit_v_811] <- NA

	  	### Score C8 based on p-value set in crit_score
	  	### significance of trend GAM: p_val
	  	if ("C8" %in% crit_v_811) {
	  			cr <- "C8"
	  			crit_scores_8 <- crit_scores[crit_scores$crit ==
	  					cr, ]
	  			score_c811$C8 <- purrr::map_dbl(.x = trend_tbl$p_val,
	  			.f = score_f, crit_df_sub = crit_scores_8)
	  	}

	  	### Score C11 combinations of significant pressures
	  	if ("C11" %in% crit_v_811) {
	  		if (is.null(score_c910_split$`TRUE`)) {
	  			 # i.e. if none of the tested indicator respond to
	  			 # any pressure
	  			 score_c811$C11 <- 0
	  		} else {
	  			 cr <- "C11"
	  			 crit_scores_11 <- crit_scores[crit_scores$crit ==
	  					 cr, ]

		  			# Extract signifiant pressures per ind from
		  			# score_c910_split$`TRUE` and save them as list
		  			press_per_ind <- split(score_c910_split$`TRUE`$press_type,
		  				score_c910_split$`TRUE`$ind)

		  			# Calculate the number of pressures and convert to
		  			# vector
		  			nr_press_per_ind <- purrr::map(press_per_ind,
		  					~length(unique(.)))
		  			nr_press_per_ind <- unlist(nr_press_per_ind)

		  			# Apply the helper function score_f
		  			score_per_ind <- purrr::map_dbl(.x = nr_press_per_ind,
		  					.f = score_f, crit_df_sub = crit_scores_11)

		  			# Merge C11 scores into score_c811 tibble
		  			temp <- tibble::tibble(ind = names(score_per_ind),
		  					C11 = unlist(score_per_ind))
		  			score_c811$C11 <- temp$C11[match(score_c811$ind,
		  					temp$ind)]

		  			# Here add zeros to indicators that did not respond
		  			# to any pressure and, hence, are not in
		  			# score_c910_split$`TRUE`
		  			score_c811$C11[-match(temp$ind,
		  					score_c811$ind)] <- 0

	  			}  # end of else condition (if sign press are not null)
	  	}  # end of if statement for C11
	 }  # end of if statement for selection of C8/C11


  # Get data tables ready for export --------------------

  # (if only the score_c811 table was generated)
  if (is.null(subcrit_v) == TRUE) {
    out <- score_c811
  } else {
    # Convert table into a nested tibble for merging
    # with other criteria
    score_c910 <- score_c910 %>% dplyr::group_by_(.dots = "ind") %>%
      tidyr::nest_(key_col = "press_spec_sc")

    # (if only the score_c910 table was generated)
    if (is.null(crit_v_811) == TRUE) {
      out <- score_c910
    } else { # (if both tables were generated)
      # Merge with score_c811
      out <- dplyr::left_join(score_c811, score_c910,
        by = "ind")
    }
  }

  #### End of Function ####
  return(out)

}
