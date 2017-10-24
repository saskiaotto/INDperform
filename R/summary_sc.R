#' Summary of indicator performance scores
#'
#' Summarizes the scoring output tibble so that IND-specific scores for each
#' criterion as well as the pressure-specific subcriteria scores (in crit.
#' 9 and 10) can be easily compared.
#'
#' @param scores_tbl The output tibble from the \code{\link{scoring}}
#'  function.
#' @param crit_scores The(un)modified criterion-scoring template
#'  \code{crit_scores_tmpl}; required to calculate the scores in
#'  percentage. Has to be the same than used in \code{scoring}. Default
#'  is the unmodified template \code{crit_scores_tmpl}.
#'
#' @return
#' The function returns a list of 2 dataframes
#' \describe{
#'   \item{\code{overview}}{IND-specific scores and percentages from
#'         max. score for all criteria (crit 9 and 10 averaged across
#'         all sign. pressures and the number of significant pressures).}
#'   \item{\code{subcriteria_per_press}}{IND- and pressure-specific scores for
#'          all (sub)criteria and the percentages from max.criterion score.}
#' }
#'
#' @family score-based IND performance functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in this package
#' scores_tbl <- scoring(trend_tbl = model_trend_ex, mod_tbl = all_results_ex,
#'   press_type = press_type_ex)
#' summary_sc(scores_tbl)
summary_sc <- function(scores_tbl,	crit_scores = INDperform::crit_scores_tmpl) {

	# Data preparation -------------------

  # Get weighted scores for calculating total scores
	 crit_scores$weighted_score <- crit_scores$score * crit_scores$weight

	 # Get total scores per criterion
	 total_sc <- crit_scores %>%
	 	dplyr::group_by_(.dots = c("crit", "subcrit")) %>%
	 	dplyr::summarise_(.dots = stats::setNames(list(~max(score)), "max_score")) %>%
	 	dplyr::group_by_(.dots = "crit") %>%
	 	dplyr::summarise_(.dots = stats::setNames(list(~sum(max_score)), "total_score"))

		# Separate data into general and pressure-specific scores (here only significant pressures)
	 # but check first if criteria are present in scoring tibble

	 if (sum(c("C8", "C11")  %in%  names(scores_tbl)) > 0) {
	 	 scores_c811 <- scores_tbl %>%
			 dplyr::select_(.dots = names(scores_tbl)[names(scores_tbl) %in% c("ind",
			  "C8", "C11")])

			 	# Add proportions only to the
			 	for(i in 2:ncol(scores_c811)) {
							crit_s <- names(scores_c811)[i]
							new_var <- paste0(crit_s,"_in%")
							tsc <- total_sc$total_score[total_sc$crit == crit_s]
							scores_c811[,new_var] <- round(scores_c811[,i] / tsc * 100, 0)
			 	}

	 }

	 if("press_spec_sc" %in%  names(scores_tbl) == TRUE) {
		 # Extract scores of sign.pressures
	 	scores_c910 <- scores_tbl %>%
			dplyr::select_(.dots = c("ind", "press_spec_sc")) %>%
			tidyr::unnest() %>%
			dplyr::filter(rowSums(dplyr::select_(.,
				.dots = c("-ind","-press", "-id", "-press_type"))) > 0)
		# Make data long for the aggregation in output 1
		scores_c910_l <- scores_c910 %>%
			tidyr::gather_(key_col = "subcrit", value_col = "score",
				gather_cols = names(scores_c910)[!names(scores_c910) %in% c("ind",
					"id", "press", "press_type")])
		# Add the criteria
		scores_c910_l$crit <- sub('\\_.*', '', scores_c910_l$subcrit)
		}

# Generate Output Table 1 ---------------------------------
# (Overview table where C9 and C10 are averaged across sign. press)

	 ### If only C8 and/or 11 were scored but NOT C9/10:
	 if (sum(c("C8", "C11")  %in%  names(scores_tbl)) > 0 &
	 		"press_spec_sc" %in%  names(scores_tbl) == FALSE) {

	 	# Returned output
	 	print_list <- vector("list", length = 1)
	 	names(print_list)[1] <- "Overview"
	 	print_list[[1]] <- scores_c811

		 } else {

				# Calculate sum across subcriteria in C9 and C10 and return to wide format
			 # (long format simply to avoid if statements for checking for crit presence)
			 scores_c910_sum <- scores_c910_l %>%
			 	dplyr::group_by_(.dots = c("ind", "press", "crit")) %>%
					dplyr::summarise_(.dots = stats::setNames(list(~sum(score)), "sum_score")) %>%
			 	tidyr::spread_("crit", "sum_score")

			 # Calculate number of sign. pressures and mean across sign.pressures for C9/C10
			 scores_c910_mean <- scores_c910_sum %>%
			 		dplyr::group_by_(.dots = "ind") %>%
			 		dplyr::summarise_(.dots = stats::setNames(list( ~dplyr::n_distinct(press),
			 			~round(sum(C9)/ nr_sign_press,1), ~round(sum(C10)/ nr_sign_press,1)),
			 			c("nr_sign_press", "C9","C10"))
			 		) # quite tricky to calculate several variables in the SE summarise version!

			 	# Add proportions of total scores
					for(i in 3:ncol(scores_c910_mean)) {
						crit_s <- names(scores_c910_mean)[i]
						new_var <- paste0(crit_s,"_in%")
						tsc <- total_sc$total_score[total_sc$crit == crit_s]
						scores_c910_mean[,new_var] <- round(scores_c910_mean[,i] / tsc * 100, 0)
					}


				### If only C9/10 were scored but NOT C8/11:
			 if(sum(c("C8", "C11")  %in%  names(scores_tbl)) == 0 &
		 		"press_spec_sc" %in%  names(scores_tbl) == TRUE) {

			 	# return list with scores_c910_mean as first output list
					out1 <- as.data.frame(scores_c910_mean)

			 } else {

				 ### Combine datasets if both exist
				 score_overview <- dplyr::left_join(scores_c811, scores_c910_mean, by = "ind")
				 score_overview <- score_overview %>% dplyr::select_(~c(ind,
				 	nr_sign_press, dplyr::everything()))

					# Convert NAs in nr_sign_press, C9, and C10 (due to lack of pressure responses) to zero
					score_overview[is.na(score_overview)] <- 0

					# Order variables
			  all_var <- c("ind", "nr_sign_press", unique(crit_scores$crit),
			  	paste0(unique(crit_scores$crit), "_in%"))
			  present_var <- names(score_overview)
			  order_var <- all_var[all_var %in% present_var]
			  score_overview <- score_overview[, order_var]

					# return sublist
					out1 <- as.data.frame(score_overview)

			 }
		 }



# Generate Output Table 1 -------------------------
	 # (Pressure-specific scores)

	 if("press_spec_sc" %in%  names(scores_tbl) == TRUE) {

	  out2 <- dplyr::select_(scores_c910, "-id")

	  ### Merge the total scores across subcriteria from calculation in output1,
	  # including the proportion

	  # Add proportions of total scores
	  for(i in 3:ncol(scores_c910_sum)) {
	  	crit_s <- names(scores_c910_sum)[i]
	  	new_var <- paste0(crit_s,"_in%")
	  	tsc <- total_sc$total_score[total_sc$crit == crit_s]
	  	scores_c910_sum[,new_var] <- round(scores_c910_sum[,i] / tsc * 100, 0)
	  }

	  # Order variables
	  order_var <-   c("ind", "press", unique(scores_c910_l$crit),
	  	paste0(unique(scores_c910_l$crit), "_in%"))
	  scores_c910_sum <- scores_c910_sum[, order_var]

	  out2 <- as.data.frame(dplyr::left_join(out2, scores_c910_sum, by = c("ind", "press")))

	 }

		 ### END OF FUNCTION ###

		 print_list <- vector("list", length = 2)
			print_list[[1]] <- out1
			print_list[[2]] <- out2
			names(print_list)[1] <- "overview"
		 names(print_list)[2] <- "subcriteria_per_press"

		 return(print_list)
}








summary_sc <- function(scores_tbl,
	 crit_scores = INDperform::crit_scores_tmpl) {

  # Data preparation -------------------

  # Get weighted scores for calculating total scores
  crit_scores$weighted_score <- crit_scores$score *
    crit_scores$weight

  # Get total scores per criterion
  total_sc <- crit_scores %>%
  	 dplyr::group_by_(.dots = c("crit", "subcrit")) %>%
    dplyr::summarise_(.dots = stats::setNames(list(~max(score)),
    "max_score")) %>%
  	 dplyr::group_by_(.dots = "crit") %>%
    dplyr::summarise_(.dots = stats::setNames(list(~sum(max_score)),
      "total_score"))

  # Separate data into general and pressure-specific
  # scores (here only significant pressures) but
  # check first if criteria are present in scoring
  # tibble
  if (sum(c("C8", "C11") %in% names(scores_tbl)) > 0) {
    scores_c811 <- scores_tbl %>%
    	 dplyr::select_(.dots = names(scores_tbl)[names(scores_tbl) %in%
        c("ind", "C8", "C11")])

    # Add proportions only to the
    for (i in 2:ncol(scores_c811)) {
      crit_s <- names(scores_c811)[i]
      new_var <- paste0(crit_s, "_in%")
      tsc <- total_sc$total_score[total_sc$crit == crit_s]
      scores_c811[, new_var] <- round(scores_c811[,
        i]/tsc * 100, 0)
    }
  }

  if ("press_spec_sc" %in% names(scores_tbl) == TRUE) {
    # Extract scores of sign.pressures
    scores_c910 <- scores_tbl %>%
    	 dplyr::select_(.dots = c("ind", "press_spec_sc")) %>%
      tidyr::unnest() %>%
      dplyr::filter(rowSums(dplyr::select_(.,
        .dots = c("-ind", "-press", "-id",
          "-press_type"))) > 0)
    # Make data long for the aggregation in output 1
    scores_c910_l <- scores_c910 %>%
    	 tidyr::gather_(key_col = "subcrit", value_col = "score",
        gather_cols = names(scores_c910)[!names(scores_c910) %in%
        c("ind", "id", "press", "press_type")])
    # Add the criteria
    scores_c910_l$crit <- sub("\\_.*", "", scores_c910_l$subcrit)
  }

  # Generate Output Table 1 -----------------------------
  # (Overview table where C9 and C10 are averaged
  #  across sign. press)

  ### If only C8 and/or 11 were scored but NOT C9/10:
  if (sum(c("C8", "C11") %in% names(scores_tbl)) >
    0 & "press_spec_sc" %in% names(scores_tbl) ==
    FALSE) {

    # Returned output
    print_list <- vector("list", length = 1)
    names(print_list)[1] <- "Overview"
    print_list[[1]] <- scores_c811

  } else {

    # Calculate sum across subcriteria in C9 and C10
    # and return to wide format (long format simply to
    # avoid if statements for checking for crit
    # presence)
    scores_c910_sum <- scores_c910_l %>%
    	 dplyr::group_by_(.dots = c("ind", "press", "crit")) %>%
      dplyr::summarise_(.dots = stats::setNames(list(~sum(score)),
        "sum_score")) %>%
    	 tidyr::spread_("crit", "sum_score")

    # Calculate number of sign. pressures and mean
    # across sign.pressures for C9/C10
    scores_c910_mean <- scores_c910_sum %>%
    	 dplyr::group_by_(.dots = "ind") %>%
      dplyr::summarise_(.dots = stats::setNames(list(~dplyr::n_distinct(press),
        ~round(sum(C9)/nr_sign_press, 1), ~round(sum(C10)/nr_sign_press,
          1)), c("nr_sign_press", "C9", "C10")))

    # Add proportions of total scores
    for (i in 3:ncol(scores_c910_mean)) {
      crit_s <- names(scores_c910_mean)[i]
      new_var <- paste0(crit_s, "_in%")
      tsc <- total_sc$total_score[total_sc$crit == crit_s]
      scores_c910_mean[, new_var] <- round(scores_c910_mean[,
        i] / tsc * 100, 0)
    }

    ### If only C9/10 were scored but NOT C8/11:
    if (sum(c("C8", "C11") %in% names(scores_tbl)) ==
      0 & "press_spec_sc" %in% names(scores_tbl) ==
      TRUE) {
      # return list with scores_c910_mean as first output
      # list
      out1 <- as.data.frame(scores_c910_mean)

    } else {

      ### Combine datasets if both exist
      score_overview <- dplyr::left_join(scores_c811,
        scores_c910_mean, by = "ind")
      score_overview <- score_overview %>% dplyr::select_(~c(ind,
        nr_sign_press, dplyr::everything()))

      # Convert NAs in nr_sign_press, C9, and C10 (due to
      # lack of pressure responses) to zero
      score_overview[is.na(score_overview)] <- 0

      # Order variables
      all_var <- c("ind", "nr_sign_press", unique(crit_scores$crit),
        paste0(unique(crit_scores$crit), "_in%"))
      present_var <- names(score_overview)
      order_var <- all_var[all_var %in% present_var]
      score_overview <- score_overview[, order_var]

      # return sublist
      out1 <- as.data.frame(score_overview)

    }
  }



  # Generate Output Table 2 -------------------------
  # (Pressure-specific scores)

  if ("press_spec_sc" %in% names(scores_tbl) == TRUE) {
    out2 <- dplyr::select_(scores_c910, "-id")

    ### Merge the total scores across subcriteria from
    ### calculation in output1, including the proportion

    # Add proportions of total scores
    for (i in 3:ncol(scores_c910_sum)) {
      crit_s <- names(scores_c910_sum)[i]
      new_var <- paste0(crit_s, "_in%")
      tsc <- total_sc$total_score[total_sc$crit == crit_s]
      scores_c910_sum[, new_var] <- round(scores_c910_sum[,
        i]/tsc * 100, 0)
    }

    # Order variables
    order_var <- c("ind", "press", unique(scores_c910_l$crit),
      paste0(unique(scores_c910_l$crit), "_in%"))
    scores_c910_sum <- scores_c910_sum[, order_var]

    out2 <- as.data.frame(dplyr::left_join(out2,
      scores_c910_sum, by = c("ind", "press")))

    ### END OF FUNCTION ###

    print_list <- vector("list", length = 2)
    print_list[[1]] <- out1
    print_list[[2]] <- out2
    names(print_list)[1] <- "overview"
    names(print_list)[2] <- "subcriteria_per_press"

  }
  return(print_list)

}
