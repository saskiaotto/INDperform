#' Score-based distance matrix
#'
#' The function computes a distance matrix based on the \code{\link{scoring}} output
#' tibble (or the output tibble from the \code{\link{expect_resp}} function).
#'
#' @param scores_tbl The output tibble from the scoring.
#' @param method_dist Dissimilarity index used in the vegdist function to
#'  to calculate the dissimilarity matrix based on the scores.
#'
#'  Default is "Euclidean", for alternatives see \code{\link[vegan]{vegdist}}.
#' @param ... Further arguments to be passed to the method \code{vegdist}.
#'
#' @return
#' The function returns a \code{\link[stats]{dist}} object.
#'
#' @seealso \code{\link[vegan]{vegdist}} for the computation of the
#' dissimilarity index
#' @family score-based IND performance functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data
#' scores_tbl <- scoring(trend_tbl = model_trend_ex,
#'   mod_tbl = all_results_ex, press_type = press_type_ex)
#' dist_matrix <- dist_sc(scores_tbl, method_dist = "euclidean")
dist_sc <- function(scores_tbl, method_dist = "euclidean",
  ...) {

	 # Data input validation -----------------------
	 # Check input tibble
  scores_tbl <- check_input_tbl(scores_tbl, tbl_name = "scores_tbl",
    parent_func = "scoring()", var_to_check = c("ind"),
    dt_to_check = c("character"))

  # Data preparation for calculating distance matrix ------

  # Seperate data into general and pressure-specific
  # scores but check first if criteria are present in
  # scoring tibble
  if (sum(c("C8", "C11") %in% names(scores_tbl)) >
    0) {
    scores_c811 <- scores_tbl %>%
    	dplyr::select_(.dots = names(scores_tbl)[names(scores_tbl) %in%
      c("ind", "C8", "C11")])
  }

  if ("press_spec_sc" %in% names(scores_tbl) == TRUE) {

	  	# Extract scores of sign. pressures
	  	scores_c910 <- scores_tbl %>%
	  		dplyr::select_(.dots = c("ind",
	  			"press_spec_sc")) %>% tidyr::unnest()
	  	# Make data long for calculating total scores per
	  	# criterion
	  	scores_c910_l <- scores_c910 %>%
	  		tidyr::gather_(key_col = "subcrit", value_col = "score",
	  			gather_cols = names(scores_c910)[!names(scores_c910) %in%
	  				c("ind", "id", "press", "press_type")])
	  	# Add the criteria
	  	scores_c910_l$crit <- sub("\\_.*", "",
	  		scores_c910_l$subcrit)

	  	# Calculate sum across subcriteria in C9 and C10
	  	scores_c910_sum <- scores_c910_l %>%
	  		dplyr::group_by_(.dots = c("ind", "press", "crit")) %>%
	  		dplyr::summarise_(.dots = stats::setNames(list(~sum(score)),
	  			"score")) %>%
	  		dplyr::ungroup(.)  # needed for later operations

	  	# Add new variable that identifies pressure and crit
	  	scores_c910_sum$press_crit <- paste(scores_c910_sum$press,
	  		scores_c910_sum$crit, sep = "_")

	  	# Make datasets wide for merging with scores_c811
	  	scores_c910w <- scores_c910_sum %>%
	  		dplyr::select_("ind", "press_crit", "score") %>%
	  		tidyr::spread_("press_crit", "score")

    }  # end of if statement for C9/10 scores


  # Merging of data depending on criteria included ------

  # If only C8 and/or 11 were scored but NOT C9/10:
  if (sum(c("C8", "C11") %in% names(scores_tbl)) >
    0 & "press_spec_sc" %in% names(scores_tbl) ==
    FALSE) {

    clust_matrix <- as.data.frame(scores_c811)
    rownames(clust_matrix) <- clust_matrix$ind
    clust_matrix$ind <- NULL

  } else {

    # If only C9/10 were scored but NOT C8/11:
    if (sum(c("C8", "C11") %in% names(scores_tbl)) ==
      0 & "press_spec_sc" %in% names(scores_tbl) ==
      TRUE) {

      clust_matrix <- as.data.frame(scores_c910w)
      rownames(clust_matrix) <- clust_matrix$ind
      clust_matrix$ind <- NULL

    } else {

      # Combine datasets if both exist
      clust_matrix <- as.data.frame(dplyr::left_join(scores_c811,
        scores_c910w, by = "ind"))
      rownames(clust_matrix) <- clust_matrix$ind
      clust_matrix$ind <- NULL

    }
  }

  # Check if dimensions are correct (min. of 2
  # criteria scored)

  if (ncol(clust_matrix) < 2) {
    message("You have only one criterion for computing your distance matrix!")
  }

  # Calculate distance matrix (default Euclidean) ------
  score_dist <- vegan::vegdist(clust_matrix,
  	method = method_dist)


  ### END OF FUNCTION

  return(score_dist)

}
