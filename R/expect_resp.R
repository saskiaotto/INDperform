#' Score adjustments for subcriterion 10.1
#'
#' \code{expect_resp} runs a shiny app in which the expectation of the IND response to
#' a pressure (subcriterion 10.1) can be manually changed to 'yes' or 'no' based on
#' visual inspection of the IND response curve.
#'
#' @param mod_tbl Output tibble from the IND~pressure modelling functions.
#' @param scores_tbl The output tibble from the \code{\link{scoring}} function.
#' @param crit_scores The(un)modified criterion-scoring template \code{crit_scores_tmpl};
#'  has to be the same than used in \code{scoring}. Default is the unmodified
#'  template \code{crit_scores_tmpl}.
#'
#' @details
#' The subcriterion 10.1 (i.e. the IND response to a pressure, which has been found significant,
#' is in line with expectations based on ecological knowledge) has been set
#' to a default score of 1 (no expectation / unclear as response is highly non-linear)
#' in the \code{\link{scoring}} function. Determining whether the IND response modelled in
#' the GAM/GAMM meets specific expectations can only be done
#' based on visual model inspections. \code{expect_resp} provides only a very simple
#' graphical representation of this smoothing function.
#'
#' For a more comprehensive figure use the \code{\link{plot_model}}
#' function and then go back to this function for modifications
#' of the expectation scores.
#'
#' @return
#' The function returns the input scoring tibble, but with modified scores
#' in the variable \code{C10_1}, once the "Press Me!" button is activated.
#'
#' @seealso \code{\link{plot_model}} for visualization of the IND responses to pressures
#' @family score-based IND performance functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # Using the Baltic Sea demo data:
#'  # Apply first the scoring on the model outputs
#'  scores_tbl <- scoring(trend_tbl = model_trend_ex, mod_tbl = all_results_ex,
#'    press_type = press_type_ex)
#'  # Then run the expect_resp() shiny function to correct one criterion
#'  scores_tbl <- expect_resp(all_results_ex, scores_tbl)
#'  # Check if it worked:
#'  expect_resp(all_results_ex, scores_tbl)
#' }

expect_resp <- function(mod_tbl, scores_tbl,
	 crit_scores = INDperform::crit_scores_tmpl) {

		# Data input validation ----------------------
		 if (missing(mod_tbl)) {
	 	stop("Argument 'mod_tbl' is missing.")
		 }
		 if (missing(scores_tbl)) {
	 	stop("Argument 'scores_tbl' is missing.")
	 }

	 # Check input tibbles
		mod_tbl <- check_input_tbl(
			mod_tbl, tbl_name = "mod_tbl", parent_func = "model_gam() or model_gamm()/select_model()",
			var_to_check = c("id", "model"), dt_to_check = c("integer", "list")
		)

	 scores_tbl <- check_input_tbl(
				scores_tbl, tbl_name = "scores_tbl", parent_func = "scoring()",
				var_to_check = c("ind", "press_spec_sc"), dt_to_check = c("character", "list")
		)

		# Check if subcrit 10_1 exists in the crit_scores table, if TRUE continue
		names_press_spec_sp <- scores_tbl %>%
			dplyr::select_("press_spec_sc") %>% tidyr::unnest() %>% names(.)
		if("C10_1" %in% names_press_spec_sp == FALSE) {
			 stop("There is no scored subcriterion 10_1 in your scoring output table, which can be modified. You need to include it in the scoring function")
		}

	 # Data preparation ----------------------

	 crit_scores$weighted_score <- crit_scores$score * crit_scores$weight

	 # Create model output table
	 scores_tbl_press <- scores_tbl %>%
		 dplyr::select_(.dots = c("ind", "press_spec_sc")) %>%
		 tidyr::unnest()

	 dat <-		scores_tbl_press %>%
		 dplyr::filter(rowSums(dplyr::select_(.,
			 .dots = c("-ind","-press", "-id", "-press_type"))) > 0) %>%
		 dplyr::select_(.dots = c("id", "ind", "press", "C10_1"))

	 # Convert score into factor showing only type
	 # (for table displayed in the shiny app)
	 dat$response_as_expected <- factor(dat$C10_1,
		 levels = crit_scores$weighted_score[crit_scores$subcrit == "C10_1"],
		 labels = crit_scores$score_explanation[crit_scores$subcrit == "C10_1"])

	 # Select few variables only for table displayed
	 dat <- dplyr::select_(dat,
		 .dots = c("id", "ind", "press", "response_as_expected") )

	 # Convert to dataframe otherwise rhandsontable has problems
	 # showing all factor levels
	 dat <- as.data.frame(dat)
	 dat <- dplyr::arrange_(dat, .dots="id")

	 # Generate 'rhandsontable' including the figures ------------

	 # Split first the model data based on whether the pressure effect
	 # was considered in the scoring (i.e. same rows as in dats)
	 mod_tbl_split <- mod_tbl[mod_tbl$id %in% dat$id, ] %>%
	 	dplyr::arrange_(.dots="id")

	 # return error message if mod_tbl_split is empty (no sign. IND~PRESS)
	 if (nrow(mod_tbl_split) == 0) {
	 	 stop("There is IND~PRESS model where scores can be adjusted.")
	 }


		# Helper function to create 'sparkline.js' charts
		chart_func <- function(model) {
			if(class(model)[1] == "gam") {
				df <- data.frame(press = seq(min(model$model[,2]), max(model$model[,2]),
					length.out=20))
				names(df)[1] <- names(model$model)[2]
				pred <- mgcv::predict.gam(model, newdata=df, type="response")
				jsonlite::toJSON(list(values=	pred,options = list(type = "line")))
			} else {
				df <- data.frame(press = seq(min(model$gam$model[,2]), max(model$gam$model[,2]),
					length.out=20))
				names(df)[1] <- names(model$gam$model)[2]
				pred <- mgcv::predict.gam(model$gam, newdata=df, type="response")
				jsonlite::toJSON(list(values=	pred, options = list(type = "line")))
			}
		}  # end of helper function

		# Apply helper function and save figures in new variable
		dat$response <- vapply(mod_tbl_split$model, FUN = chart_func, FUN.VALUE = character(1))


  # Now comes the shiny app part ... ------------

		#*****
		# This code starts the shiny app and saves the edited table
		# under "edited_tbl"
		edited_tbl <- shiny::runApp(list(

			ui = shiny::fluidPage(

				shiny::titlePanel("Score subcriterion 10.1: Indicator response to pressure as expected?"),

				shiny::sidebarLayout(
					shiny::sidebarPanel(
						shiny::helpText("The current scoring of subcriterion 10.1 is displayed.",
							"Check whether you want to change it. If so, choose another",
							"level in the column 'response_as_expected. Once you are done,",
							"press the 'Press Me!' button, which saves the table and closes the window",
							" (if you opened this shiny app in a browser, close manually the window",
							"after you pressed the 'Press Me' button)."),

						shiny::wellPanel(
							shiny::h3("Save table and close window"),
							shiny::actionButton("save", "Press Me!")
						)
					),

					shiny::mainPanel(
						rhandsontable::rHandsontableOutput("hot")
					)
				)
			),

			server = function(input, output, session) {
				values <- shiny::reactiveValues()

				## Handsontable
				shiny::observe({
					if (!is.null(input$hot)) {
						values[["previous"]] <- shiny::isolate(values[["dat"]])
						dat = rhandsontable::hot_to_r(input$hot)
					} else {
						if (is.null(values[["dat"]]))
							dat <- dat
						else
							dat <- values[["dat"]]
					}
					values[["dat"]] <- dat
				})

				output$hot <- rhandsontable::renderRHandsontable({
					dat <- values[["dat"]]
					if (!is.null(dat))
						rhandsontable::rhandsontable(dat, stretchH = "all",
							readOnly = TRUE, height=500) %>%
						rhandsontable::hot_col("response_as_expected",
							readOnly=FALSE, allowInvalid=FALSE) %>%
						rhandsontable::hot_col("response",
							renderer = htmlwidgets::JS("renderSparkline"))
				})

				## Save
				shiny::observeEvent(input$save, {
					final_dat <- shiny::isolate(values[["dat"]])
					shiny::stopApp(final_dat)
				})
			}
		)
	) # end of shiny::runApp()

		#******

		### Modify the edited table
		edited_tbl$C10_1 <- as.numeric(edited_tbl$response_as_expected) - 1
		  # need to substract 1 as no zero factor level
  edited_tbl <- dplyr::select_(edited_tbl, .dots = "-response")

  # Replace the old C10_1 values by the new ones (for the sign.
  # pressures)
		pre_out <- scores_tbl_press
		for(i in 1:nrow(pre_out)) {
			if(pre_out$id[i] %in% edited_tbl$id) {
				pre_out$C10_1[i] <- edited_tbl$C10_1[match(pre_out$id[i],
					edited_tbl$id)]
			}
		}

		# Convert data into the original nested tibble for return
		pre_out <- pre_out %>%
			dplyr::group_by_("ind") %>%
			tidyr::nest(.key = "press_spec_sc")

		# Merge back into the old
  out <- scores_tbl %>%
			dplyr::select_(.dots = "-press_spec_sc") %>%
			dplyr::left_join(pre_out, by = "ind")


  ### END OF FUNCTION
		return(out)


}
