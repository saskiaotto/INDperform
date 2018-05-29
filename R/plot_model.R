#' Visualization of all IND~pressure-model results relevant for the scoring
#'
#' \code{plot_model} creates a tibble with up to 4 individual plots and one
#' combined plot (all ggplot2 objects) for each IND~pressure pair in the
#' input tibble. The number of plots generated depends on the information
#' provided in the input tibble. If all model IND~pressure modelling functions
#' have been applied to create the final input tibble all five plots will be
#' produced.
#'
#' @param init_tbl The output tibble of the \code{\link{ind_init}} function.
#' @param mod_tbl Any output tibble from the IND~pressure modelling functions.
#' @param choose_thresh_gam Selects the threshold_GAM for the thresh_plot,
#'  which is relevant if several models are listed in 'thresh_models'.
#'
#'  The default is NULL, which shows the best performing threshold_GAM
#'  (based on the GCV as selection criterion).
#' @param pos_label Specifies the position of the annotation in the plot. Should
#'  be one of "topleft" (default), "topright", "bottomleft" or "bottomright".
#'  For more details see \code{\link{place_text}}.
#' @param header logical; if TRUE, each plot will have a header including the IND
#'  name, pressure name(s) and the model type.
#'
#' @return
#' The function returns a \code{\link[tibble]{tibble}}, including the
#' following elements:
#' \describe{
#'   \item{\code{id}}{Numerical IDs of the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{response_plot}}{A list-column of ggplot2 objects that show
#'               the observed (black points) and predicted IND response to the single
#'               pressure (based on the training data). The solid blue line represents
#'               the predicted mean and the transparent polygon the 95\% confidence interval.
#'               The effective degrees of freedom (edf), R_sq, and p-value from the
#'               fittel model fitted are additionally provided. The input needed for this
#'               plot is generated from the \code{\link{model_gam}} or
#'               \code{\link{model_gamm}} functions.}
#'   \item{\code{predict_plot}}{A list-column of ggplot2 objects that show the robustness
#'               of the modelled relationship expressed as the predictive performance
#'               (the NRMSE) on a test dataset, e.g the last years of the time series.
#'               The solid green line represents the predicted IND value given the
#'               observed pressure value for that particular year (both in the training and
#'               test data, the latter indicated in with green triangles). The transparent
#'               polygon represents the 95\% confidence interval. Observed IND values
#'               of the test data are shown as black triangle, the trainings observations
#'               are presented as black circles. The input needed for this plot is
#'               generated from the \code{\link{model_gam}} or
#'               \code{\link{model_gamm}} functions.}
#'   \item{\code{deriv_plot}}{A list-column of ggplot2 objects that show the first
#'               derivatives (S') of non-linear IND~pressure response curves (edf > 1.5)
#'               and the proportion of the pressure range where the IND shows no further
#'               significant change (i.e., slope approximates zero). Black triangles
#'               represent values at the pressure's boundary where the zero line falls
#'               into the confidence interval, which indicates no further significant
#'               IND change. Circle represent values that were considered positive
#'               for the calculation of the pressure range (see for more details
#'               \code{\link{calc_deriv}}). The input needed for this plot is generated
#'               from the \code{\link{calc_deriv}} function.}
#'   \item{\code{thresh_plot}}{A list-column of ggplot2 objects that show the observed IND
#'               response curve for a specific pressure under a low (left panel, in black)
#'               and high (right panel, in red) regime of an interacting 2nd pressure
#'               variable. The solid lines represent the predicted mean and the transparent
#'               polygons the 95\% confidence intervals. Filled circles represent the
#'               observed training observations in each regime. If no thresh_plot is
#'               created for that IND~pressure pair, no interaction was found. If more
#'               than one interacting pressure variable has been detected, i.e. more than
#'               one threshold-GAM performed better than its corresponding GAM, the
#'               threshold-GAM with the best GCV will be displayed. The input needed for
#'               this plot is generated from the \code{\link{test_interaction}} function.
#'               If the plot shows strange patterns such as smoothers hardly differ in both
#'               regimes with wide confidence intervals at the edge or few data points in one
#'               regime check the model diagnostics of this threshold model! Outliers can cause
#'               such patterns or if threshold is at the edge of the pressure range or other
#'               thresholds are similarly likely (see also \code{\link{plot_diagnostics}}.)}
#'   \item{\code{all_plots}}{A list-column of ggplot2 objects that show all
#'              plots together using additional drawing canvas from the
#'              \code{cowplot} package on top of ggplot2.}
#' }
#'
#'
#' @family IND~pressure modelling functions
#'
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data in this package
#' mod_tbl <- all_results_ex[4:5, ]
#' init_tbl <- ind_init_ex[4:5, ]
#' dat <- plot_model(init_tbl, mod_tbl, pos_label = "topleft")
#' dat$response_plot[[1]]
#' dat$predict_plot[[1]]
#' dat$deriv_plot[[2]]
#' dat$thresh_plot[[2]]
#' dat$all_plots[[2]]
#'
#' \dontrun{
#'  # Apply function to all sign. models and save specific plots
#'  id <- which(all_results_ex$p_val <= 0.05)
#'  init_tbl <- ind_init_ex[id, ]
#'  mod_tbl <- all_results_ex[id, ]
#'  dat <- plot_model(init_tbl, mod_tbl, pos_label = "bottomright")
#'  #pdf("Plot.pdf", height=10, width=10)
#'  #dat$all_plots
#'  #dev.off()
#' }
plot_model <- function(init_tbl, mod_tbl, choose_thresh_gam = NULL,
  pos_label = "topleft", header = TRUE) {

  # Data input validation ---------------------
  if (missing(init_tbl)) {
	 	stop("Argument 'init_tbl' is missing.")
  }
	if (missing(mod_tbl)) {
	 	stop("Argument 'mod_tbl' is missing.")
	 }
  # Check input tibbles
  init_tbl <- check_input_tbl(init_tbl, tbl_name = "init_tbl",
    parent_func = "ind_init()", var_to_check = c("id",
      "ind", "press", "ind_train", "press_train",
      "time_train", "ind_test", "press_test",
      "time_test", "train_na"), dt_to_check = c("integer",
      "character", "character", rep("list", 7)))
  mod_tbl <- check_input_tbl(mod_tbl, tbl_name = "mod_tbl",
    parent_func = "model_gam(), model_gamm()/select_model(), calc_deriv() or test_interaction()",
    var_to_check = c("id", "ind", "press", "edf",
      "p_val", "r_sq", "expl_dev", "nrmse", "model"),
    dt_to_check = c("integer", "character", "character",
      "numeric", "numeric", "numeric", "numeric",
      "numeric", "list"))

  # Check if init_tbl represents the same full set or
  # subset of IND-pressure combinations than mod_tbl
  # and otherwise filter for mod_tbl$id (if there is
  # any id missing in init_tbl return error message)
  # and sort in the same order
  if (!identical(init_tbl$id, mod_tbl$id)) {
    if (all(mod_tbl$id %in% init_tbl$id)) {
      init_tbl <- init_tbl[match(mod_tbl$id,
        init_tbl$id), ]
      # (match() with mod_tbl as first argument makes
      # sure only those in the same order are selected)
    } else {
      stop("Not all ids in mod_tbl are provided in init_tbl.")
    }
  }

  # Check if the chosen value for choose_thresh_gam
  # exceeds the minimum number of threshold-GAMs
  # listed in 'thresh_models':
  if (any(grepl("interaction", names(mod_tbl)) ==
    TRUE)) {
    if (!is.null(choose_thresh_gam)) {
      temp <- mod_tbl$thresh_models %>% purrr::compact(.) %>%
        purrr::discard(., is.na) %>% purrr::map(.,
        ~length(.) < choose_thresh_gam) %>%
        purrr::keep(., isTRUE)
      if (length(temp) > 0) {
        stop(paste0("The selected value for choose_thresh_gam exceeds the minimum number of thresh_gams for some ids. Select a lower value."))
      }
    }
  }


  # Input data for all ------------------------

  # Sort init_tbl and mod_tbl by id to make sure row order is same
  mod_tbl <- dplyr::arrange_(mod_tbl, .dots = "id")
  init_tbl <- dplyr::arrange_(init_tbl, .dots = "id")

  # Combine train/ test data and calculate pred on
  # observed press and sequence
  time <- purrr::map(1:length(init_tbl$time_train),
  	~ sort(c(init_tbl$time_train[[.]], init_tbl$time_test[[.]])))
  id_train <- purrr::map(1:length(time), ~which(time[[.]] %in%
    init_tbl$time_train[[.]]))
  id_test <- purrr::map(1:length(time), ~which(!time[[.]] %in%
    init_tbl$time_train[[.]]))

  # For text placement
  props_p1 <- vector("list", length = 4)
  props_p1$topleft <- data.frame(x_prop = 0, y_prop = 0.1)
  props_p1$topright <- data.frame(x_prop = 0.2, y_prop = 0.1)
  props_p1$bottomleft <- data.frame(x_prop = 0, y_prop = 0.1)
  props_p1$bottomright <- data.frame(x_prop = 0.2,
    y_prop = 0.1)

  props_p2 <- vector("list", length = 4)
  props_p2$topleft <- data.frame(x_prop = 0, y_prop = 0.05)
  props_p2$topright <- data.frame(x_prop = 0.25,
    y_prop = 0.05)
  props_p2$bottomleft <- data.frame(x_prop = 0, y_prop = 0.1)
  props_p2$bottomright <- data.frame(x_prop = 0.25,
    y_prop = 0.1)

  props_p3 <- vector("list", length = 4)
  props_p3$topleft <- data.frame(x_prop = 0, y_prop = 0.1)
  props_p3$topright <- data.frame(x_prop = 0.3, y_prop = 0.1)
  props_p3$bottomleft <- data.frame(x_prop = 0, y_prop = 0.1)
  props_p3$bottomright <- data.frame(x_prop = 0.3,
    y_prop = 0.1)


  # Plot 1 - Response curve ---------------------

  ind_train <- init_tbl$ind_train
  press_train <- init_tbl$press_train
  press_train_seq <- vector(mode = "list", length = nrow(mod_tbl))
  for (i in 1:length(press_train_seq)) {
    press_vector <- press_train[[i]]
    x_seq <- seq(min(press_vector, na.rm = TRUE),
      max(press_vector, na.rm = TRUE), length.out = length(press_vector))
    press_train_seq[[i]] <- x_seq
  }
  pred_train <- calc_pred(model_list = mod_tbl$model,
    obs_press = press_train_seq)$pred
  ci_low_train <- calc_pred(model_list = mod_tbl$model,
    obs_press = press_train_seq)$ci_low
  ci_up_train <- calc_pred(model_list = mod_tbl$model,
    obs_press = press_train_seq)$ci_up

  # Get ranges for text position
  x_range <- purrr::map(.x = press_train_seq, .f = range)
  y_range <- purrr::pmap(.l = list(ind_train, pred_train,
    ci_low_train, ci_up_train), .f = calc_y_range)

  # Text position
  if (pos_label == "topleft") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p1[[pos_label]]$x_prop,
        y_prop = props_p1[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "topright") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p1[[pos_label]]$x_prop,
        y_prop = props_p1[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "bottomleft") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p1[[pos_label]]$x_prop,
        y_prop = props_p1[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "bottomright") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p1[[pos_label]]$x_prop,
        y_prop = props_p1[[pos_label]]$y_prop,
        pos = pos_label))
  }

  # Create annotation label and axes labels
  label <- paste0("edf = ", round(mod_tbl$edf, digits = 2),
    "\nR_sq = ", round(mod_tbl$r_sq, digits = 2),
    "\np = ", round(mod_tbl$p_val, digits = 2))
  xlab <- as.list(init_tbl$press)
  ylab <- as.list(init_tbl$ind)


  # Apply plot function to all models in list
  # (if input has values)
  p1 <- purrr::map(1:length(ind_train),
  		~ if ( all(is_value(pred_train[[.]])) ) {
  					plot_response(x = press_train[[.]], y = ind_train[[.]], x_seq = press_train_seq[[.]],
  						pred = pred_train[[.]], ci_up = ci_up_train[[.]], ci_low = ci_low_train[[.]],
  					xlab = xlab, ylab = ylab, pos_text = pos_text, label = label[[1]]
  					)
  			} else {plot_empty()})


  # Plot 2 - Predictive performance
  # ---------------------

  ind <- purrr::map(1:length(init_tbl$ind_train),
  	~ c(init_tbl$ind_train[[.]],
  		init_tbl$ind_test[[.]])[ order(c(id_train[[.]], id_test[[.]])) ])
  press <- purrr::map(1:length(init_tbl$press_train),
  	~ c(init_tbl$press_train[[.]],
  		init_tbl$press_test[[.]])[ order(c(id_train[[.]], id_test[[.]])) ])
  pred <- calc_pred(model_list = mod_tbl$model, obs_press = press)$pred
  ci_low <- calc_pred(model_list = mod_tbl$model,
    obs_press = press)$ci_low
  ci_up <- calc_pred(model_list = mod_tbl$model,
    obs_press = press)$ci_up

  # To zoom into the test data only
  zoom_x_range <- function(time, id_test) {
    time_range <- range(time[id_test])
    time_range_ext <- c((time_range[1] - 2), (time_range[2] +
      1))
    return(time_range_ext)
  }
  x_range <- purrr::map2(time, id_test, ~zoom_x_range(.x,
    .y))
  # Get subsets of x_range
  zoom <- purrr::map(id_test, ~c((min(.) - 2):(max(.) +
    1)))
  # exclude zero and neg. indices (as they don't extist
  # and were only produced when id_test starts with 1 or 2)
  zoom <- purrr::map(zoom, ~ .[. > 0])
  # Get also zoomed y-range for text position
  y_range <- purrr::pmap(.l = list(ind, pred, ci_low,
    ci_up, zoom), .f = calc_y_range)

  # Text position
  if (pos_label == "topleft") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p2[[pos_label]]$x_prop,
        y_prop = props_p2[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "topright") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p2[[pos_label]]$x_prop,
        y_prop = props_p2[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "bottomleft") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p2[[pos_label]]$x_prop,
        y_prop = props_p2[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "bottomright") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p2[[pos_label]]$x_prop,
        y_prop = props_p2[[pos_label]]$y_prop,
        pos = pos_label))
  }

  # Create annotation label and axes labels
  label <- paste0("NRMSE = ", round(mod_tbl$nrmse,
    digits = 2))
  xlab <- "Time"
  ylab <- as.list(init_tbl$ind)

  # Apply plot_predict to all models in list
  # (if input has values)
		p2 <- purrr::map(1:length(ind),
			~ if ( all(is_value(pred[[.]])) ) {
				plot_predict(x = time[[.]], y_obs = ind[[.]], y_pred = pred[[.]],
					ci_up = ci_up[[.]], ci_low = ci_low[[.]],
					x_train = id_train[[.]], x_test = id_test[[.]], zoom = zoom[[.]],
					x_range = x_range[[.]], y_range = y_range[[.]], xlab = xlab,
					ylab = ylab[[.]], pos_text = pos_text[[.]], label = label[[.]]
				)
			} else {plot_empty()})



  # Plot 3 - Derivatives of non-linear smoothers
  # -----------

  if ("zero_in_conf" %in% names(mod_tbl)) {
    press_seq <- mod_tbl$press_seq
    deriv1 <- mod_tbl$deriv1
    deriv1_ci_up <- mod_tbl$deriv1_ci_up
    deriv1_ci_low <- mod_tbl$deriv1_ci_low
    zero_in_conf <- purrr::map(mod_tbl$zero_in_conf,
      as.numeric)
    zic_start_end <- purrr::map(mod_tbl$zic_start_end,
      as.numeric)

    # Get ranges for text position
    x_range <- suppressWarnings(purrr::map(.x = press_seq,
      range, na.rm = TRUE))
    y_range <- suppressWarnings(purrr::pmap(.l = list(y1 = deriv1,
      ci_low = deriv1_ci_up, ci_up = deriv1_ci_low),
      .f = calc_y_range))

    # Text position
    if (pos_label == "topleft") {
      pos_text <- purrr::map2(.x = x_range, .y = y_range,
        .f = ~place_text(.x, .y, x_prop = props_p3[[pos_label]]$x_prop,
          y_prop = props_p3[[pos_label]]$y_prop,
          pos = pos_label))
    }
    if (pos_label == "topright") {
      pos_text <- purrr::map2(.x = x_range, .y = y_range,
        .f = ~place_text(.x, .y, x_prop = props_p3[[pos_label]]$x_prop,
          y_prop = props_p3[[pos_label]]$y_prop,
          pos = pos_label))
    }
    if (pos_label == "bottomleft") {
      pos_text <- purrr::map2(.x = x_range, .y = y_range,
        .f = ~place_text(.x, .y, x_prop = props_p3[[pos_label]]$x_prop,
          y_prop = props_p3[[pos_label]]$y_prop,
          pos = pos_label))
    }
    if (pos_label == "bottomright") {
      pos_text <- purrr::map2(.x = x_range, .y = y_range,
        .f = ~place_text(.x, .y, x_prop = props_p3[[pos_label]]$x_prop,
          y_prop = props_p3[[pos_label]]$y_prop,
          pos = pos_label))
    }

    # Create annotation label and axes labels
    label <- paste0(paste0("Response to ", round(mod_tbl$prop,
      digits = 2) * 100), "% \nof pressure range")
    xlab <- init_tbl$press
    ylab <- rep(init_tbl$ind)

    # Apply plot_deriv if derivative data in tibble
    p3 <- vector(mode = "list", length = nrow(mod_tbl))
    for (i in 1:nrow(mod_tbl)) {
      if (!is.null(mod_tbl$zero_in_conf[[i]])) {
        # [[]] needed here to extract NULL values in
        # sublist
        p3[[i]] <- plot_deriv(press_seq[[i]],
          deriv1[[i]], deriv1_ci_low[[i]],
          deriv1_ci_up[[i]], zic_start_end[[i]],
          zero_in_conf[[i]], xlab[i], ylab[i],
          pos_text[[i]], label[i])
      } else {
        p3[[i]] <- plot_empty()
      }
    }
  } else {
    p3 <- list(plot_empty())
  }


  # Plot 4 - Plot strongest interaction:
  # ------------------ (show best performing
  # thresh_model)

  if ("interaction" %in% names(mod_tbl)) {
    p4 <- vector(mode = "list", length = nrow(mod_tbl))
    for (i in 1:nrow(mod_tbl)) {
      if (isTRUE(mod_tbl$interaction[i]) &
      		suppressWarnings(!any(is.na(mod_tbl$thresh_models[[i]]),
      			is.null(mod_tbl$thresh_models[[i]]))) )	{
        p4[[i]] <- plot_thresh(mod_tbl$thresh_models[[i]],
          choose_thresh_gam)
      } else {
        p4[[i]] <- plot_empty()
      }
    }
  } else {
    p4 <- list(plot_empty())
  }


  # All Plots combined -----------------------------

  # Title
  if (header) {
    title <- paste0(mod_tbl$ind, " ~ ", mod_tbl$press)
  } else {
    title <- ""
  }

  all_plots <- purrr::pmap(.l = list(p1 = p1, p2 = p2,
    p3 = p3, p4 = p4, title = title), plot_all_mod)


  # Generate output tibble of plot objects ---------

  plot_tab <- tibble::tibble(id = mod_tbl$id, ind = mod_tbl$ind,
    press = mod_tbl$press, response_plot = p1,
    predict_plot = p2, deriv_plot = p3, thresh_plot = p4,
    all_plots = all_plots)

  # Insert NA in single plots if
  # required variables not in input or no plot
  # generated per id as edf=1 / interaction = FALSE
  plot_tab$response_plot[!purrr::map_lgl(pred_train,
  	~ all(is_value(.)))] <- NA

  plot_tab$predict_plot[!purrr::map_lgl(pred,
  	~ all(is_value(.)))] <- NA

  if ("zero_in_conf" %in% names(mod_tbl)) {
    sel <- purrr::map_lgl(mod_tbl$zero_in_conf,
      is.null)
    plot_tab$deriv_plot[sel] <- NA
  } else {
    plot_tab$deriv_plot <- NA
  }

  if ("interaction" %in% names(mod_tbl)) {
    plot_tab$thresh_plot[is.na(mod_tbl$interaction) |
    		!mod_tbl$interaction |
    		(mod_tbl$interaction & suppressWarnings(any(is.na(mod_tbl$thresh_models[[i]]),
      			is.null(mod_tbl$thresh_models[[i]]))) )	 ] <- NA
  } else {
    plot_tab$thresh_plot <- NA
  }


  ### END OF FUNCTION ###
  return(plot_tab)
}


# Internal helper function --------------------

# Get full y-range shown in plot
calc_y_range <- function(y1, y2 = NULL, ci_low, ci_up,
  zoom = NULL) {
  # x: list of vectors (can have any length)
  if (is.null(zoom)) {
    out <- range(c(y1, y2, ci_low, ci_up), na.rm = TRUE)
  } else {
    out <- range(c(y1[zoom], y2[zoom], ci_low[zoom],
      ci_up[zoom]), na.rm = TRUE)
  }
  return(out)
}
