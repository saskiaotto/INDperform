#' Create indicator trend plot
#'
#' \code{plot_trend} creates for each indicator (IND) in the input tibble a
#' time series plot including the smoothed trend with 95\% confidence interval
#'  and the corresponding p- value based on the IND ~ time GAM.
#'
#' @param trend_tbl Output tibble from the \code{\link{model_trend}} function.
#' @inheritParams plot_model
#'
#' @return The function returns a list of \code{\link[ggplot2]{ggplot}} objects;
#'  one for each indicator.
#'
#' @seealso \code{\link{model_trend}} that generates the model tibble for this function
#'
#' @export
#'
#' @examples
#' # Using the example data
#' trend_tbl <- model_trend_ex
#' pt <- plot_trend(trend_tbl)
#' # Show single plots using indicator names or indices
#' pt[[2]]
#' pt$Sprat
#' # Show all plots together
#' gridExtra::grid.arrange(grobs = pt)
plot_trend <- function(trend_tbl, pos_label = "topleft") {

  # Data input validation ---------------------
  if (missing(trend_tbl)) {
    stop("Argument trend_tbl is missing.")
  }
  # Check input tibble
  trend_tbl <- check_input_tbl(trend_tbl, tbl_name = "trend_tbl",
    parent_func = "model_trend()", var_to_check = c("ind",
      "p_val", "ind_train", "time_train", "pred",
      "ci_up", "ci_low"), dt_to_check = c("character",
      "numeric", "list", "list", "list", "list",
      "list"))
  # -----------------------------------------

  # For text placement
  props_p <- vector("list", length = 4)
  props_p$topleft <- data.frame(x_prop = 0, y_prop = 0.1)
  props_p$topright <- data.frame(x_prop = 0.2, y_prop = 0.1)
  props_p$bottomleft <- data.frame(x_prop = 0, y_prop = 0.1)
  props_p$bottomright <- data.frame(x_prop = 0.2,
    y_prop = 0.1)

  # Get ranges for text position
  x_range <- purrr::map(.x = trend_tbl$time_train,
    .f = range)
  y_range <- purrr::pmap(.l = list(trend_tbl$ind_train,
    trend_tbl$pred, trend_tbl$ci_up, trend_tbl$ci_low),
    .f = calc_y_range)

  # Get text position
  if (pos_label == "topleft") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p[[pos_label]]$x_prop,
        y_prop = props_p[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "topright") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p[[pos_label]]$x_prop,
        y_prop = props_p[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "bottomleft") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p[[pos_label]]$x_prop,
        y_prop = props_p[[pos_label]]$y_prop,
        pos = pos_label))
  }
  if (pos_label == "bottomright") {
    pos_text <- purrr::map2(.x = x_range, .y = y_range,
      .f = ~place_text(.x, .y, x_prop = props_p[[pos_label]]$x_prop,
        y_prop = props_p[[pos_label]]$y_prop,
        pos = pos_label))
  }

  # Create labels with p_val
  label <- purrr::map(trend_tbl$p_val, ~paste0("p = ",
    round(., 3)))

  # Apply internal plot helper function
  p <- purrr::pmap(.l = list(time = trend_tbl$time_train,
    ind = trend_tbl$ind_train, pred = trend_tbl$pred,
    ci_up = trend_tbl$ci_up, ci_low = trend_tbl$ci_low,
    ylab = trend_tbl$ind, pos_text = pos_text,
    label = label), .f = plot_helper)

  names(p) <- trend_tbl$ind

  return(p)
}



# Internal helper functions (no extra script) -----

plot_helper <- function(time, ind, pred, ci_up, ci_low,
  ylab, pos_text, label) {
  poly_x <- c(sort(time, decreasing = FALSE), sort(time,
    decreasing = TRUE))
  poly_y <- c(ci_up[order(time, decreasing = FALSE)],
    ci_low[order(time, decreasing = TRUE)])

  p <- ggplot2::ggplot() + ggplot2::geom_polygon(data = NULL,
    ggplot2::aes(x = poly_x, y = poly_y), fill = "lightblue",
    alpha = 0.5) + ggplot2::geom_line(data = NULL,
    ggplot2::aes(x = time, y = ind)) + ggplot2::geom_point(data = NULL,
    ggplot2::aes(x = time, y = ind)) + ggplot2::geom_line(data = NULL,
    ggplot2::aes(x = time, y = pred), colour = "blue") +
    ggplot2::labs(y = ylab, x = "Time") + ggplot2::annotate(geom = "text",
    x = pos_text$x, y = pos_text$y, label = label,
    hjust = 0) + ggplot2::scale_x_continuous(breaks = pretty(min(time):max(time))) +
    plot_outline()

  return(p)
}

# Get full y-range shown in plot (same as in
# plot_model() )
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
