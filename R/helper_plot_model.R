#' Helper functions for plot_model
#'
#' These are the actual plotting functions for each of the 4 plots shown
#' in the wrapper function \code{\link{plot_model}}. \code{plot_thresh}
#' works currently only for threshold-GAMs but not for threshold_GAMMs.
#'
#' @keywords internal
#' @export
#' @seealso \code{\link{plot_model}}
#'
#' @examples
#' # Example for dealing with nested list-columns using the
#' # Baltic Sea demo data
#' thresh_sublist1 <- all_results_ex$thresh_models[[69]]
#' thresh_sublist2 <- all_results_ex$thresh_models[[70]]
#' thresh_sublist <- list(thresh_sublist1, thresh_sublist2) %>%
#'   purrr::flatten(.)
#' plot_thresh(thresh_sublist, choose_thresh_gam = NULL)
#' plot_thresh(thresh_sublist, choose_thresh_gam = 1)
#' plot_thresh(thresh_sublist, choose_thresh_gam = 2)
plot_thresh <- function(thresh_sublist, choose_thresh_gam) {

  # Which thresh_gam shall be plotted?
  select <- purrr::map_dbl(thresh_sublist, ~suppressWarnings(as.numeric(try(.$mgcv,
    silent = TRUE))))
  select <- which(select == min(select, na.rm = TRUE))
  if (!is.null(choose_thresh_gam)) {
    select <- choose_thresh_gam
  }
  # Create input
  model <- thresh_sublist[[select]]
  # Get t_val
  t_val <- model$mr
  # Get model data
  below_t_val <- model$model[model$model[, 3] ==
    1, ]
  above_t_val <- model$model[model$model[, 3] ==
    0, ]

  # Calculate predicted values for press sequence
  # below/above threshold value
  add_pred <- function(mod, t_val, which) {
    press_seq <- seq(from = min(mod$model[, 2]),
      to = max(mod$model[, 2]), length.out = length(mod$model[,
        2]))
    if (which == "below") {
      new_dat <- data.frame(press = press_seq,
        t_var = rep((t_val - (t_val * 0.1)),
          times = length(mod$model[, 2])))
    } else {
      new_dat <- data.frame(press = press_seq,
        t_var = rep((t_val + (t_val * 0.1)),
          times = length(mod$model[, 2])))
    }
    names(new_dat) <- names(mod$original_data)[2:3]
    temp <- mgcv::predict.gam(mod, newdata = new_dat,
      se.fit = TRUE)
    out <- data.frame(press_seq = press_seq, pred = temp$fit,
      ci_up = temp$fit + 1.96 * temp$se.fit,
      ci_low = temp$fit - 1.96 * temp$se.fit)
    return(out)
  }
  pred_below <- add_pred(mod = model, t_val = t_val,
    which = "below")
  pred_above <- add_pred(mod = model, t_val = t_val,
    which = "above")

  # Find limits
  y_lim <- range(c(below_t_val[, 1], above_t_val[,
    1], pred_above$ci_up, pred_above$ci_low, pred_below$ci_up,
    pred_below$ci_low), na.rm = TRUE)

  # Plot model
  plot_x <- function(model, data, pred, title, col,
    y_lim) {
    x_lab <- names(data)[2]
    y_lab <- names(data)[1]
    names(data)[1:2] <- c("ind", "press")
    # Add CI
    poly_x <- c(sort(pred$press_seq, decreasing = FALSE),
      sort(pred$press_seq, decreasing = TRUE))
    poly_y <- c(pred$ci_up[order(pred$press_seq,
      decreasing = FALSE)], pred$ci_low[order(pred$press_seq,
      decreasing = TRUE)])
    p <- ggplot2::ggplot() + ggplot2::geom_polygon(data = NULL,
      ggplot2::aes(x = poly_x, y = poly_y),
      fill = col, alpha = 0.2) + ggplot2::geom_point(data = NULL,
      ggplot2::aes(x = data$press, y = data$ind),
      colour = col, size = 1) + ggplot2::geom_line(ggplot2::aes(x = pred$press_seq,
      y = pred$pred), colour = col) + ggplot2::labs(x = x_lab,
      y = y_lab) + ggplot2::expand_limits(x = range(model$model[,
      2]), y = y_lim) + ggplot2::ggtitle(title) +
      plot_outline()
    return(p)
  }

  title1 <- bquote(paste(.(all.vars(model$formula)[3]) <=
    .(round(t_val, digits = 2)), " (p = ", .(round(summary(model)$s.pv[1],
    3)), ")"))
  title2 <- paste0(all.vars(model$formula)[3], " > ",
    round(t_val, digits = 2), " (p = ", round(summary(model)$s.pv[2],
      3), ")")
  p1 <- plot_x(model, below_t_val, pred_below, title = title1,
    "black", y_lim)
  p2 <- plot_x(model, above_t_val, pred_above, title = title2,
    "red", y_lim)

  p <- cowplot::ggdraw() + cowplot::draw_plot(p1,
    x = 0, y = 0, width = 0.5, height = 0.92) +
    cowplot::draw_plot(p2, x = 0.5, y = 0, width = 0.5,
      height = 0.92) + cowplot::draw_plot_label(label = "Strongest interaction")

  ### END OF FUNCTION
  return(p)
}


#' @rdname plot_thresh
# Function to plot response curve
plot_response <- function(x, y, x_seq, pred, ci_up,
  ci_low, xlab, ylab, pos_text, label, title = "Response curve S") {
  # x: press, y: ind, x_seq = sequence of x and pred
  # = predicted values based on x_seq for smoother

  poly_x <- c(sort(x_seq, decreasing = FALSE), sort(x_seq,
    decreasing = TRUE))
  poly_y <- c(ci_up[order(x_seq, decreasing = FALSE)],
    ci_low[order(x_seq, decreasing = TRUE)])

  p <- ggplot2::ggplot() + ggplot2::geom_polygon(data = NULL,
    ggplot2::aes(x = poly_x, y = poly_y), fill = "dodgerblue3",
    alpha = 0.2) + ggplot2::geom_line(data = NULL,
    ggplot2::aes(x = x_seq, y = pred), colour = "dodgerblue3",
    size = 1) + ggplot2::geom_point(data = NULL,
    ggplot2::aes(x = x, y = y), shape = 16, size = 1) +
    ggplot2::annotate(geom = "text", x = pos_text$x,
      y = pos_text$y, label = label, hjust = 0) +
    ggplot2::labs(x = xlab, y = ylab) + ggplot2::ggtitle(title) +
    plot_outline()

  return(p)
}

#' @rdname plot_thresh
# function to plot prediction performance of test data
plot_predict <- function(x, y_obs, y_pred, ci_up, ci_low,
  x_train, x_test, zoom, x_range, y_range, xlab,
  ylab, pos_text, label, title = "Predictive performance") {
  # x: time, y: ind, x_train: time of training data,
  # x_test: time of test data
  zoom_no_nas <- function(var, zoom) {
    out <- var[zoom]
    out <- out[!is.na(out)]
    return(out)
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }
  x_zoom <- x[zoom]
  y_pred_zoom <- y_pred[zoom]
  ci_up_zoom <- ci_up[zoom]
  ci_low_zoom <- ci_low[zoom]
  x_train_zoom <- x_train[x_train %in% zoom]
  x_seq_on_axis <- pretty(seq(x_range[1], x_range[2],
    1))
  x_seq_on_axis <- x_seq_on_axis[is.wholenumber(x_seq_on_axis)]

  p <- ggplot2::ggplot() + ggplot2::geom_ribbon(data = NULL,
    ggplot2::aes(x = x_zoom, ymin = ci_low_zoom,
      ymax = ci_up_zoom), fill = "darkseagreen4",
    alpha = 0.2) + ggplot2::geom_line(data = NULL,
    ggplot2::aes(x = x_zoom, y = y_pred_zoom),
    colour = "darkseagreen4", size = 1) + ggplot2::geom_point(data = NULL,
    ggplot2::aes(x = x[x_train_zoom], y = y_obs[x_train_zoom]),
    shape = 16, size = 2) + ggplot2::geom_point(data = NULL,
    ggplot2::aes(x = x[x_test], y = y_obs[x_test]),
    shape = 17, size = 3) + ggplot2::geom_point(data = NULL,
    ggplot2::aes(x = x[x_test], y = y_pred[x_test]),
    shape = 17, size = 3, colour = "darkseagreen4") +
    ggplot2::ylim(y_range) + ggplot2::scale_x_continuous(breaks = x_seq_on_axis,
    limits = x_range) + ggplot2::labs(x = xlab,
    y = ylab) + ggplot2::annotate(geom = "text",
    x = pos_text$x, y = pos_text$y, label = label,
    hjust = 0) + ggplot2::ggtitle(title) + plot_outline()

  return(p)
}

#' @rdname plot_thresh
# Function to plot derivatives
plot_deriv <- function(press_seq, deriv1, deriv1_ci_low,
  deriv1_ci_up, zic_start_end, zero_in_conf, xlab,
  ylab, pos_text, label) {
  p <- ggplot2::ggplot() + ggplot2::geom_line(data = NULL,
    ggplot2::aes(x = press_seq, y = deriv1), lty = 1,
    color = "red") + ggplot2::geom_line(data = NULL,
    ggplot2::aes(x = press_seq, y = deriv1_ci_up),
    lty = 2, color = "red") + ggplot2::geom_line(data = NULL,
    ggplot2::aes(x = press_seq, y = deriv1_ci_low),
    lty = 2, color = "red") + ggplot2::geom_hline(yintercept = 0,
    linetype = "dashed", color = "black") + ggplot2::labs(x = xlab,
    y = paste0("S' (", ylab, ")")) + ggplot2::geom_point(data = NULL,
    ggplot2::aes(x = press_seq, y = deriv1, col = as.factor(zero_in_conf),
      shape = as.factor(zic_start_end))) + ggplot2::annotate(geom = "text",
    x = pos_text$x, y = pos_text$y, label = label,
    hjust = 0) + ggplot2::scale_colour_manual(values = c(2,
    1)) + ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("1st Derivative S'") + plot_outline()

  return(p)
}


#' @rdname plot_thresh
# Cowplot that will combine the different ggplots
plot_all_mod <- function(p1, p2, p3, p4, title) {
  cowplot::ggdraw() + cowplot::draw_plot(p1, x = 0,
    y = 0.5, width = 0.5, height = 0.45) + cowplot::draw_plot(p2,
    x = 0.5, y = 0.5, width = 0.5, height = 0.45) +
    cowplot::draw_plot(p3, x = 0, y = 0, width = 0.5,
      height = 0.45) + cowplot::draw_plot(p4,
    x = 0.5, y = 0, width = 0.5, height = 0.45) +
    cowplot::draw_plot_label(label = title, hjust = -0.5,
      vjust = 2)
}
