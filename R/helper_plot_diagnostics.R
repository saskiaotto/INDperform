#' Helper functions for plot_model
#'
#' These are the actual plotting functions for the 6 plots shown in the wrapper
#' function \code{\link{plot_diagnostics}}.
#'
#' @keywords internal
#' @seealso \code{\link{plot_diagnostics}}
#' @export
plot_outline <- function() {
  # Create a general theme for the plots
  p <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13,
    colour = "black"), axis.text.y = ggplot2::element_text(size = 13,
    colour = "black"), axis.title = ggplot2::element_text(size = 13),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.grid = ggplot2::element_blank(), panel.border = ggplot2::element_rect(fill = NA,
      colour = "black"), panel.background = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank())
  return(p)
}

#' @rdname plot_outline
# Function to plot the Cooks distance
plot_cook <- function(values) {
  if (is.null(values)) {
    values <- 0
  }
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(y = values)) +
    ggplot2::geom_bar(data = NULL, ggplot2::aes(x = 1:length(values)),
      stat = "identity", width = 0.1) + ggplot2::labs(x = "",
    y = "Cook`s distance") + ggplot2::geom_hline(yintercept = 1,
    colour = "red") + ggplot2::geom_abline(intercept = 0,
    slope = 0) + plot_outline()
  return(p)
}

#' @rdname plot_outline
# Autocorrelation function plot
plot_acf <- function(x_var, y_var) {
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = x_var,
    y_var)) + ggplot2::geom_bar(stat = "identity",
    width = 0.1) + ggplot2::geom_abline(intercept = 0,
    slope = 0) + ggplot2::geom_abline(intercept = 0.4,
    slope = 0, lty = 2, colour = "red") + ggplot2::geom_abline(intercept = -0.4,
    slope = 0, lty = 2, colour = "red") + ggplot2::labs(x = "Lag",
    y = "ACF") + ggplot2::lims(y = c(-1, 1)) +
    ggplot2::scale_x_continuous(breaks = seq(0,
      max(x_var), 2), labels = seq(0, max(x_var),
      2)) + plot_outline()
  return(p)
}

#' @rdname plot_outline
# Partial acf plot
plot_pacf <- function(x_var, y_var, acf_lag) {
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = x_var,
    y_var)) + ggplot2::geom_bar(stat = "identity",
    width = 0.1) + ggplot2::geom_abline(intercept = 0,
    slope = 0) + ggplot2::geom_abline(intercept = 0.4,
    slope = 0, lty = 2, colour = "red") + ggplot2::geom_abline(intercept = -0.4,
    slope = 0, lty = 2, colour = "red") + ggplot2::labs(x = "Lag",
    y = "Partial ACF") + ggplot2::scale_x_continuous(breaks = seq(0,
    max(x_var), 2), labels = seq(0, max(x_var),
    2)) + plot_outline()
  return(p)
}

#' @rdname plot_outline
# Plot residuals vs fitted values
plot_resid <- function(model_fitted, model_resid) {
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = model_fitted,
    y = model_resid)) + ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 0) +
    ggplot2::labs(x = "Fitted values", y = "Residuals") +
    plot_outline()
  return(p)
}

#' @rdname plot_outline
# Quantile-quantile plot
plot_qq <- function(model_resid, theo_quan) {
  if (is.null(model_resid)) {
    model_resid <- 0
  }
  if (is.null(theo_quan)) {
    theo_quan <- 0
  }
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = theo_quan,
    y = sort(model_resid, na.last = TRUE))) + ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::labs(x = "Theoretical Quantiles",
      y = "Sample Quantiles") + plot_outline()
  return(p)
}

#' @rdname plot_outline
# GCVV plot for threshold models
plot_gcvv <- function(x_var, y_var, lab, best_t_val) {
  if (!is.null(x_var)) {
    p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = x_var,
      y = y_var)) + ggplot2::geom_line() + ggplot2::geom_vline(xintercept = best_t_val,
      colour = "red") + ggplot2::labs(x = all.vars(lab$formula)[3],
      y = "GCVV") + plot_outline()
  } else {
    p <- plot_empty()
  }
  return(p)
}

#' @rdname plot_outline
# Cowplot that will combine the different ggplots
plot_all_diag <- function(p1, p2, p3, p4, p5, p6, title) {
  cowplot::ggdraw() + cowplot::draw_plot(p1, x = 0,
    y = 0.5, width = 0.3, height = 0.4) + cowplot::draw_plot(p2,
    x = 0.3, y = 0.5, width = 0.3, height = 0.4) +
    cowplot::draw_plot(p3, x = 0.6, y = 0.5, width = 0.3,
      height = 0.4) + cowplot::draw_plot(p4,
    x = 0, y = 0.1, width = 0.3, height = 0.4) +
    cowplot::draw_plot(p5, x = 0.3, y = 0.1, width = 0.3,
      height = 0.4) + cowplot::draw_plot(p6,
    x = 0.6, y = 0.1, width = 0.3, height = 0.4) +
    cowplot::draw_plot_label(label = title, hjust = -0.25,
      vjust = 4)
}

#' @rdname plot_outline
# Function to plot an empty plot --> also used in
# plot_model()
plot_empty <- function() {
  p <- ggplot2::ggplot() + plot_outline() + ggplot2::labs(x = "",
    y = "") + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank()) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank())
  return(p)
}
