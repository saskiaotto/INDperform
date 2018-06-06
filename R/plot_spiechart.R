#' Create score-based spiechart
#'
#' \code{plot_spiechart} generates for each indicator in the scoring
#' tibble a ggplot2-based spiechart to visualize the scores of each
#' criterion.
#'
#' @param summary_tbl The output tibble from the
#'  \code{\link{summary_sc}} function.
#' @param col_press_type Colors for the spiechart slices representing
#'  criteria 9 (sensitivity; opaque) and 9 (robustness; transparent).
#'
#'  The colors distinguish the different pressure types. The default
#'  is set to the RColourBrewer palette "Set1".
#' @param col_crit8_11 Colors for the spiechart slices representing
#'  criteria 8 (trend) and 11 (management application). The default is
#'  set to cyan1 and yellow2.
#' @param lab_size Size for the labels naming the significant pressures. The default is 6.
#' @param title_size Size for the title naming the indicator. The default is 8.
#'
#' @details
#' The overall performance of each tested IND is illustrated using a spiechart,
#' which has been shown to be a well-suited graphical tool for
#' displaying multivariate data in comparative indicator evaluations
#' (Stafoggia \emph{et al.}, 2011). A spiechart superimposes a normal pie chart with
#' a modified polar area chart to permit the comparison of two sets of related
#' data, e.g. the maximum achievable scores and each IND’s realized scores. In this
#' function, the slice width is kept constant, while the length of the slices
#' represents the percentage of scores achieved, with the boundary line (i.e. the
#' inner grey circle) indicating the full 100%.
#'
#' The two unlabeled slices at the top represent the trend (right) and management
#' (left) criteria. The sensitivity and robustness scores are shown individually for
#' each pressure where a significant relationship was found. These are the labeled
#' slices grouped by their pressure type represented by dotted devision lines and the
#' segmented outer grey circle as well as pressure type-specific colors (sensitivity
#' scores are displayed in opaque color, robustness scores in transparent color).
#'
#' The plot slices adjust to the number of criteria used for the scoring function
#' (that are present in crit_scores_tmpl).
#'
#' @return The function returns a list of \code{\link[ggplot2]{ggplot}} objects.
#'
#' @references
#' Stafoggia, M., Lallo, A., Fusco, D., Barone, A.P., D`Ovidio, M., Sorge, C.,
#' Perucci, C.A. (2011) Spie charts, target plots, and radar plots for
#' displaying comparative outcomes of health care. \emph{Journal of Clinical
#' Epidemiology} 64, 770-778.
#'
#' @family score-based IND performance functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in this package
#' scores_tbl <- scoring(trend_tbl = model_trend_ex,
#'   mod_tbl = all_results_ex, press_type = press_type_ex)
#' summary_tbl <- summary_sc(scores_tbl)
#' p <- plot_spiechart(summary_tbl)
#' p$TZA
#'
#' \dontrun{
#'  # Show all spiecharts together
#'  gridExtra::grid.arrange(grobs = p)
#'
#' # To modify the plot
#' p <- plot_spiechart(summary_tbl, col_crit8_11 = c("black",
#'   "thistle4"), col_press_type = RColorBrewer::brewer.pal(3,
#'   name = "Accent"), lab_size = 4, title_size = 4)
#' gridExtra::grid.arrange(grobs = p)
#'
#' # Remove pressure-independent criteria for the plot (e.g.
#' # management) (easiest in the score tibble)
#' scores_tbl$C11 <- NULL
#' summary_tbl <- summary_sc(scores_tbl)
#' p <- plot_spiechart(summary_tbl)
#' gridExtra::grid.arrange(grobs = p)
#'
#' # Exclude additionally one pressure-specific criterion
#'  # (e.g. sensitivity C9) (easiest by removing columns
#'  # in the first list of the summary)
#' summary_tbl[[1]] <- summary_tbl[[1]][ ,
#'   ! names(summary_tbl[[1]]) %in% c("C9", "C9_in%")]
#' p <- plot_spiechart(summary_tbl)
#' gridExtra::grid.arrange(grobs = p)
#' }
plot_spiechart <- function(summary_tbl, col_press_type = NULL,
  col_crit8_11 = NULL, lab_size = 6, title_size = 8) {

  # Data input validation -----------------------
  if (missing(summary_tbl)) {
    stop("Argument summary_tbl is missing.")
  }
  # Check input list
  if (class(summary_tbl) != "list") {
    stop("summary_tbl has to be a list (output of summary_sc() function)!")
  }

  # Data re-organization ----------------------------

  # Check if crit 8 and 11 were scored (pressure
  # independent categories)
  n_c8_c11 <- c(any(grepl("C8_in%", names(summary_tbl[[1]])) ==
    TRUE), any(grepl("C11_in%", names(summary_tbl[[1]])) ==
    TRUE))

  # Check if crit 9 and 10 were scored (pressure-spec
  # categories)
  n_c9_c10 <- c(any(grepl("C9_in%", names(summary_tbl[[1]])) ==
    TRUE), any(grepl("C10_in%", names(summary_tbl[[1]])) ==
    TRUE))

  # Order pressure-specific summary by ind,
  # press_type and then press
  summary_tbl[[2]] <- summary_tbl[[2]] %>% dplyr::arrange_("ind",
    "press_type", "press")

  # Split summary_tbl by indicators
  split_input <- purrr::map(summary_tbl[[1]]$ind,
    ~summary_tbl[[2]][summary_tbl[[2]]$ind == .,
      ])

  # Get all press_types
  press_type <- unique(summary_tbl[[2]]$press_type)[!is.na(unique(summary_tbl[[2]]$press_type))] %>%
    sort()

  # Get categories and subcategories for scaling
  cat <- data.frame(press_type = press_type, n = 0,
    stringsAsFactors = FALSE)

  # Calculate n as max possible count of pressures in
  # one category
  for (i in seq_along(press_type)) {
    cat$n[i] <- max(purrr::map_dbl(split_input,
      ~length(.x$press_type[.x$press_type ==
        press_type[i]])))
  }
  # Get maximum number of possible pressures
  total_n <- sum(cat$n, na.rm = TRUE) * sum(n_c9_c10)

  # Add n max for each press_type
  split_input <- suppressMessages(purrr::map(split_input,
    ~dplyr::left_join(., cat, by = "press_type")))

  # Add count of sign. pressures/press_type
  for (i in 1:length(split_input)) {
    if (!is.na(split_input[[i]]$press_type[1])) {
      split_input[[i]]$n_press <- purrr::map_dbl(split_input[[i]]$press_type,
        ~length(which(split_input[[i]]$press_typ ==
          .)))
    }
  }

  # Plot-specific information ----------------------

  # Get colours for each pressure_type
  if (is.null(col_press_type))
    suppressWarnings(col_press_type <- RColorBrewer::brewer.pal(length(press_type),
      name = "Set1"))
  if (is.null(col_crit8_11)) {
    col_crit8_11 <- c("cyan1", "yellow2")
  }
  col_press <- data.frame(press_type = press_type,
    col = col_press_type[1:length(press_type)],
    stringsAsFactors = FALSE)
  split_input <- suppressMessages(purrr::map(split_input,
    ~dplyr::left_join(., col_press)))

  # Get scales for each slice
  parting <- 360/(total_n + sum(n_c8_c11))
  x <- 360/parting

  # Assign borders for each slice
  scale <- seq(from = 0, to = 360 - parting, length.out = x) +
    parting/2
  names(scale) <- 1:length(scale)

  # Background for the plot
  ground <- data.frame(x = c(0, 0, 360, 360), y1 = c(0,
    110, 110, 0))

  # Create borders for outer ring1
  x_ring1 <- c(0, 360, 360, 0)

  # Plot -----------------------------------------

  theme_infog <- ggplot2::theme_classic() + ggplot2::theme(axis.line = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  p <- purrr::map2(split_input, summary_tbl[[1]]$ind,
    ~plot_spie(.x, scale, parting, cat, summary_tbl[[1]],
      ground, n_c8_c11, n_c9_c10, col_crit8_11,
      x_ring1, theme_infog, ind = .y, lab_size = lab_size,
      title_size = title_size))

  # Give sublists names
  names(p) <- summary_tbl$overview$ind

  ### END OF FUNCTION ###
  return(p)
}
