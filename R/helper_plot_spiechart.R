#' Helper function for plot spiechart
#'
#' This is the actual plotting function used by the wrapper
#' function \code{\link{plot_spiechart}} for each indicator.
#'
#' @keywords internal
#' @seealso \code{\link{plot_spiechart}}
#' @export
plot_spie <- function(split_input, scale, parting,
  cat, summary_tbl, ground, n_c8_c11, n_c9_c10, col_crit8_11,
  x_ring1, theme_infog, edge = "grey30", ind, lab_size,
	 title_size) {


	# Data reorganization (now IND-specific) ---------------

  # Borders for press_type for outer ring2
  temp <- vector(mode = "integer", length = nrow(cat))
  for (i in 1:length(temp)) {
    temp[i] <- sum(cat$n[1:i], na.rm = TRUE) *
      sum(n_c9_c10) + any(n_c8_c11)
  }
  if (all(n_c8_c11)) {
    border <- c(parting, scale[temp] + parting/2,
      360)
    x_ring2 <- c(parting, 360 - parting, 360 -
      parting, parting)
  } else {
    if (any(n_c8_c11)) {
      border <- c(parting/2, scale[temp], 360 -
        parting/2)
      x_ring2 <- c(parting/2, 360 - parting/2,
        360 - parting/2, parting/2)
    } else {
      border <- c(0, scale[temp] + parting/2,
        360)
      x_ring2 <- c(0, 360, 360, 0)
    }
  }

  # Get (IND-specific) plotting information ----------------

  # Which values from scale are needed
  take <- suppressMessages(split_input %>%
  		dplyr::group_by_(.dots = "press_type") %>%
    dplyr::summarise_(.dots = stats::setNames(list(~unique(n_press)),
      "n_press")) %>% dplyr::left_join(cat, .))
  # DEN TEIL VERSTEH ICH HIER NICHT:
  take$n_press[is.na(take$n_press)] <- 0
  x <- vector(mode = "list", length = nrow(take))
  for (i in 1:nrow(take)) {
    x[[i]] <- rep(FALSE, times = take$n[i] - take$n_press[i])
    x[[i]] <- c(x[[i]], rep(TRUE, times = take$n_press[i] *
      2))
    x[[i]] <- c(x[[i]], rep(FALSE, times = take$n[i] -
      take$n_press[i]))
  }
  #--------???

  take <- unlist(x)
  if (!all(n_c9_c10)) {
    take <- take[seq(from = 1, to = length(take),
      by = 2)]
  }

  # Get x values for pressures
  if (any(take)) {
    x_bar_press <- scale[which(take == TRUE) +
      any(n_c8_c11)]
  } else {
    x_bar_press <- c(scale[2:3])
  }

  # Changes in x_bar_press if not all c8/c11
  if (!all(n_c8_c11)) {
    if (any(n_c8_c11)) {
      x_bar_press <- x_bar_press - parting/2
    }
  }
  # Get y values for pressures
  y_bar_press <- NULL
  for (i in 1:nrow(split_input)) {
    # Choose only what is present indicated by n_c9_c10
    y_bar_press <- c(y_bar_press, unlist(split_input[i,
      c("C9_in%", "C10_in%")[n_c9_c10]]))
  }

  # Get x values for labels
  if (all(n_c9_c10)) {
    x_lab <- x_bar_press[seq(from = 1, to = length(x_bar_press),
      by = 2)]
  } else {
    x_lab <- x_bar_press
  }

  # Get labels for the pressure slices
  lab <- c(split_input$press)

  # Assign colours
  if (all(n_c9_c10)) {
    # Colours for the slices if c9 and 10 are present
    col_slice <- c(rep(split_input$col, each = 2))
    alpha <- c(rep(c(1, 0.5), times = nrow(split_input)))
  } else {
    # Colours for the slices if only c9 or c10 is
    # present
    col_slice <- split_input$col
    alpha <- rep(c(1, 0.5)[n_c9_c10], times = nrow(split_input))
  }

  # col alpha and lab if nrow(split_input) == 0
  if (nrow(split_input) == 0) {
    col_slice <- NA
    alpha <- NA
    lab <- NA
  }
  # Get x and y for pressure-unspecific values
  if (!any(n_c8_c11)) {
    # if no trend/ management is plotted -->
    # parting_ind = 0
    parting_ind <- 0
    x_bar_ind <- x_bar_press[1]
    y_bar_ind <- 0
    edge <- NA
    col_crit8_11 <- NA
  }
  if (any(n_c8_c11)) {
    x_bar_ind <- scale[c(1, length(scale))]
    y_bar_ind <- c(unlist(summary_tbl[summary_tbl$ind == ind,
      c("C8_in%", "C11_in%")[n_c8_c11]]))
    parting_ind <- parting
    # Double value for remaining crit if one is
    # excluded
    if (!all(n_c8_c11)) {
      x_bar_ind <- x_bar_ind + c(-parting/4,
        parting/4)
      y_bar_ind <- rep(y_bar_ind, times = 2)
      edge <- NA
      parting_ind <- parting/2
      # Choose the colour for the remaining crit
      if (is.na(col_crit8_11[n_c8_c11])) {
        # If only one col_crit is provided (since you dont
        # provide two col_crits if you exclude c8, but in
        # that case col_crit8_11[n_c8_c11] is NA!)
        col_crit8_11 <- rep(col_crit8_11, times = 2)
      } else {
        # If the default is used
        col_crit8_11 <- rep(col_crit8_11[n_c8_c11],
          times = 2)
      }
    }
  }

  # Actual plot -----------------------

  p <- ggplot2::ggplot() + theme_infog +

  	# Create a white background
		  ggplot2::geom_polygon(data = NULL, ggplot2::aes_(x = ground$x,
		    y = ground$y1), fill = "white") +
  	# Create the outer ring
		  ggplot2::geom_polygon(data = NULL, ggplot2::aes_(x = x_ring1,
		    y = c(120, 120, 130, 130)), fill = "grey30") +
		  ggplot2::geom_polygon(data = NULL, ggplot2::aes_(x = x_ring2,
		      y = c(120, 120, 130, 130)), fill = "grey60") +
		  # Make the barplot to a spiechart
		  ggplot2::coord_polar() +
  	 # Create the white borders between the categories
		  ggplot2::geom_segment(ggplot2::aes_(x = border,
		    xend = border, y = 120, yend = 140), colour = "white",
		    data = NULL, cex = 2) +
  	 ggplot2::geom_segment(ggplot2::aes_(x = border,
		    xend = border, y = 0, yend = 100), colour = "grey60",
		    data = NULL, linetype = 2) +

  		# Plot the pressure specific data
		  ggplot2::geom_bar(data = NULL, ggplot2::aes_(x = x_bar_press,
		    y = y_bar_press), stat = "identity", width = parting,
		    fill = col_slice, alpha = alpha, col = "grey30", na.rm=TRUE) +
		  # Plot the pressure-unspecific data
		  ggplot2::geom_bar(data = NULL, ggplot2::aes_(x = x_bar_ind,
		    y = y_bar_ind), stat = "identity", width = parting_ind,
		    fill = col_crit8_11, col = edge, na.rm=TRUE) +
		  # Add 100% line
  	 ggplot2::geom_abline(intercept = 100, slope = 0,
		    linetype = 1, col = "grey60") +
  	 # Add labels for sig pressures
		  ggplot2::geom_text(ggplot2::aes_(x = x_lab, y = 100,
		    label = lab), size = lab_size, na.rm=TRUE) +
  	 # Add title
		  ggplot2::geom_text(ggplot2::aes_(x = x_ring1[1],
		    y = 150, label = ind), size = title_size, na.rm=TRUE)

  return(p)
}
