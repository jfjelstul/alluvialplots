# This function uses code adapted from the alluvial package by
# Michal Bojanowski and Robin Edwards under the MIT license

make_alluvial_plot <- function(
  ...,
  frequency,
  fill,
  palette,
  alpha = 0.5,
  width_gaps = 0.2,
  spline_knots = 0.25,
  width_axes = 2,
  title = NULL,
  axis_labels,
  space_before_labels = 0.05,
  space_under_title = 16,
  space_around_plot_horizontal = 40,
  space_around_plot_vertical = 20,
  size_title = 14,
  size_axis_labels = 10,
  size_category_labels = 10,
  color_axes = "black",
  size_legend_keys = 22,
  space_before_legend = 16,
  space_between_legend_keys = 8,
  size_legend_labels = 10,
  space_before_legend_labels = 8,
  guide = TRUE,
  return_data = FALSE
) {

  # Make category data
  category_data <- data.frame(...)
  names(category_data) <- stringr::str_c("variable_", 1:ncol(category_data))

  # Create factors
  for(i in 1:ncol(category_data)) {
    category_data[,i] <- forcats::fct_rev(category_data[,i])
  }

  # Counts
  count_axes <- ncol(category_data)
  count_alluvia <- nrow(category_data)
  count_flows <- count_axes - 1

  # Make frequency data
  frequency_data <- category_data
  frequency_data$frequency <- frequency
  frequency_data$proportion <- frequency_data$frequency / sum(frequency_data$frequency)

  # Function to calculate gaps
  calculate_gaps <- function(i, d, f, w = width_gaps) {
    a <- c(i, (1:ncol(d))[-i])
    o <- do.call(order, d[a])
    x <- c(0, cumsum(f[o])) * (1 - w)
    x <- cbind(x[-length(x)], x[-1])
    gap <- cumsum(c(0L, diff(as.numeric(d[o, i])) != 0))
    mx <- max(gap)
    if (mx == 0) {
      mx <- 1
    }
    gap <- gap / mx * w
    (x + gap)[order(o),]
  }

  gaps <- lapply(seq_along(category_data), calculate_gaps, d = category_data, f = frequency_data$proportion)

  # Create an empty list to store polygon data
  polygon_data <- list()

  # Create a counter
  counter <- 1

  # Loop through the alluvia
  for (i in 1:count_alluvia) {

    # Loop through the flows
    for (j in 1:count_flows) {

      # Increment the counter
      counter <- counter + 1

      # Calculate the spline coordinates
      plot.new()
      coordinates <- xspline(
        x = c(
          j, j, j + spline_knots, j + 1 - spline_knots,
          j + 1, j + 1, j + 1 - spline_knots, j + spline_knots, j
        ),
        y = c(
          gaps[[j]][i, c(1, 2, 2)],
          rev(gaps[[j + 1]][i, c(1, 1, 2, 2)]),
          gaps[[j]][i, c(1, 1)]
        ),
        shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
        open = FALSE,
        draw = FALSE
      )

      # Add spline coordinates to polygon data
      polygon_data[[counter]] <- dplyr::tibble(
        x = coordinates$x,
        y = coordinates$y,
        alluvia = i,
        flow = j,
        fill = fill[i]
      )
    }
  }

  # Combine polygon data
  polygon_data <- dplyr::bind_rows(polygon_data)

  # Make a polygon ID
  polygon_data$polygon_id <- stringr::str_c(polygon_data$alluvia, "-", polygon_data$flow)

  # Create empty lists to store axis and label data
  axis_data <- list()
  label_data <- list()

  # Create a new counter
  counter <- 1

  # Loop through axes
  for (i in 1:count_axes) {

    axes <- lapply(split(gaps[[i]], category_data[, i], drop = TRUE), range)

    # Loop through groups (per axis) to make lines
    for (j in seq_along(axes)) {

      # Axis
      counter <- counter + 1

      # Make axis data
      x <- c(i, i)
      y <- c(axes[[j]][1], axes[[j]][2])
      axis_data[[counter]] <- dplyr::tibble(
        axis = i,
        stratum = j,
        x = x,
        y = y
      )

      # Make label data
      x <- ifelse(i == count_axes, i - space_before_labels, i + space_before_labels)
      y <- (axes[[j]][1] + axes[[j]][2]) / 2
      label_data[[counter]] <- dplyr::tibble(
        axis = i,
        stratum = j,
        x = x,
        y = y,
        label = names(axes[j]),
        hjust = ifelse(i == count_axes, 1, 0)
      )
    }
  }

  # Combine axis data
  axis_data <- dplyr::bind_rows(axis_data)
  label_data <- dplyr::bind_rows(label_data)

  # Make an axis ID
  axis_data$line_id <- stringr::str_c(axis_data$axis, "-", axis_data$stratum)

  # Return a plot or data
  if(return_data) {

    data <- list(
      polygon_data = polygon_data,
      axis_data = axis_data,
      label_data = label_data
    )

    return(data)

  } else {

    plot <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = polygon_data, ggplot2::aes(x = x, y = y, group = polygon_id, fill = fill), alpha = alpha, color = NA) +
      ggplot2::geom_line(data = axis_data, ggplot2::aes(x = x, y = y, group = line_id), color = color_axes, size = width_axes) +
      ggplot2::geom_text(data = label_data, ggplot2::aes(x = x, y = y, label = label, hjust = hjust), size = size_category_labels * (5/14)) +
      ggplot2::scale_x_continuous(breaks = 1:count_axes, labels = axis_labels, expand = c(0, 0.05)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "right",
        legend.key = ggplot2::element_blank(),
        legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = space_before_legend),
        legend.key.size = ggplot2::unit(size_legend_keys, "pt"),
        legend.text = ggplot2::element_text(size = size_legend_labels),
        legend.spacing.x = ggplot2::unit(space_before_legend_labels, "pt"),
        legend.spacing.y = ggplot2::unit(space_between_legend_keys, "pt"),
        plot.title = ggplot2::element_text(size = size_title, hjust = 0.5, margin = ggplot2::margin(t = 0, b = space_under_title, l = 0, r = 0)),
        plot.margin = ggplot2::margin(t = space_around_plot_vertical, b = space_around_plot_vertical, l = space_around_plot_horizontal, r = space_around_plot_horizontal),
        axis.text.x = ggplot2::element_text(size = 10, margin = ggplot2::margin(t = size_axis_labels))
      )

    if(guide == TRUE) {
      plot <- plot +
        ggplot2::scale_fill_manual(values = palette, name = NULL) +
        ggplot2::guides(fill = ggplot2::guide_legend(byrow = TRUE))
    } else {
      plot <- plot +
        ggplot2::scale_fill_manual(values = palette, name = NULL, guide = NULL)
    }

    return(plot)
  }
}

# plot_data <- as.data.frame(UCBAdmissions)
# plot_data <- dplyr::select(plot_data, Dept, Gender, Admit, Freq)
#
# plot <- make_alluvial_plot(
#   plot_data$Dept, plot_data$Gender, plot_data$Admit,
#   frequency = plot_data$Freq,
#   fill = plot_data$Gender,
#   palette = c("gray70", "gray30"),
#   alpha = 0.3,
#   axis_labels = c("Department", "Gender", "Status"),
#   width_axes = 1,
#   width_gaps = 0.2,
#   return_data = FALSE,
#   size_category_labels = 10,
#   title = "Example plot",
#   guide = FALSE
# )
# plot
