# DataExplorerPro - Visualization Engine
# AI-Powered Data Exploration Studio

# Internal helper to return a quiet placeholder plot with a message
empty_plot_message <- function(msg) {
  plotly::plot_ly(
    x = 0, y = 0, type = "scatter", mode = "markers",
    marker = list(opacity = 0), showlegend = FALSE, hoverinfo = "skip"
  ) %>% plotly::layout(
    annotations = list(list(
      text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
      showarrow = FALSE
    ))
  )
}

#' Create Scatter Plot
#'
#' Generates an interactive scatter plot using Plotly.
#'
#' @param data A data frame
#' @param x_var X-axis variable
#' @param y_var Y-axis variable
#' @param color_var Color grouping variable (optional)
#' @param size_var Size variable (optional)
#' @param trendline Add trendline (default: FALSE)
#' @param title Plot title
#' @return Plotly object
#'
#' @export
create_scatter_plot <- function(data, x_var, y_var, color_var = NULL, 
                                size_var = NULL, trendline = FALSE, 
                                title = NULL) {
  
  p <- plotly::plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                       type = "scatter", mode = "markers")
  
  if (!is.null(color_var) && color_var != "") {
    p <- p %>% plotly::add_markers(
      color = ~get(color_var),
      marker = list(size = if (!is.null(size_var) && size_var != "") {
        ~get(size_var) * 2
      } else 10)
    )
  } else {
    p <- p %>% plotly::add_markers(marker = list(
      size = if(!is.null(size_var) && size_var != "") {
        ~get(size_var) * 2
      } else 10,
      color = "#667eea",
      opacity = 0.7
    ))
  }
  
  if (trendline) {
    p <- p %>% plotly::add_lines(
      y = ~fitted(lm(get(y_var) ~ get(x_var))),
      line = list(color = "#764ba2", dash = "dash")
    )
  }
  
  p %>% plotly::layout(
    title = title %||% paste("Scatter Plot:", x_var, "vs", y_var),
    xaxis = list(title = x_var),
    yaxis = list(title = y_var),
    hovermode = "closest"
  )
}

#' Create Line Chart
#'
#' Generates an interactive line chart using Plotly.
#'
#' @export
create_line_chart <- function(data, x_var, y_var, color_var = NULL, 
                             facet_var = NULL, title = NULL) {
  
  p <- plotly::plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                       type = "scatter", mode = "lines")
  
  if (!is.null(color_var) && color_var != "") {
    p <- p %>% plotly::add_lines(color = ~get(color_var))
  } else {
    p <- p %>% plotly::add_lines(line = list(color = "#667eea", width = 2))
  }
  
  p %>% plotly::layout(
    title = title %||% paste("Line Chart:", y_var, "over", x_var),
    xaxis = list(title = x_var),
    yaxis = list(title = y_var)
  )
}

#' Create Bar Chart
#'
#' Generates an interactive bar chart using Plotly.
#'
#' @export
create_bar_chart <- function(data, x_var, y_var, color_var = NULL, 
                            facet_var = NULL, title = NULL) {
  group_vars <- if (!is.null(color_var) && color_var != "") {
    c(x_var, color_var)
  } else {
    x_var
  }

  if (!is.null(y_var) && y_var != "" && is.numeric(data[[y_var]])) {
    agg_data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarize(value = mean(.data[[y_var]], na.rm = TRUE), .groups = "drop")
    y_title <- paste("Mean", y_var)
  } else {
    agg_data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarize(value = dplyr::n(), .groups = "drop")
    y_title <- "Count"
  }

  if (!is.null(color_var) && color_var != "") {
    p <- plotly::plot_ly(
      agg_data,
      x = ~get(x_var),
      y = ~value,
      color = ~get(color_var),
      type = "bar"
    )
  } else {
    p <- plotly::plot_ly(
      agg_data,
      x = ~get(x_var),
      y = ~value,
      type = "bar",
      marker = list(color = "#667eea")
    )
  }
  
  p %>% plotly::layout(
    title = title %||% paste("Bar Chart:", y_title, "by", x_var),
    xaxis = list(title = x_var),
    yaxis = list(title = y_title),
    barmode = if (!is.null(color_var) && color_var != "") "group" else NULL
  )
}

#' Create Histogram
#'
#' Generates an interactive histogram using Plotly.
#'
#' @export
create_histogram <- function(data, x_var, color_var = NULL, bins = 30, 
                            title = NULL) {
  
  p <- plotly::plot_ly(
    data,
    x = ~get(x_var),
    type = "histogram",
    nbinsx = bins,
    marker = list(color = "#667eea", opacity = 0.7)
  )
  
  if (!is.null(color_var) && color_var != "") {
    p <- plotly::plot_ly(
      data,
      x = ~get(x_var),
      color = ~get(color_var),
      type = "histogram",
      nbinsx = bins
    ) %>%
      plotly::layout(barmode = "overlay")
  }
  
  p %>% plotly::layout(
    title = title %||% paste("Histogram of", x_var),
    xaxis = list(title = x_var),
    yaxis = list(title = "Count")
  )
}

#' Create Box Plot
#'
#' Generates an interactive box plot using Plotly.
#'
#' @export
create_box_plot <- function(data, x_var, y_var, color_var = NULL, 
                           title = NULL) {
  
  p <- plotly::plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = "box")
  
  if (!is.null(color_var) && color_var != "") {
    p <- p %>% plotly::add_trace(boxpoints = "all", jitter = 0.3, pointpos = -1.8)
  }
  
  p %>% plotly::layout(
    title = title %||% paste("Box Plot:", y_var, "by", x_var),
    xaxis = list(title = x_var),
    yaxis = list(title = y_var)
  )
}

#' Create Violin Plot
#'
#' Generates an interactive violin plot using Plotly.
#'
#' @export
create_violin_plot <- function(data, x_var, y_var, color_var = NULL, 
                              title = NULL) {
  
  p <- plotly::plot_ly(
    data,
    x = ~get(x_var),
    y = ~get(y_var),
    type = "violin",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE)
  )
  
  if (!is.null(color_var) && color_var != "") {
    p <- p %>% plotly::add_trace(split = ~get(color_var))
  }
  
  p %>% plotly::layout(
    title = title %||% paste("Violin Plot:", y_var, "by", x_var),
    xaxis = list(title = x_var),
    yaxis = list(title = y_var)
  )
}

#' Create Heatmap
#'
#' Generates an interactive heatmap using Plotly.
#'
#' @export
create_heatmap <- function(data, x_var, y_var, value_var = NULL, 
                          agg_func = "mean", title = NULL) {
  
  # If value_var is missing or non-numeric, fall back to counts
  use_counts <- is.null(value_var) || !is.numeric(data[[value_var]])
  
  if (use_counts) {
    agg_data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(x_var, y_var)))) %>%
      dplyr::summarize(value = dplyr::n(), .groups = "drop")
  } else {
    agg_data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(x_var, y_var)))) %>%
      dplyr::summarize(
        value = match.fun(agg_func)(.data[[value_var]], na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  plotly::plot_ly(
    agg_data,
    x = ~get(x_var), y = ~get(y_var), z = ~value,
    type = "heatmap",
    colors = grDevices::colorRampPalette(c("#667eea", "white", "#764ba2"))(10)
  ) %>%
    plotly::layout(
      title = title %||% paste("Heatmap:", value_var, "by", x_var, "and", y_var),
      xaxis = list(title = x_var),
      yaxis = list(title = y_var)
    )
}

#' Create Density Plot
#'
#' Generates an interactive density plot using Plotly.
#'
#' @export
create_density_plot <- function(data, x_var, color_var = NULL, title = NULL) {
  
  p <- plotly::plot_ly(
    data,
    x = ~get(x_var),
    type = "histogram",
    histnorm = "probability density",
    marker = list(color = "#667eea", opacity = 0.7)
  )
  
  p %>% plotly::layout(
    title = title %||% paste("Density Plot of", x_var),
    xaxis = list(title = x_var),
    yaxis = list(title = "Density")
  )
}

#' Create Pie Chart
#'
#' Generates an interactive pie chart using Plotly.
#'
#' @export
create_pie_chart <- function(data, category_var, value_var = NULL, 
                            title = NULL) {
  
  if (is.null(value_var) || !nzchar(value_var)) {
    agg_data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(category_var))) %>%
      dplyr::summarize(value = dplyr::n(), .groups = "drop")
  } else {
    agg_data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(category_var))) %>%
      dplyr::summarize(value = sum(.data[[value_var]], na.rm = TRUE), .groups = "drop")
  }
  
  colors <- if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    RColorBrewer::brewer.pal(min(nrow(agg_data), 12), "Set2")
  } else {
    c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3")
  }
  
  plotly::plot_ly(
    agg_data,
    labels = ~get(category_var),
    values = ~value,
    type = "pie",
    marker = list(colors = colors)
  ) %>%
    plotly::layout(
      title = title %||% paste("Distribution of", category_var)
    )
}

#' Create 3D Scatter Plot
#'
#' Generates an interactive 3D scatter plot using Plotly.
#'
#' @export
create_3d_scatter <- function(data, x_var, y_var, z_var = NULL, 
                             color_var = NULL, title = NULL) {
  
  if (is.null(z_var)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    z_var <- numeric_cols[3] %||% y_var
  }
  
  p <- plotly::plot_ly(
    data,
    x = ~get(x_var), y = ~get(y_var), z = ~get(z_var),
    type = "scatter3d", mode = "markers"
  )
  
  if (!is.null(color_var) && color_var != "") {
    p <- p %>% plotly::add_trace(marker = list(color = ~get(color_var)))
  } else {
    p <- p %>% plotly::add_trace(marker = list(color = "#667eea"))
  }
  
  p %>% plotly::layout(
    title = title %||% paste("3D Scatter:", x_var, ",", y_var, ",", z_var),
    scene = list(
      xaxis = list(title = x_var),
      yaxis = list(title = y_var),
      zaxis = list(title = z_var)
    )
  )
}

#' Create Parallel Coordinates Plot
#'
#' Generates a parallel coordinates plot for multivariate data.
#'
#' @export
create_parallel_coords <- function(data, vars = NULL, title = NULL) {
  
  if (is.null(vars)) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  } else {
    numeric_data <- data[, vars, drop = FALSE]
  }
  
  if (ncol(numeric_data) < 2) {
    return(empty_plot_message("Need at least 2 numeric variables for parallel coordinates"))
  }
  
  plotly::plot_ly(
    type = "parcoords",
    dimensions = lapply(names(numeric_data), function(col) {
      list(
        label = col,
        values = ~get(col)
      )
    }),
    data = data
  ) %>%
    plotly::layout(
      title = title %||% "Parallel Coordinates Plot"
    )
}

#' Create Radar Chart
#'
#' Generates a radar/spider chart for comparing multiple variables.
#'
#' @export
create_radar_chart <- function(data, vars = NULL, group_var = NULL, 
                              title = NULL) {
  
  if (is.null(vars)) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    vars <- names(numeric_data)
  } else {
    numeric_data <- data[, vars, drop = FALSE]
  }
  
  if (ncol(numeric_data) < 3) {
    return(empty_plot_message("Need at least 3 numeric variables for radar chart"))
  }

  if (is.null(group_var)) {
    # Single radar chart for first row
    row_data <- as.numeric(numeric_data[1, ])
    plotly::plot_ly(
      type = "scatterpolar", r = row_data,
      theta = vars, mode = "lines+markers",
      fill = "toself",
      marker = list(color = "#667eea")
    ) %>%
      plotly::layout(
        polar = list(radialaxis = list(visible = TRUE)),
        title = title %||% paste("Radar Chart:", paste(vars, collapse = ", "))
      )
  } else {
    # Multiple radar charts by group
    groups <- unique(data[[group_var]])
    plot_data <- lapply(groups, function(g) {
      group_data <- data[data[[group_var]] == g, vars, drop = FALSE]
      list(
        r = colMeans(group_data, na.rm = TRUE),
        theta = vars,
        name = g
      )
    })
    
    plotly::plot_ly(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "markers",
                    marker = list(opacity = 0),
                    showlegend = FALSE, hoverinfo = "skip") %>%
      plotly::add_trace(type = "scatterpolar", r = plot_data[[1]]$r,
                        theta = plot_data[[1]]$theta, mode = "lines+markers",
                        fill = "toself", name = plot_data[[1]]$name) %>%
      plotly::layout(
        polar = list(radialaxis = list(visible = TRUE)),
        title = title %||% paste("Radar Chart by", group_var)
      )
  }
}

#' Create Correlation Heatmap
#'
#' Generates a correlation heatmap for numeric variables.
#'
#' @export
create_correlation_heatmap <- function(data, method = "pearson", 
                                       title = NULL) {
  
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) < 2) {
    return(empty_plot_message("Need at least 2 numeric variables for correlation heatmap"))
  }
  
  cor_matrix <- cor(numeric_data, use = "complete.obs", method = method)
  
  plotly::plot_ly(
    x = names(numeric_data), y = names(numeric_data),
    z = cor_matrix, type = "heatmap",
    colors = grDevices::colorRampPalette(c("#667eea", "white", "#764ba2"))(10)
  ) %>%
    plotly::layout(
      title = title %||% paste("Correlation Matrix (", method, ")")
    )
}

#' Create Dashboard Grid
#'
#' Creates a grid of multiple visualizations.
#'
#' @export
create_dashboard_grid <- function(plots, ncols = 2, title = NULL) {
  
  if (length(plots) == 0) {
    return(empty_plot_message("No plots available to display"))
  }
  
  plotly::subplot(plots, nrows = ceiling(length(plots) / ncols), 
                  shareX = FALSE, shareY = FALSE) %>%
    plotly::layout(
      title = title,
      showlegend = FALSE
    )
}

#' Export Plot
#'
#' Exports a Plotly plot to various formats.
#'
#' @param plot Plotly object
#' @param file Output file path
#' @param format Output format (html, png, json, svg)
#' @param width Width in pixels
#' @param height Height in pixels
#' @return Invisible NULL
#'
#' @export
export_plot <- function(plot, file, format = "html", 
                       width = NULL, height = NULL) {
  
  if (format == "html") {
    htmlwidgets::saveWidget(plot, file)
  } else if (format == "png") {
    plotly::export(plot, file, width = width, height = height)
  } else if (format == "json") {
    jsonlite::write_json(plotly::plotly_json(plot), file)
  } else if (format == "svg") {
    plotly::save_image(plot, file, width = width, height = height)
  } else {
    stop("Unsupported format. Use: html, png, json, svg")
  }
  
  invisible(NULL)
}

# Helper: null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
