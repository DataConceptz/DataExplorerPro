# DataExplorerPro - Advanced Visualization Engine
# AI-Powered Data Exploration Studio - Publication Ready Charts

# Internal helper to return a quiet placeholder plot with a message
empty_plot_message <- function(msg) {
  plotly::plot_ly(
    x = 0, y = 0, type = "scatter", mode = "markers",
    marker = list(opacity = 0), showlegend = FALSE, hoverinfo = "skip"
  ) %>% layout(
    annotations = list(list(
      text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
      showarrow = FALSE
    ))
  )
}

# ==============================================================================
# ADVANCED STATISTICAL CHARTS
# ==============================================================================

#' Create Violin-Box Combo Plot
#'
#' Combines violin plot with box plot for detailed distribution visualization.
#'
#' @export
create_violin_box_combo <- function(data, x_var, y_var, color_var = NULL, 
                                    show_points = TRUE, title = NULL) {
  
  p <- plot_ly(data, x = ~get(x_var), y = ~get(y_var))
  
  if (!is.null(color_var) && color_var != "") {
    p <- p %>% add_trace(type = "violin", 
                        box = list(visible = TRUE),
                        meanline = list(visible = TRUE),
                        points = if(show_points) "all" else FALSE,
                        jitter = 0.05,
                        side = "both",
                        marker = list(size = 5),
                        color = ~get(color_var))
  } else {
    p <- p %>% add_trace(type = "violin", 
                        box = list(visible = TRUE),
                        meanline = list(visible = TRUE),
                        points = if(show_points) "all" else FALSE,
                        jitter = 0.05,
                        side = "both",
                        marker = list(size = 5, color = "#667eea"),
                        line = list(color = "#667eea"))
  }
  
  p %>% layout(
    title = title %||% paste("Violin-Box Plot:", y_var, "by", x_var),
    xaxis = list(title = x_var),
    yaxis = list(title = y_var),
    violingap = 0.1
  )
}

#' Create Raincloud Plot
#'
#' Combination of density, box plot, and individual points (raincloud).
#'
#' @export
create_raincloud_plot <- function(data, x_var, y_var, color_var = NULL, 
                                  title = NULL, palette = "viridis") {
  
  if (!is.numeric(data[[y_var]])) {
    return(empty_plot_message("Raincloud plot requires a numeric Y variable"))
  }

  if (!is.null(color_var) && color_var != "") {
    groups <- unique(data[[color_var]])
    colors <- if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      suppressWarnings(colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(groups)))
    } else {
      grDevices::colorRampPalette(c("#0ea5a4", "#2563eb", "#f97316", "#22c55e", "#ef4444", "#8b5cf6"))(length(groups))
    }
    
    p <- plotly::plot_ly(x = numeric(0), y = numeric(0),
                         type = "scatter", mode = "markers",
                         marker = list(opacity = 0),
                         showlegend = FALSE, hoverinfo = "skip")
    
    has_trace <- FALSE
    for (i in seq_along(groups)) {
      group_data <- data[data[[color_var]] == groups[i], ]
      y_vals <- group_data[[y_var]]
      y_vals <- y_vals[is.finite(y_vals)]
      if (length(y_vals) < 2) {
        next
      }
      
      # Density
      dens <- density(y_vals, na.rm = TRUE)
      p <- p %>% add_trace(
        x = dens$y * max(y_vals) / max(dens$x) * 0.3 + i,
        y = dens$y,
        type = "scatter", mode = "lines",
        line = list(color = colors[i], width = 2),
        name = paste(groups[i], "density"),
        showlegend = FALSE
      )
      
      # Box plot
      p <- p %>% add_trace(
        x = rep(i + 0.35, length(y_vals)),
        y = y_vals,
        type = "box",
        boxpoints = FALSE,
        marker = list(color = colors[i]),
        line = list(color = colors[i]),
        name = groups[i],
        showlegend = FALSE
      )
      
      # Individual points (jittered)
      p <- p %>% add_trace(
        x = jitter(rep(i + 0.35, length(y_vals)), amount = 0.15),
        y = y_vals,
        type = "scatter", mode = "markers",
        marker = list(size = 4, color = colors[i], opacity = 0.5),
        name = groups[i],
        showlegend = TRUE
      )
      has_trace <- TRUE
    }
    
    if (!has_trace) {
      return(empty_plot_message("Not enough numeric data for a raincloud plot"))
    }

    p %>% layout(
      title = title %||% paste("Raincloud Plot:", y_var, "by", x_var),
      xaxis = list(title = x_var, tickvals = seq_along(groups), ticktext = groups),
      yaxis = list(title = y_var)
    )
  } else {
    y_vals <- data[[y_var]]
    y_vals <- y_vals[is.finite(y_vals)]
    if (length(y_vals) < 2) {
      return(empty_plot_message("Not enough numeric data for a raincloud plot"))
    }
    dens <- density(y_vals, na.rm = TRUE)
    
    p <- plotly::plot_ly(x = numeric(0), y = numeric(0),
                         type = "scatter", mode = "markers",
                         marker = list(opacity = 0),
                         showlegend = FALSE, hoverinfo = "skip")
    
    # Density
    p <- p %>% add_trace(
      x = dens$y * max(y_vals) / max(dens$x) * 0.3,
      y = dens$y,
      type = "scatter", mode = "lines",
      fill = "tozeroy",
      fillcolor = "rgba(102, 126, 234, 0.3)",
      line = list(color = "#667eea", width = 2),
      name = "Density"
    )
    
    # Box plot
    p <- p %>% add_trace(
      x = rep(0.35, length(y_vals)),
      y = y_vals,
      type = "box",
      boxpoints = "all",
      jitter = 0.3,
      pointpos = -1.8,
      marker = list(color = "#667eea", size = 4),
      line = list(color = "#764ba2"),
      name = "Data"
    )
    
    p %>% layout(
      title = title %||% paste("Raincloud Plot:", y_var),
      xaxis = list(title = "", showticklabels = FALSE),
      yaxis = list(title = y_var)
    )
  }
}

#' Create Ridgeline Plot
#'
#' Overlapping density plots for comparing distributions.
#'
#' @export
create_ridgeline_plot <- function(data, x_var, group_var, title = NULL, 
                                  palette = "viridis", overlap = 0.5) {
  
  if (!is.numeric(data[[x_var]])) {
    return(empty_plot_message("Ridgeline plot requires a numeric X variable"))
  }

  groups <- unique(data[[group_var]])
  n_groups <- length(groups)
  colors <- if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    suppressWarnings(colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_groups))
  } else {
    grDevices::colorRampPalette(c("#0ea5a4", "#2563eb", "#f97316", "#22c55e", "#ef4444", "#8b5cf6"))(n_groups)
  }
  
  p <- plotly::plot_ly(x = numeric(0), y = numeric(0),
                       type = "scatter", mode = "markers",
                       marker = list(opacity = 0),
                       showlegend = FALSE, hoverinfo = "skip")
  
  has_trace <- FALSE
  for (i in seq_along(groups)) {
    group_data <- data[data[[group_var]] == groups[i], ]
    y_vals <- group_data[[x_var]]
    y_vals <- y_vals[is.finite(y_vals)]
    if (length(y_vals) < 2) {
      next
    }
    dens <- density(y_vals, na.rm = TRUE)
    
    # Normalize density
    dens$y <- dens$y / max(dens$y, na.rm = TRUE) * overlap
    
    p <- p %>% add_trace(
      x = dens$x,
      y = dens$y + i,
      type = "scatter", mode = "lines",
      fill = "tozeroy",
      fillcolor = paste0("rgba(", paste(col2rgb(colors[i]), collapse = ","), ", 0.5)"),
      line = list(color = colors[i], width = 2),
      name = groups[i],
      showlegend = TRUE
    )
    has_trace <- TRUE
  }
  
  if (!has_trace) {
    return(empty_plot_message("Not enough numeric data for a ridgeline plot"))
  }

  p %>% layout(
    title = title %||% paste("Ridgeline Plot of", x_var, "by", group_var),
    xaxis = list(title = x_var),
    yaxis = list(title = group_var, 
                 tickvals = seq_along(groups), 
                 ticktext = groups,
                 range = c(0.5, n_groups + 1)),
    hovermode = "closest"
  )
}

#' Create Beeswarm Plot
#'
#' Scatter plot with points arranged to avoid overlap.
#'
#' @export
create_beeswarm_plot <- function(data, x_var, y_var, color_var = NULL, 
                                 title = NULL, palette = "Set2") {
  
  if (!is.null(color_var) && color_var != "") {
    plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
            color = ~get(color_var),
            type = "scatter", mode = "markers",
            marker = list(
              size = 8,
              opacity = 0.7,
              line = list(width = 1, color = "white")
            )) %>% layout(
              title = title %||% paste("Beeswarm Plot:", y_var, "by", x_var),
              xaxis = list(title = x_var),
              yaxis = list(title = y_var)
            )
  } else {
    plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
            type = "scatter", mode = "markers",
            marker = list(
              size = 8,
              color = "#667eea",
              opacity = 0.7,
              line = list(width = 1, color = "white")
            )) %>% layout(
              title = title %||% paste("Beeswarm Plot:", y_var),
              xaxis = list(title = x_var),
              yaxis = list(title = y_var)
            )
  }
}

# ==============================================================================
# PUBLICATION-READY STATISTICAL CHARTS
# ==============================================================================

#' Create Forest Plot
#'
#' Publication-ready forest plot for meta-analysis or effect sizes.
#'
#' @export
create_forest_plot <- function(data, study_var, effect_var, lower_ci_var, upper_ci_var, 
                               group_var = NULL, title = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(empty_plot_message("Forest plot requires data"))
  }
  req_vars <- c(effect_var, lower_ci_var, upper_ci_var)
  if (any(!nzchar(req_vars)) || any(!req_vars %in% names(data))) {
    return(empty_plot_message("Forest plot requires effect, lower CI, and upper CI columns"))
  }
  if (!is.numeric(data[[effect_var]]) || !is.numeric(data[[lower_ci_var]]) || !is.numeric(data[[upper_ci_var]])) {
    return(empty_plot_message("Forest plot requires numeric effect and CI columns"))
  }
  if (is.null(study_var) || !nzchar(study_var) || !(study_var %in% names(data))) {
    study_var <- NULL
  }
  
  p <- plot_ly(data)
  
  # Add error bars for CI
  p <- p %>% add_segments(
    x = ~get(lower_ci_var),
    xend = ~get(upper_ci_var),
    y = ~seq_len(nrow(data)),
    yend = ~seq_len(nrow(data)),
    line = list(color = "#333", width = 2),
    showlegend = FALSE
  )
  
  # Add points for effect sizes
  p <- p %>% add_markers(
    x = ~get(effect_var),
    y = ~seq_len(nrow(data)),
    marker = list(size = 12, color = "#667eea", line = list(color = "white", width = 2)),
    showlegend = FALSE
  )
  
  # Add vertical line at null effect
  p <- p %>% add_lines(
    x = c(0, 0),
    y = c(0, nrow(data) + 1),
    line = list(color = "red", dash = "dash", width = 1),
    showlegend = FALSE
  )
  
  # Add study labels
  if (!is.null(study_var)) {
    p <- p %>% add_annotations(
      x = rep(min(data[[lower_ci_var]], na.rm = TRUE) - 0.1, nrow(data)),
      y = seq_len(nrow(data)),
      text = data[[study_var]],
      showarrow = FALSE,
      xanchor = "right",
      font = list(size = 10)
    )
  }
  
  p %>% layout(
    title = title %||% "Forest Plot",
    xaxis = list(title = "Effect Size (95% CI)"),
    yaxis = list(title = "", showticklabels = FALSE, range = c(0, nrow(data) + 1)),
    margin = list(l = 150)
  )
}

#' Create Bland-Altman Plot
#'
#' Agreement analysis between two measurement methods.
#'
#' @export
create_bland_altman_plot <- function(data, method1_var, method2_var, 
                                     title = NULL, alpha = 0.05) {
  
  m1 <- data[[method1_var]]
  m2 <- data[[method2_var]]
  
  mean_vals <- (m1 + m2) / 2
  diff_vals <- m1 - m2
  
  mean_diff <- mean(diff_vals, na.rm = TRUE)
  sd_diff <- sd(diff_vals, na.rm = TRUE)
  
  lower_LoA <- mean_diff - 1.96 * sd_diff
  upper_LoA <- mean_diff + 1.96 * sd_diff
  
  p <- plot_ly(data, x = ~mean_vals, y = ~diff_vals,
              type = "scatter", mode = "markers",
              marker = list(size = 8, color = "#667eea", opacity = 0.6)) %>%
    add_lines(y = rep(mean_diff, length(mean_vals)),
             line = list(color = "blue", width = 2, dash = "solid"),
             name = "Mean Difference") %>%
    add_lines(y = rep(upper_LoA, length(mean_vals)),
             line = list(color = "red", width = 1.5, dash = "dash"),
             name = "+1.96 SD") %>%
    add_lines(y = rep(lower_LoA, length(mean_vals)),
             line = list(color = "red", width = 1.5, dash = "dash"),
             name = "-1.96 SD") %>%
    add_annotations(
      x = max(mean_vals, na.rm = TRUE),
      y = upper_LoA,
      text = paste("Upper LoA:", round(upper_LoA, 2)),
      showarrow = FALSE,
      xanchor = "right"
    ) %>%
    add_annotations(
      x = max(mean_vals, na.rm = TRUE),
      y = lower_LoA,
      text = paste("Lower LoA:", round(lower_LoA, 2)),
      showarrow = FALSE,
      xanchor = "right"
    ) %>%
    layout(
      title = title %||% paste("Bland-Altman Plot:", method1_var, "vs", method2_var),
      xaxis = list(title = paste("Mean of", method1_var, "and", method2_var)),
      yaxis = list(title = paste(method1_var, "-", method2_var)),
      legend = list(x = 0.1, y = 0.9)
    )
  
  # Calculate and return statistics
  stats <- list(
    mean_difference = mean_diff,
    sd_difference = sd_diff,
    upper_LoA = upper_LoA,
    lower_LoA = lower_LoA,
    correlation = cor(mean_vals, diff_vals, use = "complete.obs")
  )
  
  attr(p, "bland_altman_stats") <- stats
  return(p)
}

#' Create Q-Q Plot
#'
#' Quantile-quantile plot for normality assessment.
#'
#' @export
create_qq_plot <- function(data, var, group_var = NULL, title = NULL,
                          distribution = "norm") {
  
  if (!is.null(group_var) && group_var != "") {
    groups <- unique(data[[group_var]])
    p <- plotly::plot_ly(x = numeric(0), y = numeric(0),
                         type = "scatter", mode = "markers",
                         marker = list(opacity = 0),
                         showlegend = FALSE, hoverinfo = "skip")
    
    has_trace <- FALSE
    for (g in groups) {
      vals <- data[[var]][data[[group_var]] == g]
      vals <- vals[is.finite(vals)]
      if (length(vals) < 2) {
        next
      }
      qq_data <- qqnorm(vals, plot.it = FALSE)
      
      p <- p %>% add_trace(
        x = qq_data$x,
        y = qq_data$y,
        type = "scatter", mode = "markers",
        name = g,
        marker = list(size = 6)
      )
      has_trace <- TRUE
    }
    
    if (!has_trace) {
      return(empty_plot_message("Not enough numeric data for a Q-Q plot"))
    }

    # Add reference line
    p <- p %>% add_lines(
      x = range(qq_data$x),
      y = range(qq_data$x),
      line = list(color = "red", dash = "dash"),
      name = "Reference"
    )
    
    p %>% layout(
      title = title %||% paste("Q-Q Plot of", var),
      xaxis = list(title = "Theoretical Quantiles"),
      yaxis = list(title = "Sample Quantiles"),
      legend = list(x = 0.05, y = 0.95)
    )
  } else {
    vals <- data[[var]]
    qq_data <- qqnorm(vals, plot.it = FALSE)
    
    plot_ly(x = qq_data$x, y = qq_data$y,
           type = "scatter", mode = "markers",
           marker = list(size = 8, color = "#667eea", opacity = 0.6)) %>%
      add_lines(x = range(qq_data$x), y = range(qq_data$x),
               line = list(color = "red", dash = "dash")) %>%
      layout(
        title = title %||% paste("Q-Q Plot of", var),
        xaxis = list(title = "Theoretical Quantiles"),
        yaxis = list(title = "Sample Quantiles")
      )
  }
}

#' Create Volcano Plot
#'
#' For differential expression analysis (e.g., RNA-seq, microarray).
#'
#' @export
create_volcano_plot <- function(data, logfc_var, pval_var, 
                                label_var = NULL, 
                                fc_threshold = 2, pval_threshold = 0.05,
                                title = NULL) {
  
  data$neg_log10_pval <- -log10(data[[pval_var]])
  data$significance <- "Not Significant"
  data$significance[data[[logfc_var]] > log2(fc_threshold) & 
                      data[[pval_var]] < pval_threshold] <- "Up"
  data$significance[data[[logfc_var]] < -log2(fc_threshold) & 
                      data[[pval_var]] < pval_threshold] <- "Down"
  
  colors <- c("Not Significant" = "gray70", "Up" = "#e74c3c", "Down" = "#3498db")
  
  p <- plot_ly(data, x = ~get(logfc_var), y = ~neg_log10_pval,
              color = ~significance,
              colors = colors,
              type = "scatter", mode = "markers",
              marker = list(size = 8, opacity = 0.7)) %>%
    add_lines(x = c(-log2(fc_threshold), -log2(fc_threshold)),
             y = c(0, max(data$neg_log10_pval, na.rm = TRUE)),
             line = list(color = "gray50", dash = "dash")) %>%
    add_lines(x = c(log2(fc_threshold), log2(fc_threshold)),
             y = c(0, max(data$neg_log10_pval, na.rm = TRUE)),
             line = list(color = "gray50", dash = "dash")) %>%
    add_lines(x = c(min(data[[logfc_var]], na.rm = TRUE), max(data[[logfc_var]], na.rm = TRUE)),
             y = c(-log10(pval_threshold), -log10(pval_threshold)),
             line = list(color = "gray50", dash = "dash")) %>%
    layout(
      title = title %||% "Volcano Plot",
      xaxis = list(title = paste("log2(Fold Change)")),
      yaxis = list(title = "-log10(p-value)"),
      legend = list(x = 0.02, y = 0.98)
    )
  
  # Label top genes if requested
  if (!is.null(label_var)) {
    top_genes <- data[order(data$neg_log10_pval, decreasing = TRUE)[1:10], ]
    p <- p %>% add_annotations(
      x = top_genes[[logfc_var]],
      y = top_genes$neg_log10_pval,
      text = top_genes[[label_var]],
      showarrow = TRUE,
      arrowhead = 2,
      arrowsize = 0.5,
      font = list(size = 8)
    )
  }
  
  return(p)
}

#' Create Manhattan Plot
#'
#' For genome-wide association studies (GWAS).
#'
#' @export
create_manhattan_plot <- function(data, chr_var, pos_var, pval_var, 
                                  suggestiveline = 5e-8, genomewideline = 5e-8,
                                  title = NULL) {
  
  # Prepare data
  data$CHR <- as.numeric(data[[chr_var]])
  data$BP <- as.numeric(data[[pos_var]])
  data$P <- data[[pval_var]]
  data$neg_log10_pval <- -log10(data$P)
  
  # Calculate cumulative positions
  chr_lengths <- aggregate(BP ~ CHR, data, max)
  chr_lengths$cumsum <- cumsum(c(0, chr_lengths$BP[-nrow(chr_lengths)]))
  data <- merge(data, chr_lengths[, c("CHR", "cumsum")], by = "CHR")
  data$cum_BP <- data$BP + data$cumsum
  
  # Color by chromosome
  n_chr <- max(data$CHR, na.rm = TRUE)
  colors <- rep(c("#1f77b4", "#aec7e8"), length.out = n_chr)
  
  p <- plot_ly(data, x = ~cum_BP, y = ~neg_log10_pval,
              color = ~as.factor(CHR),
              colors = colors,
              type = "scatter", mode = "markers",
              marker = list(size = 3, opacity = 0.7),
              hoverinfo = "text",
              text = ~paste("CHR:", CHR, "<br>BP:", BP, "<br>P:", P)) %>%
    add_lines(y = rep(-log10(suggestiveline), 2),
             line = list(color = "blue", dash = "dash", width = 1),
             name = "Suggestive") %>%
    add_lines(y = rep(-log10(genomewideline), 2),
             line = list(color = "red", dash = "dash", width = 1),
             name = "Genome-wide") %>%
    layout(
      title = title %||% "Manhattan Plot",
      xaxis = list(
        title = "Chromosome",
        tickvals = chr_lengths$cumsum + chr_lengths$BP / 2,
        ticktext = chr_lengths$CHR
      ),
      yaxis = list(title = "-log10(p-value)"),
      legend = list(show = FALSE)
    )
  
  return(p)
}

# ==============================================================================
# ADVANCED COMPARISON CHARTS
# ==============================================================================

#' Create Grouped Error Bar Plot
#'
#' Bar chart with error bars for comparing means across groups.
#'
#' @export
create_errorbar_plot <- function(data, x_var, y_var, group_var = NULL, 
                                 error_type = "se", ci_level = 0.95,
                                 title = NULL) {
  # Avoid duplicated grouping columns which break dplyr/tibble name repair
  if (!is.null(group_var) && nzchar(group_var) && identical(group_var, x_var)) {
    group_var <- NULL
  }
  
  if (!is.null(group_var) && group_var != "") {
    # Calculate summary statistics
    summary_data <- data %>%
      group_by(!!sym(x_var), !!sym(group_var)) %>%
      summarize(
        mean = mean(!!sym(y_var), na.rm = TRUE),
        se = sd(!!sym(y_var), na.rm = TRUE) / sqrt(n()),
        sd = sd(!!sym(y_var), na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    if (error_type == "ci") {
      df <- pmax(summary_data$n - 1, 1)
      summary_data$error <- qt(ci_level/2 + 0.5, df) * summary_data$se
    } else {
      summary_data$error <- summary_data$se
    }
    
    plot_ly(summary_data, x = ~get(x_var), y = ~mean,
           color = ~get(group_var),
           type = "bar",
           error_y = list(array = summary_data$error, arrayminus = summary_data$error, visible = TRUE),
           marker = list(opacity = 0.8)) %>%
      layout(
        title = title %||% paste("Mean ±", error_type, "of", y_var, "by", x_var),
        xaxis = list(title = x_var),
        yaxis = list(title = paste("Mean of", y_var)),
        barmode = "group"
      )
  } else {
    summary_data <- data %>%
      summarize(
        mean = mean(!!sym(y_var), na.rm = TRUE),
        se = sd(!!sym(y_var), na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
    
    if (error_type == "ci") {
      df <- pmax(summary_data$n - 1, 1)
      summary_data$error <- qt(ci_level/2 + 0.5, df) * summary_data$se
    } else {
      summary_data$error <- summary_data$se
    }
    
    plot_ly(summary_data, x = ~get(x_var), y = ~mean,
           type = "bar",
           error_y = list(array = summary_data$error, arrayminus = summary_data$error, visible = TRUE),
           marker = list(color = "#667eea", opacity = 0.8)) %>%
      layout(
        title = title %||% paste("Mean ±", error_type, "of", y_var),
        xaxis = list(title = x_var),
        yaxis = list(title = paste("Mean of", y_var))
      )
  }
}

#' Create Dumbbell Plot
#'
#' Shows change between two time points or conditions.
#'
#' @export
create_dumbbell_plot <- function(data, label_var, value1_var, value2_var,
                                 color_var = NULL, title = NULL) {
  
  data$change <- data[[value2_var]] - data[[value1_var]]
  
  if (!is.null(color_var) && color_var != "") {
    plot_ly(data) %>%
      add_segments(
        x = ~get(value1_var),
        xend = ~get(value2_var),
        y = ~get(label_var),
        yend = ~get(label_var),
        color = ~get(color_var),
        size = I(2),
        opacity = 0.7
      ) %>%
      add_markers(
        x = ~get(value1_var),
        y = ~get(label_var),
        marker = list(size = 10, color = "#667eea", symbol = "circle"),
        name = value1_var
      ) %>%
      add_markers(
        x = ~get(value2_var),
        y = ~get(label_var),
        marker = list(size = 10, color = "#764ba2", symbol = "diamond"),
        name = value2_var
      ) %>%
      layout(
        title = title %||% paste("Change from", value1_var, "to", value2_var),
        xaxis = list(title = "Value"),
        yaxis = list(title = ""),
        legend = list(x = 0.1, y = 0.9)
      )
  } else {
    plot_ly(data) %>%
      add_segments(
        x = ~get(value1_var),
        xend = ~get(value2_var),
        y = ~get(label_var),
        yend = ~get(label_var),
        line = list(color = "#667eea", size = 2),
        opacity = 0.7
      ) %>%
      add_markers(
        x = ~get(value1_var),
        y = ~get(label_var),
        marker = list(size = 10, color = "#667eea", symbol = "circle"),
        name = value1_var
      ) %>%
      add_markers(
        x = ~get(value2_var),
        y = ~get(label_var),
        marker = list(size = 10, color = "#764ba2", symbol = "diamond"),
        name = value2_var
      ) %>%
      layout(
        title = title %||% paste("Change from", value1_var, "to", value2_var),
        xaxis = list(title = "Value"),
        yaxis = list(title = ""),
        legend = list(x = 0.1, y = 0.9)
      )
  }
}

#' Create Lollipop Chart
#'
#' Modern alternative to bar charts for categorical comparisons.
#'
#' @export
create_lollipop_chart <- function(data, category_var, value_var, 
                                  color_var = NULL, sort = TRUE,
                                  title = NULL) {
  
  if (sort) {
    data <- data[order(data[[value_var]], decreasing = TRUE), ]
  }
  
  if (!is.null(color_var) && color_var != "") {
    plot_ly(data) %>%
      add_segments(
        x = rep(0, nrow(data)),
        xend = ~get(value_var),
        y = ~get(category_var),
        yend = ~get(category_var),
        color = ~get(color_var),
        line = list(width = 3)
      ) %>%
      add_markers(
        x = ~get(value_var),
        y = ~get(category_var),
        marker = list(size = 12),
        showlegend = FALSE
      ) %>%
      layout(
        title = title %||% paste("Lollipop Chart of", value_var),
        xaxis = list(title = value_var),
        yaxis = list(title = ""),
        legend = list(x = 0.1, y = 0.9)
      )
  } else {
    plot_ly(data) %>%
      add_segments(
        x = rep(0, nrow(data)),
        xend = ~get(value_var),
        y = ~get(category_var),
        yend = ~get(category_var),
        line = list(color = "#667eea", width = 3)
      ) %>%
      add_markers(
        x = ~get(value_var),
        y = ~get(category_var),
        marker = list(size = 12, color = "#764ba2"),
        showlegend = FALSE
      ) %>%
      layout(
        title = title %||% paste("Lollipop Chart of", value_var),
        xaxis = list(title = value_var),
        yaxis = list(title = "")
      )
  }
}

#' Create Diverging Bar Chart
#'
## For showing positive/negative values from a central point.
#'
#' @export
create_diverging_bar_chart <- function(data, category_var, value_var, 
                                       midpoint = 0, title = NULL) {
  
  data$color <- ifelse(data[[value_var]] >= midpoint, "Positive", "Negative")
  data <- data[order(data[[value_var]]), ]
  
  colors <- c("Positive" = "#27ae60", "Negative" = "#e74c3c")
  
  plot_ly(data, x = ~get(value_var), y = ~get(category_var),
         type = "bar", orientation = "h",
         marker = list(color = ~color, colors = colors)) %>%
    layout(
      title = title %||% paste("Diverging Bar Chart of", value_var),
      xaxis = list(title = value_var),
      yaxis = list(title = ""),
      legend = list(x = 0.1, y = 0.9)
    )
}

# ==============================================================================
# TIME SERIES & TREND CHARTS
# ==============================================================================

#' Create Area Chart with Confidence Bands
#'
#' Time series with uncertainty visualization.
#'
#' @export
create_area_confidence_band <- function(data, time_var, value_var, 
                                        group_var = NULL, ci_level = 0.95,
                                        title = NULL) {
  
  if (!is.null(group_var) && group_var != "") {
    summary_data <- data %>%
      group_by(!!sym(time_var), !!sym(group_var)) %>%
      summarize(
        mean = mean(!!sym(value_var), na.rm = TRUE),
        se = sd(!!sym(value_var), na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    z <- qnorm(1 - (1 - ci_level) / 2)
    summary_data$upper <- summary_data$mean + z * summary_data$se
    summary_data$lower <- summary_data$mean - z * summary_data$se
    
    plot_ly(summary_data) %>%
      add_ribbons(
        x = ~get(time_var),
        ymin = ~lower,
        ymax = ~upper,
        color = ~get(group_var),
        opacity = 0.2
      ) %>%
      add_lines(
        x = ~get(time_var),
        y = ~mean,
        color = ~get(group_var),
        line = list(width = 2)
      ) %>%
      layout(
        title = title %||% paste(value_var, "over", time_var, paste0("(", ci_level * 100, "% CI)")),
        xaxis = list(title = time_var),
        yaxis = list(title = value_var),
        legend = list(x = 0.05, y = 0.95)
      )
  } else {
    summary_data <- data %>%
      group_by(!!sym(time_var)) %>%
      summarize(
        mean = mean(!!sym(value_var), na.rm = TRUE),
        se = sd(!!sym(value_var), na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    z <- qnorm(1 - (1 - ci_level) / 2)
    summary_data$upper <- summary_data$mean + z * summary_data$se
    summary_data$lower <- summary_data$mean - z * summary_data$se
    
    plot_ly(summary_data) %>%
      add_ribbons(
        x = ~get(time_var),
        ymin = ~lower,
        ymax = ~upper,
        fillcolor = "rgba(102, 126, 234, 0.2)",
        line = list(width = 0)
      ) %>%
      add_lines(
        x = ~get(time_var),
        y = ~mean,
        line = list(color = "#667eea", width = 2)
      ) %>%
      add_markers(
        x = ~get(time_var),
        y = ~mean,
        marker = list(size = 6, color = "#667eea")
      ) %>%
      layout(
        title = title %||% paste(value_var, "over", time_var, paste0("(", ci_level * 100, "% CI)")),
        xaxis = list(title = time_var),
        yaxis = list(title = value_var)
      )
  }
}

#' Create Stacked Area Chart
#'
#' For showing composition over time.
#'
#' @export
create_stacked_area_chart <- function(data, time_var, value_var, group_var,
                                      title = NULL, percent = FALSE) {
  
  if (percent) {
    plot_ly(data, x = ~get(time_var), y = ~get(value_var),
           fill = ~get(group_var),
           type = "scatter", mode = "none",
           stackgroup = "one",
           groupnorm = "percent") %>%
      layout(
        title = title %||% paste("Percentage of", value_var, "by", group_var),
        xaxis = list(title = time_var),
        yaxis = list(title = "Percentage")
      )
  } else {
    plot_ly(data, x = ~get(time_var), y = ~get(value_var),
           fill = ~get(group_var),
           type = "scatter", mode = "none",
           stackgroup = "one") %>%
      layout(
        title = title %||% paste("Stacked", value_var, "by", group_var),
        xaxis = list(title = time_var),
        yaxis = list(title = value_var)
      )
  }
}

#' Create Calendar Heatmap
#'
#' For visualizing temporal patterns (e.g., daily activity).
#'
#' @export
create_calendar_heatmap <- function(data, date_var, value_var, title = NULL) {
  
  data$date <- as.Date(data[[date_var]])
  data$year <- year(data$date)
  data$month <- month(data$date)
  data$day <- day(data$date)
  data$weekday <- wday(data$date)
  data$week <- week(data$date)
  
  # Create calendar layout
  calendar_data <- data %>%
    group_by(year, month, weekday, week) %>%
    summarize(value = mean(!!sym(value_var), na.rm = TRUE), .groups = "drop")
  
  plot_ly(calendar_data, x = ~week, y = ~weekday, z = ~value,
         type = "heatmap",
         colors = grDevices::colorRampPalette(c("#f7fbff", "#08306b"))(10),
         hoverinfo = "x+y+z") %>%
    layout(
      title = title %||% paste("Calendar Heatmap of", value_var),
      xaxis = list(title = "Week of Month"),
      yaxis = list(
        title = "",
        tickvals = 1:7,
        ticktext = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      )
    )
}

# ==============================================================================
# MULTIVARIATE & ADVANCED CHARTS
# ==============================================================================

#' Create Bubble Chart
#'
#' Scatter plot with sized points for third variable.
#'
#' @export
create_bubble_chart <- function(data, x_var, y_var, size_var, color_var = NULL,
                                max_size = 50, title = NULL) {
  
  if (!is.null(color_var) && color_var != "") {
    plot_ly(data, x = ~get(x_var), y = ~get(y_var),
           size = ~get(size_var),
           color = ~get(color_var),
           type = "scatter", mode = "markers",
           marker = list(
             sizemode = "area",
             sizeref = 2 * max_size / (50^2),
             opacity = 0.7,
             line = list(width = 1, color = "white")
           )) %>%
      layout(
        title = title %||% paste("Bubble Chart:", x_var, "vs", y_var, "(size =", size_var, ")"),
        xaxis = list(title = x_var),
        yaxis = list(title = y_var)
      )
  } else {
    plot_ly(data, x = ~get(x_var), y = ~get(y_var),
           size = ~get(size_var),
           type = "scatter", mode = "markers",
           marker = list(
             sizemode = "area",
             sizeref = 2 * max_size / (50^2),
             color = "#667eea",
             opacity = 0.7,
             line = list(width = 1, color = "white")
           )) %>%
      layout(
        title = title %||% paste("Bubble Chart:", x_var, "vs", y_var, "(size =", size_var, ")"),
        xaxis = list(title = x_var),
        yaxis = list(title = y_var)
      )
  }
}

#' Create Ternary Plot
#'
#' For visualizing proportions of three variables.
#'
#' @export
create_ternary_plot <- function(data, a_var, b_var, c_var, 
                                color_var = NULL, size_var = NULL,
                                title = NULL) {
  
  # Normalize to proportions
  total <- data[[a_var]] + data[[b_var]] + data[[c_var]]
  a_prop <- data[[a_var]] / total
  b_prop <- data[[b_var]] / total
  c_prop <- data[[c_var]] / total
  
  # Convert to Cartesian coordinates
  x <- 0.5 * (2 * b_prop + c_prop)
  y <- sqrt(3) / 2 * c_prop
  
  if (!is.null(color_var) && !is.null(size_var)) {
    plotly::plot_ly(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "markers",
                    marker = list(opacity = 0),
                    showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(
        x = x, y = y,
        marker = list(
          size = data[[size_var]],
          color = data[[color_var]],
          colorscale = "Viridis",
          opacity = 0.7
        ),
        text = paste(
          paste(a_var, ":", round(a_prop * 100, 1), "%"),
          paste(b_var, ":", round(b_prop * 100, 1), "%"),
          paste(c_var, ":", round(c_prop * 100, 1), "%"),
          sep = "<br>"
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = title %||% "Ternary Plot",
        xaxis = list(title = paste(b_var, "+ 0.5", c_var), range = c(0, 1)),
        yaxis = list(title = paste0(sqrt(3) / 2, "*", c_var), range = c(0, 0.87)),
        showlegend = FALSE
      )
  } else if (!is.null(color_var)) {
    plotly::plot_ly(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "markers",
                    marker = list(opacity = 0),
                    showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(
        x = x, y = y,
        marker = list(
          color = data[[color_var]],
          colorscale = "Viridis",
          opacity = 0.7
        ),
        text = paste(
          paste(a_var, ":", round(a_prop * 100, 1), "%"),
          paste(b_var, ":", round(b_prop * 100, 1), "%"),
          paste(c_var, ":", round(c_prop * 100, 1), "%"),
          sep = "<br>"
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = title %||% "Ternary Plot",
        xaxis = list(title = paste(b_var, "+ 0.5", c_var), range = c(0, 1)),
        yaxis = list(title = paste0(sqrt(3) / 2, "*", c_var), range = c(0, 0.87)),
        showlegend = FALSE
      )
  } else {
    plotly::plot_ly(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "markers",
                    marker = list(opacity = 0),
                    showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(
        x = x, y = y,
        marker = list(
          size = 10,
          color = "#667eea",
          opacity = 0.7
        ),
        text = paste(
          paste(a_var, ":", round(a_prop * 100, 1), "%"),
          paste(b_var, ":", round(b_prop * 100, 1), "%"),
          paste(c_var, ":", round(c_prop * 100, 1), "%"),
          sep = "<br>"
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = title %||% "Ternary Plot",
        xaxis = list(title = paste(b_var, "+ 0.5", c_var), range = c(0, 1)),
        yaxis = list(title = paste0(sqrt(3) / 2, "*", c_var), range = c(0, 0.87)),
        showlegend = FALSE
      )
  }
}

#' Create Sankey Diagram
#'
#' For visualizing flow and relationships.
#'
#' @export
create_sankey_diagram <- function(data, source_var, target_var, value_var,
                                  title = NULL) {
  
  # Get unique nodes
  nodes <- unique(c(data[[source_var]], data[[target_var]]))
  node_indices <- setNames(seq_along(nodes) - 1, nodes)
  
  # Create links
  links <- data.frame(
    source = node_indices[data[[source_var]]],
    target = node_indices[data[[target_var]]],
    value = data[[value_var]]
  )
  
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = nodes,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = links$source,
      target = links$target,
      value = links$value
    )
  ) %>% layout(
    title = title %||% "Sankey Diagram",
    font = list(size = 10)
  )
}

#' Create Chord Diagram
#'
#' For visualizing relationships between groups.
#'
#' @export
create_chord_diagram <- function(data, from_var, to_var, value_var,
                                 title = NULL) {
  
  # Create adjacency matrix
  groups <- unique(c(data[[from_var]], data[[to_var]]))
  n <- length(groups)
  adj_matrix <- matrix(0, n, n, dimnames = list(groups, groups))
  
  for (i in seq_len(nrow(data))) {
    adj_matrix[data[[from_var]][i], data[[to_var]][i]] <- 
      adj_matrix[data[[from_var]][i], data[[to_var]][i]] + data[[value_var]][i]
  }
  
  # Use plotly's chord diagram (via parcoords workaround)
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = groups,
      pad = 10,
      thickness = 15
    ),
    link = list(
      source = rep(seq_len(n) - 1, each = n),
      target = rep(seq_len(n) - 1, n),
      value = as.vector(adj_matrix)
    )
  ) %>% layout(
    title = title %||% "Chord Diagram",
    font = list(size = 10)
  )
}

#' Create Sunburst Chart
#'
#' Hierarchical data visualization.
#'
#' @export
create_sunburst_chart <- function(data, labels_var, parents_var, values_var,
                                title = NULL) {
  
  plot_ly(
    type = "sunburst",
    labels = data[[labels_var]],
    parents = data[[parents_var]],
    values = data[[values_var]],
    branchvalues = "total"
  ) %>% layout(
    title = title %||% "Sunburst Chart"
  )
}

#' Create Treemap
#'
#' Hierarchical data with rectangular tiling.
#'
#' @export
create_treemap <- function(data, labels_var, parents_var, values_var,
                          color_var = NULL, title = NULL) {
  
  if (!is.null(color_var)) {
    plot_ly(
      type = "treemap",
      labels = data[[labels_var]],
      parents = data[[parents_var]],
      values = data[[values_var]],
      marker = list(colors = data[[color_var]])
    ) %>% layout(
      title = title %||% "Treemap"
    )
  } else {
    plot_ly(
      type = "treemap",
      labels = data[[labels_var]],
      parents = data[[parents_var]],
      values = data[[values_var]]
    ) %>% layout(
      title = title %||% "Treemap"
    )
  }
}

# ==============================================================================
# PUBLICATION CUSTOMIZATION
# ==============================================================================

#' Apply Publication Theme
#'
#' Applies journal-style formatting to plots.
#'
#' @param plot Plotly object
#' @param theme Theme name (apa, nature, science,plos)
#' @param font_size Base font size
#' @return Styled Plotly object
#'
#' @export
apply_publication_theme <- function(plot, theme = "apa", font_size = 12) {
  
  themes <- list(
    apa = list(
      font_family = "Times New Roman",
      bg_color = "white",
      axis_color = "black",
      grid = FALSE
    ),
    nature = list(
      font_family = "Arial",
      bg_color = "white",
      axis_color = "black",
      grid = FALSE
    ),
    science = list(
      font_family = "Helvetica",
      bg_color = "white",
      axis_color = "black",
      grid = FALSE
    ),
    plos = list(
      font_family = "Arial",
      bg_color = "white",
      axis_color = "black",
      grid = TRUE
    )
  )
  
  t <- themes[[theme]] %||% themes$apa
  
  plot <- plot %>% layout(
    font = list(family = t$font_family, size = font_size),
    plot_bgcolor = t$bg_color,
    paper_bgcolor = t$bg_color,
    xaxis = list(
      title_font = list(family = t$font_family, size = font_size + 2),
      tickfont = list(family = t$font_family, size = font_size - 2),
      showgrid = t$grid,
      gridcolor = "gray90",
      zeroline = FALSE,
      linecolor = t$axis_color,
      linewidth = 2
    ),
    yaxis = list(
      title_font = list(family = t$font_family, size = font_size + 2),
      tickfont = list(family = t$font_family, size = font_size - 2),
      showgrid = t$grid,
      gridcolor = "gray90",
      zeroline = FALSE,
      linecolor = t$axis_color,
      linewidth = 2
    ),
    legend = list(
      font = list(family = t$font_family, size = font_size),
      bgcolor = "transparent",
      bordercolor = t$axis_color,
      borderwidth = 1
    ),
    margin = list(l = 60, r = 30, t = 50, b = 60)
  )
  
  return(plot)
}

#' Export Publication Quality
#'
#' Exports plot at high resolution for publications.
#'
#' @param plot Plotly object
#' @param file Output file path
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution (default: 300)
#' @param format Output format (png, pdf, svg, eps)
#' @return Invisible NULL
#'
#' @export
export_publication_quality <- function(plot, file, width = 6, height = 4,
                                       dpi = 300, format = "png") {
  
  # Convert inches to pixels
  width_px <- width * dpi
  height_px <- height * dpi
  
  if (format == "png") {
    plotly::export(plot, file, width = width_px, height = height_px, scale = 1)
  } else if (format == "pdf") {
    # For PDF, useorca or save via Cairo
    tryCatch({
      plotly::save_image(plot, file, width = width, height = height, scale = 1)
    }, error = function(e) {
      warning("PDF export may require orca. Saving as PNG instead.")
      plotly::export(plot, gsub("\\.pdf$", ".png", file), 
                    width = width_px, height = height_px)
    })
  } else if (format == "svg") {
    plotly::save_image(plot, file, width = width, height = height)
  } else if (format == "eps") {
    warning("EPS format not directly supported. Consider using SVG and converting.")
  } else {
    stop("Unsupported format. Use: png, pdf, svg")
  }
  
  invisible(NULL)
}

#' Create Multi-Panel Figure
#'
#' Combines multiple plots into a single figure.
#'
#' @param plots List of Plotly objects
#' @param ncols Number of columns
#' @param titles Optional titles for each panel
#' @param shared_xaxis Whether x-axes are shared
#' @param shared_yaxis Whether y-axes are shared
#' @return Combined Plotly figure
#'
#' @export
create_multi_panel_figure <- function(plots, ncols = 2, titles = NULL,
                                      shared_xaxis = FALSE, 
                                      shared_yaxis = FALSE) {
  
  if (length(plots) == 0) {
    stop("No plots provided")
  }
  
  nrows <- ceiling(length(plots) / ncols)
  
  fig <- subplot(plots, nrows = nrows, 
                shareX = shared_xaxis, 
                shareY = shared_yaxis,
                which_layout = "merge")
  
  if (!is.null(titles)) {
    for (i in seq_along(titles)) {
      fig <- fig %>% layout(
        annotations = list(
          list(
            x = ((i - 1) %% ncols) / (ncols - 1),
            y = 1 - floor((i - 1) / ncols) / nrows,
            text = titles[i],
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            font = list(size = 14)
          )
        )
      )
    }
  }
  
  fig %>% layout(
    showlegend = FALSE,
    margin = list(l = 50, r = 30, t = 50 + 20 * nrows, b = 50)
  )
}

# Helper: null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
