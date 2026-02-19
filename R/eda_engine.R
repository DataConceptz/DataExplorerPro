# DataExplorerPro - Automatic EDA Engine
# AI-Powered Data Exploration Studio

#' Generate Comprehensive EDA Report
#'
#' Creates a complete exploratory data analysis report for a dataset.
#'
#' @param data A data frame to analyze
#' @param output_format Output format (html, pdf, rmarkdown)
#' @param include_plots Whether to include visualizations
#' @param ai_insights Whether to generate AI-powered insights
#' @param model Ollama model for AI insights
#' @return Path to generated report (or renders if output_format = "html")
#'
#' @export
generate_eda_report <- function(data, output_format = "html", 
                                include_plots = TRUE,
                                ai_insights = TRUE,
                                model = "llama3.2") {
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Generate report based on format
  if (output_format == "html") {
    return(generate_html_eda_report(data, include_plots, ai_insights, model))
  } else if (output_format == "rmarkdown") {
    return(generate_rmarkdown_eda_report(data))
  } else {
    stop("Unsupported output format. Use 'html' or 'rmarkdown'")
  }
}

#' Generate HTML EDA Report
#'
#' Creates a standalone HTML EDA report.
#'
#' @keywords internal
generate_html_eda_report <- function(data, include_plots, ai_insights, model) {
  
  # Create temporary directory for report
  temp_dir <- tempdir()
  report_file <- file.path(temp_dir, paste0("EDA_Report_", format(Sys.Date(), "%Y%m%d"), ".html"))
  
  # Generate report content
  report_content <- build_eda_html_content(data, include_plots, ai_insights, model)
  
  # Write HTML file
  writeLines(report_content, report_file)
  
  message(paste("Report generated:", report_file))
  
  # Open in browser if in interactive session
  if (interactive()) {
    browseURL(report_file)
  }
  
  return(invisible(report_file))
}

# Build HTML content for EDA report
build_eda_html_content <- function(data, include_plots, ai_insights, model) {
  
  # Get data summary
  data_summary <- summarize_data(data)
  
  # Get AI insights if requested
  insights_text <- ""
  if (ai_insights) {
    tryCatch({
      insights <- generate_data_insights(data, model = model)
      insights_text <- paste0(
        "<h2>AI-Generated Insights</h2>\n<ul>\n",
        paste0("<li>", insights, "</li>\n", collapse = ""),
        "</ul>\n"
      )
    }, error = function(e) {
      insights_text <- "<p>Could not generate AI insights.</p>"
    })
  }
  
  # Build HTML
  html_content <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>DataExplorerPro - EDA Report</title>
  <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
  <style>
    body { font-family: "Segoe UI", Arial, sans-serif; margin: 40px; background: #f5f5f5; }
    .container { max-width: 1200px; margin: 0 auto; background: white; padding: 40px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
    h1 { color: #667eea; border-bottom: 3px solid #667eea; padding-bottom: 10px; }
    h2 { color: #764ba2; margin-top: 30px; }
    .summary-box { display: grid; grid-template-columns: repeat(3, 1fr); gap: 20px; margin: 20px 0; }
    .stat-card { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; }
    .stat-card .value { font-size: 32px; font-weight: bold; }
    .stat-card .label { font-size: 14px; opacity: 0.9; }
    table { width: 100%; border-collapse: collapse; margin: 20px 0; }
    th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
    th { background: #667eea; color: white; }
    tr:hover { background: #f5f5f5; }
    .plot-container { margin: 20px 0; padding: 20px; background: #fafafa; border-radius: 8px; }
    .insights-box { background: #e8f4fd; border-left: 4px solid #2196F3; padding: 20px; margin: 20px 0; }
    code { background: #f4f4f4; padding: 2px 6px; border-radius: 4px; }
    pre { background: #f4f4f4; padding: 15px; border-radius: 8px; overflow-x: auto; }
    .footer { text-align: center; margin-top: 40px; color: #666; font-size: 12px; }
  </style>
</head>
<body>
  <div class="container">
    <h1>📊 DataExplorerPro - EDA Report</h1>
    <p>Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
    
    <h2>📋 Data Overview</h2>
    <div class="summary-box">
      <div class="stat-card">
        <div class="value">', nrow(data), '</div>
        <div class="label">Rows</div>
      </div>
      <div class="stat-card">
        <div class="value">', ncol(data), '</div>
        <div class="label">Columns</div>
      </div>
      <div class="stat-card">
        <div class="value">', round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 1), '%</div>
        <div class="label">Missing</div>
      </div>
    </div>
    
    <h2>📝 Data Structure</h2>
    <table>
      <tr><th>Column</th><th>Type</th><th>Missing</th><th>Unique</th><th>Example</th></tr>
      ', paste0(sapply(names(data), function(col) {
        n_missing <- sum(is.na(data[[col]]))
        n_unique <- length(unique(data[[col]]))
        example <- ifelse(n_unique > 0, as.character(data[[col]][1]), "NA")
        paste0('<tr><td>', col, '</td><td>', class(data[[col]])[1], '</td><td>', n_missing, 
               '</td><td>', n_unique, '</td><td>', substr(example, 1, 30), '</td></tr>')
      }), collapse = "\n"), '
    </table>
    
    <h2>📈 Summary Statistics</h2>
    ', if (include_plots) paste0('<div class="plot-container"><div id="dist_plot"></div></div>') else '', '
    <pre>', capture.output(summary(data)), '</pre>
    
    <h2>🔗 Correlations</h2>
    ', if (include_plots) paste0('<div class="plot-container"><div id="corr_plot"></div></div>') else '', '
    
    ', insights_text, '
    
    <div class="footer">
      <p>Generated by DataExplorerPro v1.0.0 | Powered by Ollama</p>
    </div>
  </div>
  
  ', if (include_plots) generate_plotly_scripts(data) else '', '
</body>
</html>')
  
  return(html_content)
}

# Generate Plotly.js scripts for HTML report
generate_plotly_scripts <- function(data) {
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  scripts <- '<script>'
  
  # Distribution plot
  if (length(numeric_cols) > 0) {
    first_numeric <- numeric_cols[1]
    dist_data <- jsonlite::toJSON(list(x = data[[first_numeric]]), auto_unbox = TRUE)
    scripts <- paste0(scripts, '
    var distData = ', dist_data, ';
    Plotly.newPlot("dist_plot", [{
      x: distData.x,
      type: "histogram",
      marker: { color: "#667eea", opacity: 0.7 }
    }], {
      title: "Distribution of ', first_numeric, '",
      xaxis: { title: "', first_numeric, '" },
      yaxis: { title: "Count" }
    });')
  }
  
  # Correlation heatmap
  if (length(numeric_cols) >= 2) {
    cor_matrix <- cor(data[, numeric_cols, drop = FALSE], use = "complete.obs")
    corr_json <- jsonlite::toJSON(list(
      z = cor_matrix,
      x = numeric_cols,
      y = numeric_cols
    ), auto_unbox = TRUE)
    scripts <- paste0(scripts, '
    var corrData = ', corr_json, ';
    Plotly.newPlot("corr_plot", [{
      x: corrData.x,
      y: corrData.y,
      z: corrData.z,
      type: "heatmap",
      colorscale: [[0, "#667eea"], [0.5, "white"], [1, "#764ba2"]]
    }], {
      title: "Correlation Matrix"
    });')
  }
  
  scripts <- paste0(scripts, '\n</script>')
  
  return(scripts)
}

# Generate RMarkdown report template
generate_rmarkdown_eda_report <- function(data) {
  
  rmd_content <- '---
title: "DataExplorerPro - EDA Report"
author: "DataExplorerPro"
date: "`r format(Sys.time(), \'%Y-%m-%d %H:%M:%S\')`"
output: html_document
---

```{r setup, include=FALSE}
library(ggplot2)
library(dplyr)
library(plotly)
library(skimr)

data <- readRDS("data.rds")
```

# Data Overview

```{r overview}
glimpse(data)
skim(data)
```

# Summary Statistics

```{r summary}
summary(data)
```

# Visualizations

## Distributions

```{r distributions, fig.height=8}
ggplot(data, aes(x = .)) + geom_histogram() + theme_minimal()
```

## Correlations

```{r correlations}
cor_matrix <- cor(select_if(data, is.numeric), use = "complete.obs")
heatmap(cor_matrix)
```

# AI Insights

```{r ai-insights, eval=require(ollama)}
# Generate AI insights using Ollama
insights <- generate_data_insights(data)
print(insights)
```
'
  
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, temp_file)
  
  return(temp_file)
}

#' Summarize Data
#'
#' Creates a comprehensive summary of a dataset.
#'
#' @param data A data frame
#' @return List with summary information
#'
#' @export
summarize_data <- function(data) {
  
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  list(
    dimensions = list(rows = nrow(data), columns = ncol(data)),
    memory_usage = object.size(data),
    missing_values = list(
      total = sum(is.na(data)),
      percentage = round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 2)
    ),
    column_types = table(sapply(data, function(x) class(x)[1])),
    numeric_summary = if (any(sapply(data, is.numeric))) {
      num_cols <- names(data)[sapply(data, is.numeric)]
      lapply(data[, num_cols, drop = FALSE], function(x) {
        list(
          mean = mean(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          q25 = quantile(x, 0.25, na.rm = TRUE),
          q75 = quantile(x, 0.75, na.rm = TRUE)
        )
      })
    } else NULL,
    categorical_summary = if (any(sapply(data, function(x) is.character(x) || is.factor(x)))) {
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      lapply(data[, cat_cols, drop = FALSE], function(x) {
        list(
          unique = length(unique(x)),
          mode = names(sort(table(x), decreasing = TRUE))[1],
          top_values = head(sort(table(x), decreasing = TRUE), 5)
        )
      })
    } else NULL
  )
}

#' Detect Outliers
#'
#' Identifies outliers in numeric columns using IQR method.
#'
#' @param data A data frame
#' @param method Outlier detection method (iqr, zscore)
#' @param threshold Threshold for detection (default: 1.5 for IQR)
#' @return List with outlier information
#'
#' @export
detect_outliers <- function(data, method = "iqr", threshold = 1.5) {
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  outliers <- lapply(numeric_cols, function(col) {
    vals <- data[[col]]
    
    if (method == "iqr") {
      q1 <- quantile(vals, 0.25, na.rm = TRUE)
      q3 <- quantile(vals, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - threshold * iqr
      upper <- q3 + threshold * iqr
      
      outlier_mask <- vals < lower | vals > upper
      outlier_vals <- vals[outlier_mask]
      
      list(
        column = col,
        method = "IQR",
        threshold = threshold,
        lower_bound = lower,
        upper_bound = upper,
        n_outliers = sum(outlier_mask, na.rm = TRUE),
        percentage = round(100 * sum(outlier_mask, na.rm = TRUE) / sum(!is.na(vals)), 2),
        outlier_values = head(outlier_vals, 10)
      )
    } else if (method == "zscore") {
      z_scores <- abs(scale(vals))
      outlier_mask <- z_scores > threshold
      
      list(
        column = col,
        method = "Z-Score",
        threshold = threshold,
        n_outliers = sum(outlier_mask, na.rm = TRUE),
        percentage = round(100 * sum(outlier_mask, na.rm = TRUE) / sum(!is.na(vals)), 2),
        outlier_values = head(vals[outlier_mask], 10)
      )
    }
  })
  
  return(outliers)
}

#' Analyze Correlations
#'
#' Computes and analyzes correlations between variables.
#'
#' @param data A data frame
#' @param method Correlation method (pearson, spearman, kendall)
#' @return List with correlation matrix and analysis
#'
#' @export
analyze_correlations <- function(data, method = "pearson") {
  
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) < 2) {
    return(list(message = "Need at least 2 numeric columns"))
  }
  
  cor_matrix <- cor(numeric_data, use = "complete.obs", method = method)
  
  # Find strong correlations
  cor_pairs <- which(cor_matrix > 0.7 | cor_matrix < -0.7, arr.ind = TRUE)
  cor_pairs <- cor_pairs[cor_pairs[,1] < cor_pairs[,2], , drop = FALSE]
  
  strong_correlations <- apply(cor_pairs, 1, function(idx) {
    list(
      var1 = rownames(cor_matrix)[idx[1]],
      var2 = colnames(cor_matrix)[idx[2]],
      correlation = round(cor_matrix[idx[1], idx[2]], 3)
    )
  })
  
  list(
    correlation_matrix = cor_matrix,
    strong_correlations = strong_correlations,
    method = method
  )
}

#' Suggest Analysis Steps
#'
#' AI-powered suggestions for data analysis workflow.
#'
#' @param data A data frame
#' @param goal Analysis goal
#' @param model Ollama model
#' @return Character vector of suggestions
#'
#' @export
suggest_analysis_steps <- function(data, goal = "exploratory", model = "llama3.2") {
  
  data_summary <- paste0(
    "Data: ", nrow(data), "x", ncol(data), " | ",
    "Numeric: ", sum(sapply(data, is.numeric)), " | ",
    "Categorical: ", sum(sapply(data, function(x) is.character(x) || is.factor(x)))
  )
  
  prompt <- paste0(
    "Suggest 5 analysis steps for this dataset.\n\n",
    data_summary, "\n\n",
    "Goal: ", goal, "\n\n",
    "Return as numbered list, one step per line."
  )
  
  response <- ollama_chat(prompt, model = model)
  
  steps <- strsplit(response, "\n")[[1]]
  steps <- steps[grepl("^\\d", steps)]
  
  return(steps)
}