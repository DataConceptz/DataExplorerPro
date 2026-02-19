# DataExplorerPro - Dashboard Engine
# Interactive Dashboard Builder with Templates

#' Create Dashboard Template
#'
#' Creates a dashboard layout template with multiple panels
#'
#' @param template_name Name of the template to use
#' @return Dashboard configuration list
#' @export
create_dashboard_template <- function(template_name = "executive") {
  
  templates <- list(
    executive = list(
      name = "Executive Summary",
      description = "High-level KPIs and summary charts",
      layout = list(
        rows = 3,
        cols = 4,
        panels = list(
          list(id = "kpi1", type = "stat", row = 1, col = 1, width = 1, height = 1, title = "Total Records"),
          list(id = "kpi2", type = "stat", row = 1, col = 2, width = 1, height = 1, title = "Variables"),
          list(id = "kpi3", type = "stat", row = 1, col = 3, width = 1, height = 1, title = "Missing %"),
          list(id = "kpi4", type = "stat", row = 1, col = 4, width = 1, height = 1, title = "Numeric Vars"),
          list(id = "chart1", type = "chart", row = 2, col = 1, width = 2, height = 2, title = "Distribution", chart_type = "histogram"),
          list(id = "chart2", type = "chart", row = 2, col = 3, width = 2, height = 2, title = "Correlation", chart_type = "heatmap"),
          list(id = "table1", type = "table", row = 4, col = 1, width = 4, height = 1, title = "Summary Statistics")
        )
      )
    ),
    
    analytical = list(
      name = "Deep Dive Analysis",
      description = "Detailed charts and statistical analysis",
      layout = list(
        rows = 4,
        cols = 3,
        panels = list(
          list(id = "scatter1", type = "chart", row = 1, col = 1, width = 2, height = 2, title = "Scatter Analysis", chart_type = "scatter"),
          list(id = "box1", type = "chart", row = 1, col = 3, width = 1, height = 2, title = "Distribution", chart_type = "box"),
          list(id = "bar1", type = "chart", row = 3, col = 1, width = 1, height = 2, title = "Categories", chart_type = "bar"),
          list(id = "line1", type = "chart", row = 3, col = 2, width = 2, height = 2, title = "Trends", chart_type = "line")
        )
      )
    ),
    
    comparison = list(
      name = "Side-by-Side Comparison",
      description = "Compare multiple variables or groups",
      layout = list(
        rows = 2,
        cols = 2,
        panels = list(
          list(id = "comp1", type = "chart", row = 1, col = 1, width = 1, height = 2, title = "Variable A", chart_type = "violin"),
          list(id = "comp2", type = "chart", row = 1, col = 2, width = 1, height = 2, title = "Variable B", chart_type = "violin"),
          list(id = "comp3", type = "chart", row = 3, col = 1, width = 1, height = 2, title = "Distribution A", chart_type = "histogram"),
          list(id = "comp4", type = "chart", row = 3, col = 2, width = 1, height = 2, title = "Distribution B", chart_type = "histogram")
        )
      )
    ),
    
    timeseries = list(
      name = "Time Series Dashboard",
      description = "Temporal analysis and trends",
      layout = list(
        rows = 3,
        cols = 2,
        panels = list(
          list(id = "ts_main", type = "chart", row = 1, col = 1, width = 2, height = 2, title = "Main Trend", chart_type = "line"),
          list(id = "ts_seasonal", type = "chart", row = 3, col = 1, width = 1, height = 1, title = "Distribution", chart_type = "histogram"),
          list(id = "ts_detail", type = "chart", row = 3, col = 2, width = 1, height = 1, title = "Scatter", chart_type = "scatter")
        )
      )
    ),
    
    network = list(
      name = "Network Analysis",
      description = "Relationships and connections",
      layout = list(
        rows = 2,
        cols = 2,
        panels = list(
          list(id = "network_graph", type = "network", row = 1, col = 1, width = 2, height = 2, title = "Correlation Network"),
          list(id = "centrality", type = "chart", row = 3, col = 1, width = 1, height = 1, title = "Node Centrality", chart_type = "bar"),
          list(id = "metrics", type = "table", row = 3, col = 2, width = 1, height = 1, title = "Network Metrics")
        )
      )
    )
  )
  
  if (!template_name %in% names(templates)) {
    stop("Template not found. Available templates: ", paste(names(templates), collapse = ", "))
  }
  
  templates[[template_name]]
}

#' Save Dashboard Configuration
#'
#' Save current dashboard layout and settings to JSON
#'
#' @param config Dashboard configuration list
#' @param filepath Path to save the configuration
#' @export
save_dashboard_config <- function(config, filepath) {
  jsonlite::write_json(config, filepath, pretty = TRUE, auto_unbox = TRUE)
}

#' Load Dashboard Configuration
#'
#' Load dashboard layout from saved JSON file
#'
#' @param filepath Path to the configuration file
#' @return Dashboard configuration list
#' @export
load_dashboard_config <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("Configuration file not found: ", filepath)
  }
  jsonlite::read_json(filepath, simplifyVector = TRUE)
}

#' Export Dashboard as HTML
#'
#' Export the current dashboard as standalone HTML
#'
#' @param dashboard_data List containing all dashboard elements
#' @param output_file Output HTML file path
#' @param include_data Whether to embed data in HTML
#' @export
export_dashboard_html <- function(dashboard_data, output_file, include_data = FALSE) {
  
  # Create HTML template
  html_template <- '
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{{dashboard_title}}</title>
  <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { 
      font-family: "Inter", sans-serif;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      padding: 20px;
    }
    .dashboard-container {
      max-width: 1400px;
      margin: 0 auto;
      background: white;
      border-radius: 16px;
      padding: 32px;
      box-shadow: 0 20px 60px rgba(0,0,0,0.3);
    }
    .dashboard-header {
      margin-bottom: 32px;
      padding-bottom: 16px;
      border-bottom: 2px solid #e5e7eb;
    }
    .dashboard-title {
      font-size: 28px;
      font-weight: 700;
      color: #1f2937;
      margin-bottom: 8px;
    }
    .dashboard-subtitle {
      font-size: 14px;
      color: #6b7280;
    }
    .dashboard-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
      gap: 24px;
    }
    .dashboard-panel {
      background: #f9fafb;
      border-radius: 12px;
      padding: 20px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    .panel-title {
      font-size: 16px;
      font-weight: 600;
      color: #374151;
      margin-bottom: 16px;
    }
    .stat-value {
      font-size: 36px;
      font-weight: 700;
      color: #667eea;
      margin-bottom: 4px;
    }
    .stat-label {
      font-size: 12px;
      color: #9ca3af;
      text-transform: uppercase;
      letter-spacing: 0.05em;
    }
  </style>
</head>
<body>
  <div class="dashboard-container">
    <div class="dashboard-header">
      <div class="dashboard-title">{{dashboard_title}}</div>
      <div class="dashboard-subtitle">Generated on {{timestamp}}</div>
    </div>
    <div class="dashboard-grid">
      {{panels}}
    </div>
  </div>
</body>
</html>
'
  
  # Generate panel HTML
  panels_html <- ""
  for (panel in dashboard_data$panels) {
    if (panel$type == "chart") {
      panels_html <- paste0(panels_html, sprintf(
        '<div class="dashboard-panel"><div class="panel-title">%s</div><div id="%s"></div></div>',
        panel$title, panel$id
      ))
    } else if (panel$type == "stat") {
      panels_html <- paste0(panels_html, sprintf(
        '<div class="dashboard-panel"><div class="stat-value">%s</div><div class="stat-label">%s</div></div>',
        panel$value, panel$title
      ))
    }
  }
  
  # Replace placeholders
  html_content <- html_template
  html_content <- gsub("{{dashboard_title}}", dashboard_data$title, html_content)
  html_content <- gsub("{{timestamp}}", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), html_content)
  html_content <- gsub("{{panels}}", panels_html, html_content)
  
  # Write to file
  writeLines(html_content, output_file)
  
  message("Dashboard exported to: ", output_file)
  invisible(output_file)
}

#' Get Available Dashboard Templates
#'
#' Returns list of all available dashboard templates
#'
#' @export
get_dashboard_templates <- function() {
  c("executive", "analytical", "comparison", "timeseries", "network")
}
