# DataExplorerPro Server Logic
# AI-Powered Data Exploration Studio

library(DataExplorerPro)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(skimr)
library(DT)
library(jsonlite)
library(httr)
library(glue)
library(purrr)
library(stringr)
library(lubridate)
library(scales)
library(rlang)

# Functions used below are provided by the DataExplorerPro namespace.
# Do not source relative ../../R files here: that path is not valid after install.

# Helper function to check Ollama and get models
check_ollama_status <- function() {
  tryCatch({
    base_url <- getOption("DataExplorerPro.ollama_base_url", Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"))
    response <- httr::GET(
      paste0(base_url, "/api/tags"),
      httr::timeout(10)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "parsed")
      if (!is.null(result$models) && length(result$models) > 0) {
        models <- sapply(result$models, function(x) x$name)
        list(
          connected = TRUE,
          models = models,
          needs_pull = FALSE
        )
      } else {
        list(connected = TRUE, models = character(0), needs_pull = TRUE)
      }
    } else {
      list(connected = FALSE, models = character(0), needs_pull = FALSE)
    }
  }, error = function(e) {
    list(connected = FALSE, models = character(0), needs_pull = FALSE, error = e$message)
  })
}

# ============================================
# HELPER FUNCTIONS
# ============================================

# Empty plot helper to avoid plotly warnings when no traces are available
empty_plot <- function(msg = NULL, height = NULL) {
  p <- plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "markers",
                       marker = list(opacity = 0))
  if (!is.null(msg)) {
    p <- p %>% layout(
      annotations = list(list(text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                               showarrow = FALSE))
    )
  }
  if (!is.null(height)) {
    p$x$layout$height <- height
  }
  p
}

# Fullscreen-capable modal for charts
chart_modal <- function(title, ...) {
  modalDialog(
    title = title,
    ...,
    easyClose = TRUE,
    size = "l",
    class = "modal-fullscreen",
    footer = tagList(
      tags$button(type = "button", class = "btn-fullscreen", onclick = "toggleFullscreenModal()", "Fullscreen"),
      modalButton("Close")
    )
  )
}

# Check Ollama connection
check_ollama_connection <- function() {
  tryCatch({
    base_url <- getOption("DataExplorerPro.ollama_base_url", Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"))
    response <- httr::GET(paste0(base_url, "/api/tags"), httr::timeout(5))
    httr::status_code(response) == 200
  }, error = function(e) FALSE)
}

# Ollama Chat Function
ollama_chat <- function(message, history = NULL, model = "llama3.2", 
                       temperature = 0.7, max_tokens = 2000,
                       data_context = NULL) {
  
  base_url <- getOption("DataExplorerPro.ollama_base_url", Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"))
  request_timeout <- as.integer(getOption("DataExplorerPro.ollama_request_timeout", 300L))
  retry_times <- as.integer(getOption("DataExplorerPro.ollama_retry_times", 1L))
  max_tokens <- as.integer(max(100, min(10000, max_tokens %||% 2000)))

  # First check if Ollama is running by testing the API
  preflight_error <- tryCatch({
    test_response <- httr::GET(paste0(base_url, "/api/tags"), httr::timeout(5))
    if (httr::status_code(test_response) != 200) {
      return("Error: Ollama service is not running. Please start Ollama with 'ollama serve'.")
    }
    
    # Check available models
    models <- httr::content(test_response, as = "parsed")$models
    model_names <- if (is.null(models) || length(models) == 0) {
      character(0)
    } else {
      vapply(models, function(m) m$name %||% "", character(1))
    }
    
    if (!(model %in% model_names)) {
      available_models <- paste(model_names, collapse = ", ")
      return(paste("Error: Model '", model, "' not found. Available models: ", available_models, ". Please select a different model from the dropdown.", sep = ""))
    }
    NULL
  }, error = function(e) {
    "Error: Cannot connect to Ollama. Please ensure Ollama is installed and running with 'ollama serve'."
  })

  if (is.character(preflight_error) && nzchar(preflight_error)) {
    return(preflight_error)
  }
  
  # Build messages array
  messages <- list()
  
  # Build system prompt – include data context when available
  system_text <- paste(
    "You are DataExplorerPro, an AI assistant for data analysis in R.",
    "You help users explore data, create visualizations, and write R code.",
    "Be concise, helpful, and provide accurate R code when requested."
  )
  
  if (!is.null(data_context) && nzchar(data_context)) {
    system_text <- paste0(
      system_text,
      "\n\nThe user currently has a dataset loaded with the following details:\n",
      data_context,
      "\nUse this context to answer data-related questions accurately."
    )
  } else {
    system_text <- paste(system_text, "No dataset is currently loaded.")
  }
  
  # Add system prompt
  messages[[1]] <- list(role = "system", content = system_text)
  
  # Add history
  if (!is.null(history)) {
    for (h in history) {
      messages[[length(messages) + 1]] <- list(role = h$role, content = h$content)
    }
  }
  
  # Add current message
  messages[[length(messages) + 1]] <- list(role = "user", content = message)
  
  # Make API call
  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_tokens = max_tokens,
    stream = FALSE
  )
  
  tryCatch({
    response <- httr::RETRY(
      "POST",
      paste0(base_url, "/api/chat"),
      body = body,
      encode = "json",
      httr::timeout(request_timeout),
      times = retry_times,
      quiet = TRUE,
      pause_min = 1,
      pause_cap = 3,
      terminate_on = c(400, 401, 403, 404, 422)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "parsed")
      if (!is.null(result$message$content)) {
        return(result$message$content)
      } else {
        return("Error: Unexpected response format from Ollama API")
      }
    } else {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      return(paste("Error: Ollama API returned status", httr::status_code(response), "-", error_content))
    }
  }, error = function(e) {
    err <- conditionMessage(e)
    if (grepl("timeout|timed out", err, ignore.case = TRUE)) {
      return(paste0(
        "Error: Ollama request timed out after ", request_timeout, " seconds. ",
        "Try reducing Max Tokens, switching to a smaller/faster model, ",
        "or increasing options(DataExplorerPro.ollama_request_timeout = ...)."
      ))
    }
    return(paste("Error connecting to Ollama:", err))
  })
}

extract_json_payload <- function(text) {
  cleaned <- gsub("```json|```JSON|```", "", text)
  cleaned <- gsub("\r\n?", "\n", cleaned)
  cleaned <- trimws(cleaned)

  start_idx <- regexpr("\\{", cleaned, perl = TRUE)[1]
  end_matches <- gregexpr("\\}", cleaned, perl = TRUE)[[1]]
  end_idx <- if (length(end_matches) > 0) end_matches[length(end_matches)] else -1

  if (start_idx > 0 && end_idx > start_idx) {
    cleaned <- substr(cleaned, start_idx, end_idx)
  }

  cleaned
}

sanitize_triple_quoted_fields <- function(text) {
  out <- text

  repeat {
    m <- regexpr('"([A-Za-z0-9_]+)"\\s*:\\s*"""', out, perl = TRUE)
    if (m[1] == -1) break

    match_len <- attr(m, "match.length")
    prefix <- substr(out, m[1], m[1] + match_len - 1)
    key <- sub('^"([A-Za-z0-9_]+)".*$', "\\1", prefix, perl = TRUE)

    value_start <- m[1] + match_len
    rest <- substr(out, value_start, nchar(out))
    end_rel <- regexpr('"""', rest, perl = TRUE)[1]
    if (end_rel == -1) break

    value <- substr(rest, 1, end_rel - 1)
    replacement <- paste0('"', key, '": ', jsonlite::toJSON(value, auto_unbox = TRUE))

    before <- if (m[1] > 1) substr(out, 1, m[1] - 1) else ""
    after <- substr(rest, end_rel + 3, nchar(rest))
    out <- paste0(before, replacement, after)
  }

  out
}

extract_response_field <- function(text, field) {
  triple_pattern <- paste0('"', field, '"\\s*:\\s*"""([\\s\\S]*?)"""')
  triple_match <- regmatches(text, regexec(triple_pattern, text, perl = TRUE))[[1]]
  if (length(triple_match) >= 2) {
    return(trimws(triple_match[2]))
  }

  quoted_pattern <- paste0('"', field, '"\\s*:\\s*"((?:\\\\.|[^"\\\\])*)"')
  quoted_match <- regmatches(text, regexec(quoted_pattern, text, perl = TRUE))[[1]]
  if (length(quoted_match) >= 2) {
    return(tryCatch(
      jsonlite::fromJSON(paste0('"', quoted_match[2], '"')),
      error = function(e) {
        gsub("\\\\n", "\n", quoted_match[2], fixed = TRUE)
      }
    ))
  }

  NULL
}

extract_insights_field <- function(text) {
  if (grepl('"insights"\\s*:\\s*null', text, perl = TRUE)) {
    return(NULL)
  }

  arr_loc <- regexpr('"insights"\\s*:\\s*\\[[\\s\\S]*?\\]', text, perl = TRUE)
  if (arr_loc[1] > 0) {
    arr_text <- substr(text, arr_loc[1], arr_loc[1] + attr(arr_loc, "match.length") - 1)
    parsed <- tryCatch(
      jsonlite::fromJSON(paste0("{", arr_text, "}")),
      error = function(e) NULL
    )
    if (!is.null(parsed) && !is.null(parsed$insights)) {
      return(as.character(parsed$insights))
    }
  }

  single <- extract_response_field(text, "insights")
  if (is.null(single)) return(NULL)

  if (is.character(single) && length(single) == 1) {
    parts <- unlist(strsplit(single, "\n|;"))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    return(parts)
  }

  as.character(single)
}

parse_nlq_response <- function(response) {
  cleaned <- extract_json_payload(response)
  candidates <- unique(c(
    cleaned,
    sanitize_triple_quoted_fields(cleaned),
    gsub('"""', '"', cleaned, fixed = TRUE)
  ))

  for (candidate in candidates) {
    parsed <- tryCatch(jsonlite::fromJSON(candidate), error = function(e) NULL)
    if (!is.null(parsed)) {
      if (is.null(parsed$data_result)) parsed$data_result <- NULL
      if (is.null(parsed$insights)) parsed$insights <- NULL
      return(parsed)
    }
  }

  code <- extract_response_field(cleaned, "code")
  explanation <- extract_response_field(cleaned, "explanation")
  insights <- extract_insights_field(cleaned)

  if (!is.null(code) || !is.null(explanation) || !is.null(insights)) {
    return(list(
      code = if (!is.null(code) && nzchar(trimws(code))) code else "# Could not generate code automatically\n# Please try rephrasing your query",
      explanation = if (!is.null(explanation) && nzchar(trimws(explanation))) explanation else response,
      insights = insights,
      data_result = NULL
    ))
  }

  list(
    code = "# Could not generate code automatically\n# Please try rephrasing your query",
    explanation = response,
    insights = NULL,
    data_result = NULL
  )
}

# Process Natural Language Query
process_natural_language_query <- function(query, query_type, data, model, temperature = 0.7, max_tokens = 2000) {
  
  # Build compact data context – limit to first 30 columns for speed
  col_limit <- min(ncol(data), 30L)
  col_names <- names(data)
  if (length(col_names) > 30) col_names <- c(col_names[seq_len(30L)], paste0("... +", length(col_names) - 30L, " more"))
  data_context <- paste0(
    nrow(data), " rows, ", ncol(data), " cols. ",
    "Cols: ", paste(col_names, collapse = ", "), ". ",
    "Types: ", paste(paste0(names(data)[seq_len(col_limit)], "=", sapply(data[, seq_len(col_limit), drop = FALSE], function(x) class(x)[1])), collapse = ", ")
  )
  
  # Shared instruction to be brief and return JSON only
  json_instr <- "Reply ONLY with valid JSON (RFC8259), no markdown fences. Keys: code, explanation (1-2 sentences), insights (1-2 bullet points or null). Do NOT use triple quotes. 'code' must be a JSON string with escaped newlines (\\n)."
  
  # Build prompt based on query type
  prompt <- switch(query_type,
                   "transform" = paste0(
                     "R dplyr expert. Query: '", query, "'. Data: ", data_context, ". ",
                     json_instr, " 'code' = complete dplyr pipeline."
                   ),
                   "visualize" = paste0(
                     "R plotly/ggplot2 expert. Query: '", query, "'. Data: ", data_context, ". ",
                     json_instr, " 'code' = complete visualization code."
                   ),
                   "stats" = paste0(
                     "R statistics expert. Query: '", query, "'. Data: ", data_context, ". ",
                     json_instr, " 'code' = complete statistical code."
                   ),
                   paste0(
                     "R data analyst. Query: '", query, "'. Data: ", data_context, ". ",
                     json_instr, " 'code' = R code to answer the query."
                   )
  )
  
  # Call Ollama
  response <- ollama_chat(prompt, model = model, temperature = temperature, max_tokens = max_tokens)
  
  # Parse response (robust to malformed quasi-JSON from LLMs)
  parse_nlq_response(response)
}

# Format chat text for display
format_chat_text <- function(text) {
  htmltools::HTML(gsub("\n", "<br/>", htmltools::htmlEscape(text)))
}

# Format AI response text for display (improved formatting without asterisks)
format_ai_text <- function(text) {
  # Remove all asterisks
  text <- gsub("\\*", "", text)

  # Convert line breaks to HTML
  text <- gsub("\n", "<br/>", text)

  # Add paragraph breaks for better readability
  text <- gsub("(<br/>){2,}", "</p><p>", text)
  text <- paste0("<p>", text, "</p>")

  # Escape HTML entities
  htmltools::HTML(text)
}

clean_insight_lines <- function(text, min_chars = 6) {
  if (is.null(text) || !is.character(text)) return(character(0))
  lines <- unlist(strsplit(text, "\n"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- gsub("^\\u2022\\s*", "", lines)
  lines <- gsub("^[-*]\\s*", "", lines)
  lines <- gsub("^\\d+\\s*[\\).:-]\\s*", "", lines)
  lines <- lines[nchar(lines) >= min_chars]
  lines
}

render_insight_list <- function(lines) {
  tags$ul(
    class = "ai-insights-list",
    lapply(seq_along(lines), function(i) {
      tags$li(
        class = "ai-insight-item",
        tags$span(class = "ai-insight-bullet", sprintf("%02d", i)),
        tags$span(lines[[i]])
      )
    })
  )
}

render_insights_card <- function(title, lines, warning = FALSE, subtitle = NULL, empty_message = NULL) {
  card_class <- "ai-insights-card"
  if (isTRUE(warning)) {
    card_class <- paste(card_class, "ai-insights-warning")
  }
  content <- NULL
  if (length(lines) > 0) {
    content <- render_insight_list(lines)
  } else if (!is.null(empty_message)) {
    content <- div(class = "ai-insights-muted", empty_message)
  }
  div(
    class = card_class,
    div(class = "ai-insights-header", title),
    if (!is.null(subtitle)) div(class = "ai-insights-muted", subtitle),
    content
  )
}

render_insights_section <- function(label, lines) {
  if (length(lines) == 0) return(NULL)
  tagList(
    div(class = "ai-insights-muted", label),
    render_insight_list(lines)
  )
}

# Chart Creation Functions
create_scatter_plot <- function(data, x_var, y_var, color_var = NULL, size_var = NULL, trendline = FALSE) {
  has_size <- !is.null(size_var) && size_var != "" && is.numeric(data[[size_var]])
  if (!is.null(color_var) && color_var != "") {
    p <- plot_ly(
      data, x = ~get(x_var), y = ~get(y_var),
      color = ~get(color_var),
      size = if (has_size) ~get(size_var) else NULL,
      sizes = if (has_size) c(6, 18) else NULL,
      type = "scatter", mode = "markers",
      marker = list(size = if (!has_size) 10 else NULL),
      name = paste("Scatter:", y_var, "vs", x_var)
    )
  } else {
    p <- plot_ly(
      data, x = ~get(x_var), y = ~get(y_var),
      size = if (has_size) ~get(size_var) else NULL,
      sizes = if (has_size) c(6, 18) else NULL,
      type = "scatter", mode = "markers",
      marker = list(color = "#667eea", size = if (!has_size) 10 else NULL),
      name = paste("Scatter:", y_var, "vs", x_var)
    )
  }

  if (isTRUE(trendline) && is.numeric(data[[x_var]]) && is.numeric(data[[y_var]])) {
    fit <- stats::lm(data[[y_var]] ~ data[[x_var]])
    p <- p %>% add_lines(
      x = data[[x_var]],
      y = stats::fitted(fit),
      line = list(color = "#0f172a", dash = "dash"),
      name = "Trendline",
      inherit = FALSE
    )
  }

  p %>% layout(xaxis = list(title = x_var), yaxis = list(title = y_var), showlegend = TRUE)
}


create_box_plot <- function(data, x_var, y_var, color_var = NULL) {
  p <- plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = "box",
              name = paste("Box Plot:", y_var, "by", x_var))

  if (!is.null(color_var) && color_var != "") {
    p <- p %>% add_trace(boxpoints = "all", jitter = 0.3, pointpos = -1.8,
                        name = paste("Points:", y_var, "by", x_var))
  }

  p %>% layout(xaxis = list(title = x_var), yaxis = list(title = y_var), showlegend = TRUE)
}

create_violin_plot <- function(data, x_var, y_var, color_var = NULL) {
  p <- plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = "violin",
              box = list(visible = TRUE), meanline = list(visible = TRUE),
              name = paste("Violin:", y_var, "by", x_var))

  if (!is.null(color_var) && color_var != "") {
    p <- p %>% add_trace(split = ~get(color_var), name = paste("Split by", color_var))
  }

  p %>% layout(xaxis = list(title = x_var), yaxis = list(title = y_var), showlegend = TRUE)
}


create_radar_chart <- function(data) {
  numeric_data <- data[, sapply(data, is.numeric)]
  if (ncol(numeric_data) < 3) {
    return(empty_plot("Need at least 3 numeric variables for radar chart"))
  }
  agg_data <- numeric_data[seq_len(min(10, nrow(numeric_data))), ]
  plot_ly(type = "scatterpolar", r = as.numeric(agg_data[1, ]), 
          theta = names(numeric_data), mode = "lines+markers",
          fill = "toself", marker = list(color = "#667eea"))
}

# Generate Chart Code
generate_chart_code <- function(chart_type, x_var, y_var, 
                                color_var = NULL, size_var = NULL, facet_var = NULL) {
  value_var <- if (!is.null(size_var) && nzchar(size_var) && size_var != ".__value__") {
    size_var
  } else {
    ".__value__"
  }
  sunburst_code <- if (value_var == ".__value__") {
    glue::glue("data %>% dplyr::mutate(.__value__ = 1) %>% plot_ly(labels = ~{x_var}, parents = ~{y_var}, values = ~.__value__, type = 'sunburst')")
  } else {
    glue::glue("plot_ly(data, labels = ~{x_var}, parents = ~{y_var}, values = ~{value_var}, type = 'sunburst')")
  }
  treemap_code <- if (value_var == ".__value__") {
    glue::glue("data %>% dplyr::mutate(.__value__ = 1) %>% plot_ly(labels = ~{x_var}, parents = ~{y_var}, values = ~.__value__, type = 'treemap')")
  } else {
    glue::glue("plot_ly(data, labels = ~{x_var}, parents = ~{y_var}, values = ~{value_var}, type = 'treemap')")
  }
  base_code <- switch(chart_type,
                      "scatter" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, type = 'scatter', mode = 'markers')"),
                      "line" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, type = 'scatter', mode = 'lines')"),
                      "bar" = glue::glue("data %>% dplyr::group_by({x_var}) %>% dplyr::summarize(value = mean({y_var}, na.rm = TRUE)) %>% plot_ly(x = ~{x_var}, y = ~value, type = 'bar')"),
                      "histogram" = glue::glue("plot_ly(data, x = ~{x_var}, type = 'histogram')"),
                      "box" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, type = 'box')"),
                      "violin" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, type = 'violin', box = list(visible = TRUE))"),
                      "density" = glue::glue("plot_ly(data, x = ~{x_var}, type = 'histogram', histnorm = 'probability density')"),
                      "heatmap" = glue::glue("data %>% dplyr::group_by({x_var}, {y_var}) %>% dplyr::summarize(value = dplyr::n()) %>% plot_ly(x = ~{x_var}, y = ~{y_var}, z = ~value, type = 'heatmap')"),
                      "pie" = glue::glue("data %>% dplyr::count({x_var}) %>% plot_ly(labels = ~{x_var}, values = ~n, type = 'pie')"),
                      "violin_box" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE))"),
                      "beeswarm" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, type = 'scatter', mode = 'markers', marker = list(size = 8, opacity = 0.7))"),
                      "dumbbell" = glue::glue("# Dumbbell: compare {y_var} vs {size_var} by {x_var}\ncreate_dumbbell_plot(data, '{x_var}', '{y_var}', '{size_var}')"),
                      "forest" = glue::glue("# Forest plot: effect={y_var}, lower_ci={size_var}, upper_ci (group)\ncreate_forest_plot(data, '{x_var}', '{y_var}', '{size_var}', '<upper_ci_col>')"),
                      "bland_altman" = glue::glue("# Bland-Altman agreement plot\ncreate_bland_altman_plot(data, '{x_var}', '{y_var}')"),
                      "stacked_area" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, type = 'scatter', mode = 'lines', stackgroup = 'one')"),
                      "bubble" = glue::glue("plot_ly(data, x = ~{x_var}, y = ~{y_var}, size = ~{size_var}, type = 'scatter', mode = 'markers')"),
                      "treemap" = treemap_code,
                      "sunburst" = sunburst_code,
                      "parallel" = "plot_ly(type = 'parcoords', dimensions = lapply(names(data[sapply(data, is.numeric)]), function(col) list(label = col, values = data[[col]])), data = data)",
                      "radar" = "plot_ly(type = 'scatterpolar', r = as.numeric(data[1, sapply(data, is.numeric)]), theta = names(data[sapply(data, is.numeric)]), mode = 'lines+markers', fill = 'toself')",
                      "# Chart code not available for this type"
  )
  
  if (!is.null(color_var) && color_var != "") {
    base_code <- paste0(base_code, " %>% add_trace(marker = list(color = ~", color_var, "))")
  }
  
  base_code <- paste0(base_code, " %>% layout(title = '", chart_type, " plot')")
  
  return(base_code)
}

# Generate Chart Insights
generate_chart_insights <- function(data, x_var, y_var, chart_type, model = "llama3.2") {
  if (!check_ollama_connection()) {
    return(render_insights_card(
      "AI Insights",
      character(0),
      warning = TRUE,
      subtitle = "AI insights are unavailable: Ollama is not reachable."
    ))
  }

  status <- check_ollama_status()
  if (!is.null(status$models) && length(status$models) > 0 && !(model %in% status$models)) {
    return(render_insights_card(
      "AI Insights",
      character(0),
      warning = TRUE,
      subtitle = paste0("AI insights skipped: model '", model, "' is not available.")
    ))
  }

  # Use Ollama to generate insights
  prompt <- paste0(
    "Generate 3 brief insights about a ", chart_type, " plot of '", y_var, "' by '", x_var, "'. ",
    "Data has ", nrow(data), " rows. ",
    "Return as bullet points only."
  )
  
  insights <- ollama_chat(prompt, model = model)
  insights_text <- if (is.character(insights)) paste(insights, collapse = "\n") else ""
  has_err <- is.character(insights) && any(grepl("^Error:", insights), na.rm = TRUE)
  has_timeout <- is.character(insights) && any(grepl("timeout", insights, ignore.case = TRUE), na.rm = TRUE)

  if (is.null(insights) || !is.character(insights) || has_err || has_timeout) {
    return(render_insights_card(
      "AI Insights",
      character(0),
      warning = TRUE,
      subtitle = "AI insights are temporarily unavailable. Check the Ollama connection or try again."
    ))
  }

  insight_lines <- clean_insight_lines(insights_text)
  if (length(insight_lines) == 0) {
    return(render_insights_card(
      "AI Insights",
      character(0),
      empty_message = "AI generated insights are being processed..."
    ))
  }
  render_insights_card("AI Insights", insight_lines)
}

# Generate AI Insights for EDA
generate_ai_insights <- function(data, model = "llama3.2") {
  data_context <- paste0(
    nrow(data), " rows, ", ncol(data), " columns. ",
    "Columns: ", paste(names(data), collapse = ", "), ". ",
    "Types: ", paste(paste0(names(data), "=", sapply(data, function(x) class(x)[1])), collapse = ", ")
  )

  prompt <- paste0(
    "Generate 5 key insights about this dataset. ",
    "Return as numbered bullet points focusing on patterns, anomalies, and recommendations."
  )

  insights <- ollama_chat(prompt, model = model, data_context = data_context)

  # More specific error detection - only treat as error if it starts with "Error:" or is NULL
  if (is.null(insights) ||
      (is.character(insights) && grepl("^Error:", insights) && !grepl("^[0-9]", insights)) ||
      (is.character(insights) && grepl("dataset loaded|provide the dataset|don't have a specific dataset", insights, ignore.case = TRUE))) {

    # Extract error details for better user guidance
    error_msg <- if (grepl("not running", insights)) {
      "Ollama service is not running"
    } else if (grepl("Model.*not found", insights)) {
      "Selected AI model is not available"
    } else if (grepl("Cannot connect", insights)) {
      "Cannot connect to Ollama service"
    } else if (grepl("API returned status", insights)) {
      "Ollama API error"
    } else {
      paste("Unexpected response:", substr(insights, 1, 100))
    }

    troubleshoot_steps <- c(
      "Check Ollama is running: ollama list",
      "Select an available model from the dropdown above",
      "Pull additional models: ollama pull <model-name>",
      "Restart Ollama: ollama serve"
    )
    dataset_insights <- c(
      paste("Dataset contains", nrow(data), "rows and", ncol(data), "columns"),
      paste("Numeric columns:", sum(sapply(data, is.numeric))),
      paste("Categorical columns:", sum(sapply(data, function(x) is.factor(x) || is.character(x)))),
      paste("Missing data:", round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 1), "%")
    )

    return(div(
      class = "ai-insights-card ai-insights-warning",
      div(class = "ai-insights-header", "AI Insights Unavailable"),
      div(class = "ai-insights-muted", error_msg),
      render_insights_section("To troubleshoot:", troubleshoot_steps),
      render_insights_section("Basic dataset insights:", dataset_insights)
    ))
  }

  # If we get here, we have a response - treat it as successful
  # Process successful insights
  insight_lines <- clean_insight_lines(insights, min_chars = 4)

  if (length(insight_lines) == 0) {
    return(render_insights_card(
      "AI Insights",
      character(0),
      empty_message = "AI generated insights are being processed..."
    ))
  }

  render_insights_card("AI Insights", insight_lines)
}

build_context_aware_sample_questions <- function(data = NULL) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(c(
      "Load a dataset to get context-aware sample questions..." = ""
    ))
  }

  cols <- names(data)
  numeric_cols <- cols[vapply(data, is.numeric, logical(1))]
  categorical_cols <- cols[vapply(
    data,
    function(x) is.factor(x) || is.character(x) || is.logical(x),
    logical(1)
  )]
  datetime_cols <- cols[vapply(
    data,
    function(x) inherits(x, c("Date", "POSIXct", "POSIXt")),
    logical(1)
  )]

  if (length(datetime_cols) == 0) {
    datetime_cols <- cols[grepl("date|time|month|year", cols, ignore.case = TRUE)]
  }

  id_like <- grepl("(^id$|_id$|^index$|uuid|key)", cols, ignore.case = TRUE)
  measure_candidates <- setdiff(numeric_cols, cols[id_like])
  if (length(measure_candidates) == 0) measure_candidates <- numeric_cols

  primary_metric <- measure_candidates[1] %||% cols[1]
  secondary_metric <- if (length(measure_candidates) >= 2) measure_candidates[2] else primary_metric
  primary_group <- categorical_cols[1] %||% cols[1]
  time_col <- datetime_cols[1] %||% cols[1]

  target_candidates <- cols[grepl(
    "target|label|class|outcome|churn|status|response|convert|purchase|fraud|default",
    cols,
    ignore.case = TRUE
  )]
  target_col <- target_candidates[1] %||% NULL

  choices <- c(
    "Select a sample question..." = "",
    "1) Dataset quality audit" = paste0(
      "Audit my dataset quality for `", primary_metric,
      "`: summarize missingness by column, duplicate rows, impossible values, and recommend a concrete cleaning plan with R code."
    ),
    "2) Segment comparison" = paste0(
      "Compare `", primary_metric, "` across `", primary_group,
      "` with effect sizes and significance tests, then explain which groups are materially different."
    ),
    "3) Relationship deep dive" = paste0(
      "Analyze the relationship between `", primary_metric, "` and `", secondary_metric,
      "` using correlation, scatter visualization, and a simple model; summarize actionable insights."
    ),
    "4) Outlier and anomaly detection" = paste0(
      "Detect outliers in `", primary_metric,
      "` using IQR and robust z-scores, list flagged records, and provide a safe treatment strategy with auditable R code."
    ),
    "5) Feature engineering starter" = paste0(
      "Create an end-to-end feature engineering pipeline using key columns including `", primary_metric,
      "`, with type fixes, derived features, and validation checks."
    )
  )

  if (length(datetime_cols) > 0 && length(measure_candidates) > 0) {
    choices <- c(
      choices,
      "6) Time trend + seasonality" = paste0(
        "Build a time-series analysis of `", primary_metric, "` over `", time_col,
        "`, decompose trend/seasonality, detect anomalies, and forecast the next 12 periods."
      )
    )
  }

  if (!is.null(target_col) && length(measure_candidates) > 0) {
    choices <- c(
      choices,
      "7) Predictive model baseline" = paste0(
        "Build a baseline predictive model for `", target_col,
        "`, evaluate with appropriate metrics, and show the top drivers with practical interpretation."
      )
    )
  }

  choices
}

# Server Definition
server <- function(input, output, session) {
  # Safety: ensure HTTR requests always time out and won't hang the session
  httr::set_config(httr::timeout(getOption("DataExplorerPro.http_timeout", 300L)))
  options(curl_interrupt = TRUE)
  options(error = NULL)
  
  # Global reactive values
  global_data <- reactiveValues(
    data = NULL,
    original_data = NULL,
    transformed_data = NULL,
    query_history = list(),
    query_code = NULL,
    query_type = NULL,
    query_result = NULL,
    chat_history = list(),
    charts = list()
  )
  eda_cache <- reactiveValues(last_key = NULL)
  chart_cache <- reactiveValues(last_key = NULL)
  settings <- reactiveValues(
    app_theme = "light",
    compact_layout = FALSE,
    temperature = 0.7,
    max_tokens = 2000,
    default_chart_theme = "default",
    default_point_size = 8,
    default_opacity = 0.7,
    perf_sample_threshold = 50000,
    perf_sample_size = 50000,
    perf_auto_sample = TRUE,
    ollama_model = NULL
  )

  observeEvent(TRUE, {
    session$sendCustomMessage("setCompact", settings$compact_layout)
    updateSelectInput(session, "chart_theme", selected = settings$default_chart_theme)
    updateNumericInput(session, "point_size", value = settings$default_point_size)
    updateNumericInput(session, "opacity", value = settings$default_opacity)
    updateNumericInput(session, "temperature", value = settings$temperature)
    updateNumericInput(session, "max_tokens", value = settings$max_tokens)
    updateSelectInput(
      session,
      "sample_nlp_query",
      choices = build_context_aware_sample_questions(global_data$data),
      selected = ""
    )
  }, once = TRUE)

  # Keep server-side theme setting synchronized with header theme switcher/localStorage
  observeEvent(input$app_theme, {
    req(!is.null(input$app_theme), nzchar(input$app_theme))
    settings$app_theme <- input$app_theme
    session$sendCustomMessage("setTheme", input$app_theme)
  }, ignoreInit = FALSE)

  # Performance and profiling
  perf_stats <- reactiveVal(list())
  
  # Ollama API Configuration
  ollama_config <- reactiveValues(
    base_url = "http://localhost:11434",
    model = "llama3.2",
    connected = FALSE,
    available_models = character(0),
    needs_model_pull = FALSE
  )
  
  # Initialize Ollama connection and check for models
  observe({
    status <- check_ollama_status()
    ollama_config$connected <- status$connected
    ollama_config$available_models <- status$models
    ollama_config$needs_model_pull <- status$needs_pull
    
    if (status$connected) {
      if (status$needs_pull) {
        # Ollama is running but no models installed
        output$ollama_status <- renderUI({
          div(
            span(class = "badge bg-warning", "No Models Found"),
            tags$small(style = "display: block; margin-top: 5px; color: #666;",
                      "Click to install a model")
          )
        })
      } else {
        # Models available
        output$ollama_status <- renderUI({
          span(class = "badge bg-success", 
               paste("Ollama Ready (", length(status$models), "models)"))
        })
      }
    } else {
      # Ollama not running
      output$ollama_status <- renderUI({
        div(
          span(class = "badge bg-danger", "Ollama Not Running"),
          tags$small(style = "display: block; margin-top: 5px; color: #666;",
                    "Start Ollama to enable AI features")
        )
      })
    }
  })
  
  # Update Ollama model selection dynamically
  observe({
    if (length(ollama_config$available_models) > 0) {
      current_model <- input$ollama_model
      preferred_model <- ollama_config$model
      selected_model <- if (!is.null(current_model) && current_model %in% ollama_config$available_models) {
        current_model
      } else if (!is.null(preferred_model) && preferred_model %in% ollama_config$available_models) {
        preferred_model
      } else {
        ollama_config$available_models[1]
      }
      updateSelectInput(session, "ollama_model",
                       choices = ollama_config$available_models,
                       selected = selected_model)
    }
  })

  # Persist user-selected model
  observeEvent(input$ollama_model, {
    if (!is.null(input$ollama_model) && input$ollama_model != "") {
      ollama_config$model <- input$ollama_model
    }
  }, ignoreInit = TRUE)
  
  # Show model installation modal when clicked
  observeEvent(input$ollama_status_click, {
    showModal(modalDialog(
      title = tags$h4(tags$i(class = "fas fa-robot"), " Ollama Setup"),
      
      if (!ollama_config$connected) {
        div(
          h5("Ollama is not running or not installed."),
          tags$hr(),
          h5("Installation Instructions:"),
          tags$ol(
            tags$li("Download Ollama from: ", 
                   tags$a(href = "https://ollama.com", target = "_blank", "https://ollama.com")),
            tags$li("Run: ", code("ollama serve")),
            tags$li("Then pull a model: ", code("ollama pull llama3.2"))
          ),
          tags$div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                  tags$code("curl -fsSL https://ollama.ai/install.sh | sh"))
        )
      } else if (ollama_config$needs_model_pull) {
        div(
          h5("Ollama is running but no models are installed."),
          tags$hr(),
          h5("Available Models to Install:"),
          tags$ul(
            tags$li(tags$strong("llama3.2"), " - General purpose, best for most tasks"),
            tags$li(tags$strong("codellama") , " - Optimized for code generation"),
            tags$li(tags$strong("mistral") , " - Fast and efficient"),
            tags$li(tags$strong("qwen2.5") , " - Good multilingual support")
          ),
          h5("Install a model:"),
          tags$div(style = "background: #e8f4fd; padding: 15px; border-radius: 8px;",
                  tags$code("ollama pull llama3.2")),
          hr(),
          actionButton("pull_model_btn", "Pull llama3.2 Now", 
                      icon = icon("download"), class = "btn-primary-custom"),
          hr(),
          textOutput("pull_status")
        )
      } else {
        div(
          h5("Ollama is connected with models available!"),
          tags$hr(),
          h5("Installed Models:"),
          tags$ul(lapply(ollama_config$available_models, tags$li)),
          hr(),
          selectInput("selected_model", "Select Model:",
                     choices = ollama_config$available_models,
                     selected = ollama_config$available_models[1]),
          actionButton("refresh_models", "Refresh Model List", 
                      icon = icon("refresh"), class = "btn-outline-secondary")
        )
      },
      
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Pull model button handler
  observeEvent(input$pull_model_btn, {
    output$pull_status <- renderText("Pulling model... This may take a few minutes.")
    
    # Run pull command in background
    observe({
      tryCatch({
        result <- system("ollama pull llama3.2", intern = TRUE, wait = TRUE)
        output$pull_status <- renderText("Model pulled successfully! Refreshing...")
        
        # Refresh Ollama status
        status <- check_ollama_status()
        ollama_config$connected <- status$connected
        ollama_config$available_models <- status$models
        ollama_config$needs_model_pull <- status$needs_pull
        
        # Close modal and show success
        removeModal()
        showNotification("Model installed successfully! AI features are now enabled.", 
                        type = "message")
      }, error = function(e) {
        output$pull_status <- renderText(paste("Error:", e$message))
      })
    })
  })
  
  # Refresh models handler
  observeEvent(input$refresh_models, {
    status <- check_ollama_status()
    ollama_config$connected <- status$connected
    ollama_config$available_models <- status$models
    showNotification("Model list refreshed", type = "message")
  })
  
  # Settings modal
  observeEvent(input$settings_btn, {
    showModal(modalDialog(
      title = tagList(icon("cog"), "Settings"),
      size = "m",
      
      tabsetPanel(
        tabPanel("Appearance",
          div(class = "sidebar-section",
            div(class = "section-title", tags$i(class = "fas fa-palette"), "Theme"),
            selectInput("app_theme", "Application Theme:",
              choices = c(
                "Cobalt" = "cobalt",
                "Dark" = "dark",
                "Light (Default)" = "light", 
                "Solarize" = "solarize",
                "Blue" = "blue",
                "Green" = "green",
                "Purple" = "purple",
                "Orange" = "orange",
                "Red" = "red",
                "Monochrome" = "monochrome"
              ),
              selected = settings$app_theme
            ),
            checkboxInput("compact_layout", "Compact layout (reduce spacing)", value = settings$compact_layout),
            helpText("Choose a theme that works best for your environment.")
          )
        ),
        
        tabPanel("AI Settings",
          div(class = "sidebar-section",
            div(class = "section-title", tags$i(class = "fas fa-robot"), "Ollama Configuration"),
            selectInput("settings_ollama_model", "Default Ollama Model:",
              choices = if(length(ollama_config$available_models) > 0) ollama_config$available_models else c("No models available"),
              selected = if (length(ollama_config$available_models) > 0) {
                preferred <- settings$ollama_model %||% ollama_config$model
                if (!is.null(preferred) && preferred %in% ollama_config$available_models) {
                  preferred
                } else {
                  ollama_config$available_models[1]
                }
              } else NULL
            ),
            sliderInput("settings_temperature", "Default Temperature:", 
              min = 0, max = 1, value = settings$temperature, step = 0.1),
            numericInput("settings_max_tokens", "Default Max Tokens:", 
              value = settings$max_tokens, min = 100, max = 10000),
            hr(),
            actionButton("refresh_ollama_settings", "Refresh Models", 
              icon = icon("refresh"), class = "btn-outline-secondary")
          )
        ),
        
        tabPanel("Chart Defaults",
          div(class = "sidebar-section",
            div(class = "section-title", tags$i(class = "fas fa-chart-bar"), "Default Chart Settings"),
            selectInput("default_chart_theme", "Chart Theme:",
              choices = c("default", "minimal", "classic", "modern", "dark", "light"),
              selected = settings$default_chart_theme
            ),
            sliderInput("default_point_size", "Default Point Size:", 
              min = 1, max = 10, value = settings$default_point_size, step = 1),
            sliderInput("default_opacity", "Default Opacity:", 
              min = 0.1, max = 1, value = settings$default_opacity, step = 0.1)
          )
        ),
        tabPanel("Performance",
          div(class = "sidebar-section",
            div(class = "section-title", tags$i(class = "fas fa-bolt"), "Performance"),
            numericInput("perf_sample_threshold", "Auto-sample threshold (rows):", value = settings$perf_sample_threshold, min = 1000, step = 1000),
            numericInput("perf_sample_size", "Sample size when sampling:", value = settings$perf_sample_size, min = 1000, step = 1000),
            checkboxInput("perf_auto_sample", "Enable auto-sampling for large datasets", value = settings$perf_auto_sample),
            helpText("When enabled, EDA and visualizations will use a sampled subset for speed."),
            hr(),
            actionButton("run_profiler", "Profile EDA (current dataset)", class = "btn-secondary btn-block"),
            uiOutput("perf_status")
          )
        )
      ),
      
      easyClose = TRUE,
      footer = tagList(
        actionButton("save_settings", "Save Settings", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Save settings
  observeEvent(input$save_settings, {
    # Save theme setting
    if (!is.null(input$app_theme)) {
      settings$app_theme <- input$app_theme
      session$sendCustomMessage("setTheme", input$app_theme)
    }
    # Compact layout setting
    if (!is.null(input$compact_layout)) {
      settings$compact_layout <- as.logical(input$compact_layout)
      session$sendCustomMessage("setCompact", as.logical(input$compact_layout))
    }
    
    # Update default Ollama settings
    if (!is.null(input$settings_ollama_model)) {
      settings$ollama_model <- input$settings_ollama_model
      ollama_config$model <- input$settings_ollama_model
      updateSelectInput(session, "ollama_model", selected = input$settings_ollama_model)
    }
    if (!is.null(input$settings_temperature)) {
      settings$temperature <- input$settings_temperature
      updateSliderInput(session, "temperature", value = input$settings_temperature)
    }
    if (!is.null(input$settings_max_tokens)) {
      settings$max_tokens <- input$settings_max_tokens
      updateNumericInput(session, "max_tokens", value = input$settings_max_tokens)
    }

    if (!is.null(input$default_chart_theme)) {
      settings$default_chart_theme <- input$default_chart_theme
      updateSelectInput(session, "chart_theme", selected = input$default_chart_theme)
    }
    if (!is.null(input$default_point_size)) {
      settings$default_point_size <- input$default_point_size
      updateNumericInput(session, "point_size", value = input$default_point_size)
    }
    if (!is.null(input$default_opacity)) {
      settings$default_opacity <- input$default_opacity
      updateNumericInput(session, "opacity", value = input$default_opacity)
    }
    if (!is.null(input$perf_sample_threshold)) {
      settings$perf_sample_threshold <- input$perf_sample_threshold
    }
    if (!is.null(input$perf_sample_size)) {
      settings$perf_sample_size <- input$perf_sample_size
    }
    if (!is.null(input$perf_auto_sample)) {
      settings$perf_auto_sample <- as.logical(input$perf_auto_sample)
    }
    
    showNotification("Settings saved successfully!", type = "message")
    removeModal()
  })
  
  # Refresh Ollama models in settings
  observeEvent(input$refresh_ollama_settings, {
    status <- check_ollama_status()
    ollama_config$connected <- status$connected
    ollama_config$available_models <- status$models
    
    if (length(status$models) > 0) {
      selected_model <- if (!is.null(ollama_config$model) && ollama_config$model %in% status$models) {
        ollama_config$model
      } else {
        status$models[1]
      }
      updateSelectInput(session, "settings_ollama_model",
                       choices = status$models,
                       selected = selected_model)
      showNotification("Models refreshed!", type = "message")
    } else {
      showNotification("No models found. Make sure Ollama is running.", type = "warning")
    }
  })

  # Run profiler (user-initiated)
  observeEvent(input$run_profiler, {
    req(global_data$data)
    showNotification("Profiling EDA...", type = "message")
    tryCatch({
      generate_eda_report()
      ps <- perf_stats()
      showModal(modalDialog(
        title = "EDA Profile Results",
        p(paste0("EDA execution time: ", round(ps$eda_time, 2), " seconds")),
        if (!is.null(ps$sampled) && ps$sampled) p(paste0("Sample used: ", ps$sample_n, " of ", ps$original_n)),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Profiling failed:", e$message), type = "error")
    })
  })

  # Show keyboard shortcuts modal
  observeEvent(input$show_shortcuts, {
    # Read shortcuts file if available
    shortcuts_path <- system.file("docs/SHORTCUTS.md", package = "DataExplorerPro")
    if (shortcuts_path == "") {
      # Local dev fallback
      shortcuts_path <- "docs/SHORTCUTS.md"
    }
    content <- tryCatch({
      txt <- readLines(shortcuts_path)
      paste(txt, collapse = "\n")
    }, error = function(e) "Ctrl/Cmd + Enter: Send message in AI Chat\nShift + Enter: Insert newline in AI Chat input")

    showModal(modalDialog(
      title = "Keyboard Shortcuts",
      pre(content, style = "white-space: pre-wrap; font-family: monospace;"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Show comprehensive help modal
  observeEvent(input$show_help, {
    help_content <- tagList(
      # Tab navigation
      div(class = "help-tabs", style = "display: flex; gap: 8px; margin-bottom: 20px; flex-wrap: wrap;",
        actionButton("help_tab_overview", "Overview", class = "btn btn-sm btn-primary", style = "border-radius: 20px;"),
        actionButton("help_tab_getting_started", "Getting Started", class = "btn btn-sm btn-secondary", style = "border-radius: 20px;"),
        actionButton("help_tab_visualization", "Visualization", class = "btn btn-sm btn-secondary", style = "border-radius: 20px;"),
        actionButton("help_tab_ai", "AI Features", class = "btn btn-sm btn-secondary", style = "border-radius: 20px;"),
        actionButton("help_tab_shortcuts", "Shortcuts", class = "btn btn-sm btn-secondary", style = "border-radius: 20px;")
      ),
      # Tab content
      uiOutput("help_content_panel")
    )
    
    showModal(modalDialog(
      title = tagList(icon("question-circle"), "Help & Documentation"),
      help_content,
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    # Default to overview tab
    output$help_content_panel <- renderUI({
      tagList(
        h4("DataExplorerPro Overview"),
        p("DataExplorerPro is an AI-powered data exploration studio for RStudio. It combines interactive visualizations with local AI capabilities via Ollama."),
        tags$hr(),
        h5("Key Features:"),
        tags$ul(
          tags$li(strong("50+ Chart Types"), " - From basic to advanced statistical visualizations"),
          tags$li(strong("Natural Language Queries"), " - Ask questions about your data in plain English"),
          tags$li(strong("Auto EDA"), " - One-click comprehensive exploratory data analysis"),
          tags$li(strong("AI Chat"), " - Chat with AI about your data and get insights"),
          tags$li(strong("8 Themes"), " - Dark, Light, Blue, Green, Purple, Orange, Red, Monochrome"),
          tags$li(strong("Privacy-First"), " - All AI processing happens locally via Ollama")
        ),
        tags$hr(),
        h5("Quick Tips:"),
        tags$ul(
          tags$li("Click the", tags$em("info icon"), "in the header for a guided tour"),
          tags$li("Use the", tags$em("theme dropdown"), "to switch visual themes"),
          tags$li("The", tags$em("status badge"), "shows Ollama connection status"),
          tags$li("Click the", tags$em("gear icon"), "for settings")
        )
      )
    })
  })
  
  # Help tab handlers
  observeEvent(input$help_tab_overview, {
    output$help_content_panel <- renderUI({
      tagList(
        h4("DataExplorerPro Overview"),
        p("DataExplorerPro is an AI-powered data exploration studio for RStudio. It combines interactive visualizations with local AI capabilities via Ollama."),
        tags$hr(),
        h5("Key Features:"),
        tags$ul(
          tags$li(strong("50+ Chart Types"), " - From basic to advanced statistical visualizations"),
          tags$li(strong("Natural Language Queries"), " - Ask questions about your data in plain English"),
          tags$li(strong("Auto EDA"), " - One-click comprehensive exploratory data analysis"),
          tags$li(strong("AI Chat"), " - Chat with AI about your data and get insights"),
          tags$li(strong("8 Themes"), " - Dark, Light, Blue, Green, Purple, Orange, Red, Monochrome"),
          tags$li(strong("Privacy-First"), " - All AI processing happens locally via Ollama")
        ),
        tags$hr(),
        h5("Quick Tips:"),
        tags$ul(
          tags$li("Click the", tags$em("info icon"), "in the header for a guided tour"),
          tags$li("Use the", tags$em("theme dropdown"), "to switch visual themes"),
          tags$li("The", tags$em("status badge"), "shows Ollama connection status"),
          tags$li("Click the", tags$em("gear icon"), "for settings")
        )
      )
    })
  })
  
  observeEvent(input$help_tab_getting_started, {
    output$help_content_panel <- renderUI({
      tagList(
        h4("Getting Started"),
        tags$ol(
          tags$li(
            strong("Install Ollama"),
            tags$ul(
              tags$li("Download from", tags$a(href = "https://ollama.com", "ollama.com")),
              tags$li("Pull a model:", tags$code("ollama pull llama3.2"))
            )
          ),
          tags$li(
            strong("Load Your Data"),
            tags$ul(
              tags$li("Upload CSV, Excel, RDS, or Parquet files"),
              tags$li("Or select a built-in sample dataset"),
              tags$li("Click 'Load Dataset'")
            )
          ),
          tags$li(
            strong("Explore Your Data"),
            tags$ul(
              tags$li("View data preview and variable list in sidebar"),
              tags$li("Click 'Auto EDA' for comprehensive analysis"),
              tags$li("Use 'Visualization' tab to create charts")
            )
          ),
          tags$li(
            strong("Use AI Features"),
            tags$ul(
              tags$li("Type natural language queries in 'Ask AI' tab"),
              tags$li("Chat with AI about your data in 'AI Chat' tab"),
              tags$li("Get AI-generated insights in EDA reports")
            )
          )
        ),
        tags$hr(),
        h5("Example Natural Language Queries:"),
        tags$ul(
          tags$li(tags$code("What is the average mpg by cylinder?")),
          tags$li(tags$code("Show me a histogram of price")),
          tags$li(tags$code("Which variables are most correlated?")),
          tags$li(tags$code("Find outliers in the sales column"))
        )
      )
    })
  })
  
  observeEvent(input$help_tab_visualization, {
    output$help_content_panel <- renderUI({
      tagList(
        h4("Visualization Guide"),
        h5("Basic Charts"),
        tags$table(class = "table table-striped", style = "font-size: 13px;",
          tags$thead(tags$tr(tags$th("Chart"), tags$th("Best For"))),
          tags$tbody(
            tags$tr(tags$td("Scatter Plot"), tags$td("Relationships between two continuous variables")),
            tags$tr(tags$td("Line Chart"), tags$td("Time series, trends over time")),
            tags$tr(tags$td("Bar Chart"), tags$td("Comparing categorical values")),
            tags$tr(tags$td("Histogram"), tags$td("Distribution of a single variable")),
            tags$tr(tags$td("Box Plot"), tags$td("Outliers, quartiles, spread")),
            tags$tr(tags$td("Violin Plot"), tags$td("Distribution shape + density")),
            tags$tr(tags$td("Heatmap"), tags$td("Correlations, 2D matrices"))
          )
        ),
        tags$hr(),
        h5("Advanced Charts"),
        tags$ul(
          tags$li(strong("Raincloud Plot"), " - Publication-quality distribution visualization"),
          tags$li(strong("Ridgeline Plot"), " - Compare multiple distributions"),
          tags$li(strong("Forest Plot"), " - Meta-analysis, effect sizes with CI"),
          tags$li(strong("Volcano Plot"), " - Genomics, differential analysis"),
          tags$li(strong("Sankey Diagram"), " - Flow visualization"),
          tags$li(strong("Network Graph"), " - Relationship visualization")
        ),
        tags$hr(),
        h5("Tips:"),
        tags$ul(
          tags$li("Use the expand button to view charts in fullscreen"),
          tags$li("Hover over charts for interactive tooltips"),
          tags$li("Export charts as HTML, PNG, or JSON"),
          tags$li("Use 'Color' and 'Facet' options for multivariate analysis")
        )
      )
    })
  })
  
  observeEvent(input$help_tab_ai, {
    output$help_content_panel <- renderUI({
      tagList(
        h4("AI Features"),
        h5("Natural Language Queries"),
        p("Ask questions about your data in plain English. The AI will:"),
        tags$ol(
          tags$li("Interpret your question"),
          tags$li("Generate appropriate R/dplyr code"),
          tags$li("Execute the analysis"),
          tags$li("Present results with visualizations")
        ),
        tags$hr(),
        h5("Supported Ollama Models"),
        tags$table(class = "table table-striped", style = "font-size: 13px;",
          tags$thead(tags$tr(tags$th("Model"), tags$th("Best For"), tags$th("Speed"))),
          tags$tbody(
            tags$tr(tags$td("llama3.2"), tags$td("General queries, code generation"), tags$td("Fast")),
            tags$tr(tags$td("mistral"), tags$td("Balanced performance"), tags$td("Medium")),
            tags$tr(tags$td("codellama"), tags$td("Code-focused tasks"), tags$td("Medium")),
            tags$tr(tags$td("qwen2.5"), tags$td("Multilingual support"), tags$td("Medium"))
          )
        ),
        tags$hr(),
        h5("AI Configuration"),
        p("Access AI settings via", strong("Settings (gear icon) → AI Settings")),
        tags$ul(
          tags$li(strong("Model Selection"), " - Choose from detected Ollama models"),
          tags$li(strong("Temperature"), " - Control response creativity (0.0-2.0)"),
          tags$li(strong("Max Tokens"), " - Maximum response length")
        ),
        tags$hr(),
        h5("Privacy Note"),
        tags$div(class = "alert alert-info", style = "padding: 10px; border-radius: 8px;",
          tags$i(class = "fas fa-shield-alt"),
          " All AI processing happens locally via Ollama. Your data never leaves your machine."
        )
      )
    })
  })
  
  observeEvent(input$help_tab_shortcuts, {
    output$help_content_panel <- renderUI({
      tagList(
        h4("Keyboard Shortcuts"),
        tags$table(class = "table table-striped", style = "font-size: 13px;",
          tags$thead(tags$tr(tags$th("Shortcut"), tags$th("Action"))),
          tags$tbody(
            tags$tr(tags$td(tags$code("Ctrl/Cmd + Shift + D")), tags$td("Launch DataExplorerPro (RStudio)")),
            tags$tr(tags$td(tags$code("Ctrl/Cmd + Enter")), tags$td("Send chat message in AI Chat")),
            tags$tr(tags$td(tags$code("Shift + Enter")), tags$td("Insert newline in chat input")),
            tags$tr(tags$td(tags$code("Esc")), tags$td("Close modal dialogs")),
            tags$tr(tags$td(tags$code("?")), tags$td("Open this help panel"))
          )
        ),
        tags$hr(),
        h5("Mouse Actions"),
        tags$ul(
          tags$li(strong("Click status badge"), " - View Ollama setup options"),
          tags$li(strong("Click theme dropdown"), " - Switch visual theme"),
          tags$li(strong("Click chart expand icon"), " - View in fullscreen modal"),
          tags$li(strong("Hover over charts"), " - View interactive tooltips")
        )
      )
    })
  })
  
  # ============================================
  # DATA LOADING
  # ============================================
  
  observeEvent(input$load_data, {
    req(input$load_data > 0)
    
    tryCatch({
      data <- NULL
      
      # Load data from file or sample
      if (!is.null(input$data_file)) {
        file_ext <- tools::file_ext(input$data_file$name)
        data <- switch(tolower(file_ext),
                       "csv" = utils::read.csv(input$data_file$datapath, stringsAsFactors = FALSE),
                       "tsv" = utils::read.delim(input$data_file$datapath, stringsAsFactors = FALSE),
                       "xlsx" = {
                         if (!requireNamespace("readxl", quietly = TRUE)) {
                           stop("The 'readxl' package is not installed. Install it or choose another file type.")
                         }
                         as.data.frame(readxl::read_excel(input$data_file$datapath))
                       },
                       "rds" = base::readRDS(input$data_file$datapath),
                       "parquet" = {
                         if (!requireNamespace("arrow", quietly = TRUE)) {
                           stop("The 'arrow' package is not installed. Install it or choose another file type.")
                         }
                         as.data.frame(arrow::read_parquet(input$data_file$datapath))
                       },
                       NULL)
        if (is.null(data)) {
          stop("Unsupported file format: ", file_ext)
        }
      } else if (input$sample_data != "none") {
        data <- get_sample_data(input$sample_data)
      } else {
        showNotification("Please select a file or sample dataset", type = "warning")
        return()
      }
      
      # Store data
      global_data$data <- data
      global_data$original_data <- data
      global_data$transformed_data <- data

      # Refresh Ask-tab sample questions with dataset-aware suggestions
      updateSelectInput(
        session,
        "sample_nlp_query",
        choices = build_context_aware_sample_questions(data),
        selected = ""
      )
      
      # Show success message
      showNotification("Data loaded successfully!", type = "message")
      
      # Trigger data loaded event (safely, after success notification)
      tryCatch({
        if (requireNamespace("gargoyle", quietly = TRUE)) {
          gargoyle::trigger("data_loaded")
        }
      }, error = function(e) {
        # Silently ignore gargoyle errors - data is already loaded
      })
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Get sample data
  get_sample_data <- function(dataset_name) {
    result <- switch(dataset_name,
           "mtcars" = datasets::mtcars,
           "iris" = datasets::iris,
           "diamonds" = {
             if (!requireNamespace("ggplot2", quietly = TRUE)) {
               stop("The 'ggplot2' package is not installed. Install it or choose another dataset.")
             }
             as.data.frame(ggplot2::diamonds)
           },
           "penguins" = {
             if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
               stop("The 'palmerpenguins' package is not installed. Install it with: install.packages('palmerpenguins')")
             }
             as.data.frame(palmerpenguins::penguins)
           },
           "gapminder" = {
             if (!requireNamespace("gapminder", quietly = TRUE)) {
               stop("The 'gapminder' package is not installed. Install it or choose another dataset.")
             }
             as.data.frame(gapminder::gapminder)
           },
           "airquality" = datasets::airquality,
           "usarrests" = datasets::USArrests,
           "co2" = datasets::CO2,
           "chickweight" = datasets::ChickWeight,
           "toothgrowth" = datasets::ToothGrowth,
           NULL)
    
    if (is.null(result)) {
      stop("Unknown dataset: ", dataset_name)
    }
    
    as.data.frame(result)
  }
  
  # ============================================
  # HAS_DATA FLAG (for welcome hero conditionalPanel)
  # ============================================
  output$has_data <- reactive({ !is.null(global_data$data) })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # ============================================
  # DATA INFO DISPLAY
  # ============================================
  
  output$data_info <- renderUI({
    req(global_data$data)
    
    data <- global_data$data
    nrow <- nrow(data)
    ncol <- ncol(data)
    missing <- sum(is.na(data))
    missing_pct <- round(100 * missing / (nrow * ncol), 1)
    
    tagList(
      div(class = "stat-card",
          div(class = "value", nrow),
          div(class = "label", "Rows")
      ),
      div(class = "stat-card",
          div(class = "value", ncol),
          div(class = "label", "Columns")
      ),
      div(class = "stat-card",
          div(class = "value", missing_pct, "%"),
          div(class = "label", "Missing")
      )
    )
  })
  
  # ============================================
  # VARIABLE LIST
  # ============================================
  
  output$variable_list <- renderUI({
    req(global_data$data)
    
    data <- global_data$data
    var_info <- lapply(names(data), function(var) {
      type <- class(data[[var]])[1]
      type_class <- switch(type,
                           "numeric" = "numeric",
                           "integer" = "numeric",
                           "character" = "categorical",
                           "factor" = "categorical",
                           "Date" = "datetime",
                           "POSIXct" = "datetime",
                           "POSIXt" = "datetime",
                           "logical" = "categorical",
                           "other")
      
      tags$div(class = paste("variable-tag", type_class),
               tags$strong(var), " (", type, ")")
    })
    
    do.call(tagList, var_info)
  })
  
  # ============================================
  # CHART VARIABLE SELECTORS
  # ============================================
  
  output$x_var_selector <- renderUI({
    req(global_data$data)
    selectInput("x_var", "X Variable:", choices = names(global_data$data))
  })
  
  output$y_var_selector <- renderUI({
    req(global_data$data)
    selectInput("y_var", "Y Variable:", choices = names(global_data$data))
  })
  
  output$color_var_selector <- renderUI({
    req(global_data$data)
    selectInput("color_var", "Color By:", 
                choices = c("None" = "", names(global_data$data)))
  })
  
  output$size_var_selector <- renderUI({
    req(global_data$data)
    label <- if (is.null(input$chart_type)) {
      "Size By:"
    } else {
      switch(input$chart_type,
             dumbbell = "Value 2:",
             forest = "Lower CI:",
             sunburst = "Value:",
             treemap = "Value:",
             bubble = "Size By:",
             "Size By:")
    }
    selectInput("size_var", label, 
                choices = c("None" = "", names(global_data$data)))
  })
  
  output$facet_var_selector <- renderUI({
    req(global_data$data)
    selectInput("facet_var", "Facet By:", 
                choices = c("None" = "", names(global_data$data)))
  })
  
  output$group_var_selector <- renderUI({
    req(global_data$data)
    label <- if (identical(input$chart_type, "forest")) "Upper CI:" else "Group By:"
    selectInput("group_var", label,
                choices = c("None" = "", names(global_data$data)))
  })

  # Chart availability indicator
  output$chart_availability <- renderUI({
    req(global_data$data)
    data <- global_data$data
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    cat_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    all_cols <- names(data)

    can_make <- function(chart_type) {
      switch(chart_type,
             scatter = length(numeric_cols) >= 2,
             line = length(numeric_cols) >= 2,
             bar = length(all_cols) >= 1,
             histogram = length(numeric_cols) >= 1,
             box = length(numeric_cols) >= 1 && (length(cat_cols) >= 1 || length(numeric_cols) >= 1),
             violin = length(numeric_cols) >= 1 && (length(cat_cols) >= 1 || length(numeric_cols) >= 1),
             heatmap = length(all_cols) >= 2,
             density = length(numeric_cols) >= 1,
             pie = length(all_cols) >= 1,
             violin_box = length(numeric_cols) >= 1 && length(cat_cols) >= 1,
             beeswarm = length(numeric_cols) >= 1 && length(cat_cols) >= 1,
             dumbbell = length(numeric_cols) >= 2 && length(cat_cols) >= 1,
             forest = length(numeric_cols) >= 3,
             bland_altman = length(numeric_cols) >= 2,
             stacked_area = length(numeric_cols) >= 2 && length(cat_cols) >= 1,
             bubble = length(numeric_cols) >= 3,
             sunburst = length(all_cols) >= 2,
             treemap = length(all_cols) >= 2,
             parallel = length(numeric_cols) >= 2,
             radar = length(numeric_cols) >= 3,
             FALSE)
    }

    charts <- c(
      "scatter", "line", "bar", "histogram", "box", "violin", "heatmap", "density", "pie",
      "violin_box", "beeswarm", "dumbbell",
      "forest", "bland_altman",
      "stacked_area",
      "bubble", "sunburst", "treemap",
      "parallel", "radar"
    )
    labels <- c(
      scatter = "Scatter", line = "Line", bar = "Bar", histogram = "Histogram", box = "Box", violin = "Violin",
      heatmap = "Heatmap", density = "Density", pie = "Pie",
      violin_box = "Violin-Box", beeswarm = "Beeswarm", dumbbell = "Dumbbell",
      forest = "Forest", bland_altman = "Bland-Altman",
      stacked_area = "Stacked Area",
      bubble = "Bubble", sunburst = "Sunburst", treemap = "Treemap",
      parallel = "Parallel", radar = "Radar"
    )

    pills <- lapply(charts, function(ct) {
      ok <- can_make(ct)
      style <- if (ok) {
        "display:inline-block;margin:4px 6px 0 0;padding:4px 8px;border-radius:999px;background:rgba(16,185,129,0.18);color:#0f766e;border:1px solid rgba(16,185,129,0.5);font-size:11px;font-weight:600;"
      } else {
        "display:inline-block;margin:4px 6px 0 0;padding:4px 8px;border-radius:999px;background:rgba(148,163,184,0.25);color:#64748b;border:1px solid rgba(148,163,184,0.35);font-size:11px;font-weight:600;"
      }
      tags$span(style = style, labels[[ct]])
    })

    div(
      div(style = "margin-top:6px;font-size:12px;color:var(--text-muted);", "Available chart types for this dataset:"),
      div(pills)
    )
  })
  
  # ============================================
  # CHART CREATION
  # ============================================
  
  build_chart <- function(notify = TRUE, insights = TRUE) {
    req(global_data$data)
    req(input$chart_type)
    stage <- "init"
    chart_context <- "type=unknown"

    tryCatch({
      stage <- "sampling"
      perf_threshold <- input$perf_sample_threshold %||% 50000
      perf_sample_size <- input$perf_sample_size %||% 50000
      perf_auto <- if (!is.null(input$perf_auto_sample)) input$perf_auto_sample else TRUE
      sampled_info <- get_analysis_data(global_data$data, threshold = perf_threshold, sample_size = perf_sample_size, auto_sample = perf_auto)
      data <- sampled_info$data
      if (isTRUE(sampled_info$sampled) && notify) {
        showNotification(
          paste0("Using a sample of ", sampled_info$sample_n, " rows (of ", sampled_info$original_n, ") for faster charting."),
          type = "message"
        )
      }
      chart_type <- input$chart_type
      chart_context <- paste(
        "type=", chart_type,
        "x=", input$x_var %||% "",
        "y=", input$y_var %||% "",
        "color=", input$color_var %||% "",
        "size=", input$size_var %||% "",
        "group=", input$group_var %||% "",
        sep = ""
      )

      stage <- "validation"
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      all_cols <- names(data)

      required_map <- list(
        scatter = c("x_var", "y_var"),
        line = c("x_var", "y_var"),
        bar = c("x_var", "y_var"),
        histogram = c("x_var"),
        box = c("x_var", "y_var"),
        violin = c("x_var", "y_var"),
        heatmap = c("x_var", "y_var"),
        density = c("x_var"),
        pie = c("x_var"),
        violin_box = c("x_var", "y_var"),
        beeswarm = c("x_var", "y_var"),
        dumbbell = c("x_var", "y_var", "size_var"),
        forest = c("x_var", "y_var", "size_var", "group_var"),
        bland_altman = c("x_var", "y_var"),
        stacked_area = c("x_var", "y_var", "group_var"),
        bubble = c("x_var", "y_var", "size_var"),
        sunburst = c("x_var", "y_var", "size_var"),
        treemap = c("x_var", "y_var", "size_var"),
        parallel = character(0),
        radar = character(0)
      )

      required_inputs <- required_map[[chart_type]]

      if (chart_type %in% c("histogram", "density") && length(numeric_cols) == 0) {
        if (notify) showNotification("This chart requires numeric data. No numeric columns found.", type = "error")
        return(NULL)
      }

      # Resolve chart variables to compatible defaults to prevent invalid combos
      cat_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      date_cols <- names(data)[sapply(data, inherits, what = "Date")]
      pick_num <- function(v, idx = 1) {
        if (!is.null(v) && nzchar(v) && v %in% numeric_cols) return(v)
        if (length(numeric_cols) >= idx) return(numeric_cols[idx])
        ""
      }
      pick_cat <- function(v, idx = 1) {
        if (!is.null(v) && nzchar(v) && v %in% cat_cols) return(v)
        if (length(cat_cols) >= idx) return(cat_cols[idx])
        if (length(all_cols) >= idx) return(all_cols[idx])
        ""
      }
      pick_any <- function(v, idx = 1) {
        if (!is.null(v) && nzchar(v) && v %in% all_cols) return(v)
        if (length(all_cols) >= idx) return(all_cols[idx])
        ""
      }
      pick_date <- function(v) {
        if (!is.null(v) && nzchar(v) && v %in% date_cols) return(v)
        if (length(date_cols) >= 1) return(date_cols[1])
        ""
      }

      x_var <- input$x_var %||% ""
      y_var <- input$y_var %||% ""
      color_var <- input$color_var %||% ""
      size_var <- input$size_var %||% ""
      group_var <- input$group_var %||% ""
      facet_var <- input$facet_var %||% ""

      if (chart_type %in% c("scatter", "line", "bland_altman")) {
        x_var <- pick_num(x_var, 1); y_var <- pick_num(y_var, 2)
      } else if (chart_type %in% c("bar", "box", "violin", "violin_box", "beeswarm")) {
        x_var <- pick_cat(x_var, 1); y_var <- pick_num(y_var, 1)
      } else if (chart_type %in% c("histogram", "density")) {
        x_var <- pick_num(x_var, 1)
      } else if (chart_type == "heatmap") {
        x_var <- pick_any(x_var, 1); y_var <- pick_any(y_var, 2)
      } else if (chart_type == "pie") {
        x_var <- pick_cat(x_var, 1)
        if (!(nzchar(y_var) && y_var %in% numeric_cols)) y_var <- ""
      } else if (chart_type == "dumbbell") {
        x_var <- pick_cat(x_var, 1); y_var <- pick_num(y_var, 1); size_var <- pick_num(size_var, 2)
      } else if (chart_type == "forest") {
        x_var <- pick_cat(x_var, 1); y_var <- pick_num(y_var, 1); size_var <- pick_num(size_var, 2); group_var <- pick_num(group_var, 3)
      } else if (chart_type == "stacked_area") {
        x_var <- pick_num(x_var, 1); y_var <- pick_num(y_var, 2); group_var <- pick_cat(group_var, 1)
      } else if (chart_type == "bubble") {
        x_var <- pick_num(x_var, 1); y_var <- pick_num(y_var, 2); size_var <- pick_num(size_var, 3)
      } else if (chart_type %in% c("sunburst", "treemap")) {
        x_var <- pick_any(x_var, 1); y_var <- pick_any(y_var, 2); size_var <- pick_num(size_var, 1)
      }

      sync_selected <- function(id, value) {
        if (!is.null(value) && nzchar(value) && !identical(input[[id]], value)) {
          updateSelectInput(session, id, selected = value)
        }
      }
      sync_selected("x_var", x_var)
      sync_selected("y_var", y_var)
      sync_selected("color_var", color_var)
      sync_selected("size_var", size_var)
      sync_selected("group_var", group_var)
      sync_selected("facet_var", facet_var)

      if (chart_type %in% c("sunburst", "treemap") && !nzchar(size_var)) {
        data$.__value__ <- 1
        size_var <- ".__value__"
      }
      if (!is.null(required_inputs) && length(required_inputs) > 0) {
        resolved_vals <- list(
          x_var = x_var,
          y_var = y_var,
          size_var = size_var,
          group_var = group_var
        )
        missing_inputs <- required_inputs[sapply(required_inputs, function(k) {
          v <- resolved_vals[[k]]
          is.null(v) || !nzchar(v)
        })]
        if (length(missing_inputs) > 0) {
          if (notify) {
            showNotification(
              paste("Missing required inputs:", paste(missing_inputs, collapse = ", ")),
              type = "error"
            )
          }
          output$main_chart <- renderPlotly({
            empty_plot(paste("Missing required inputs:", paste(missing_inputs, collapse = ", ")))
          })
          return(NULL)
        }
      }
      if (chart_type == "forest") {
        if (!nzchar(x_var) || !nzchar(y_var) || !nzchar(size_var) || !nzchar(group_var)) {
          if (notify) {
            showNotification("Forest plot requires study label and three numeric columns (effect, lower CI, upper CI).", type = "error")
          }
          output$main_chart <- renderPlotly({
            empty_plot("Forest plot requires study, effect, lower CI, upper CI")
          })
          return(NULL)
        }
        if (!is.numeric(data[[y_var]]) || !is.numeric(data[[size_var]]) || !is.numeric(data[[group_var]])) {
          if (notify) {
            showNotification("Forest plot requires numeric effect and CI columns.", type = "error")
          }
          output$main_chart <- renderPlotly({
            empty_plot("Forest plot requires numeric effect and CI columns")
          })
          return(NULL)
        }
      }

      chart_context <- paste(
        "type=", chart_type,
        "x=", x_var,
        "y=", y_var,
        "color=", color_var,
        "size=", size_var,
        "group=", group_var,
        sep = ""
      )
      
      # Cache key to avoid rebuilding the same chart
      stage <- "cache"
      cache_key <- paste(
        chart_type,
        x_var, y_var, color_var, size_var, group_var, facet_var,
        input$chart_theme, input$color_palette, input$point_size, input$opacity,
        nrow(data), ncol(data),
        sep = "|"
      )
      if (identical(cache_key, chart_cache$last_key)) {
        return(invisible(NULL))
      }
      chart_cache$last_key <- cache_key

      # Get customization options
      stage <- "options"
      theme <- input$chart_theme %||% "default"
      point_size <- input$point_size %||% 8
      opacity <- input$opacity %||% 0.7
      
      heatmap_value_var <- if (!is.null(color_var) && color_var != "" && is.numeric(data[[color_var]])) {
        color_var
      } else {
        NULL
      }

      # Build plot based on chart type
      stage <- "switch"
      p <- switch(chart_type,
                  # Basic Charts
                  "scatter" = create_scatter_plot(data, x_var, y_var, 
                                                   color_var, size_var, trendline = isTRUE(input$show_trendline)),
                  "line" = create_line_chart(data, x_var, y_var, 
                                            color_var, facet_var),
                  "bar" = create_bar_chart(data, x_var, y_var, 
                                          color_var, facet_var),
                  "histogram" = create_histogram(data, x_var, color_var),
                  "box" = create_box_plot(data, x_var, y_var, color_var),
                  "violin" = create_violin_plot(data, x_var, y_var, color_var),
                  "heatmap" = create_heatmap(data, x_var, y_var, heatmap_value_var),
                  "density" = create_density_plot(data, x_var, color_var),
                  "pie" = create_pie_chart(
                    data,
                    x_var,
                    if (!is.null(y_var) && nzchar(y_var)) y_var else NULL
                  ),
                  
                  # Advanced Charts
                  "violin_box" = create_violin_box_combo(data, x_var, y_var, color_var),
                  "beeswarm" = create_beeswarm_plot(data, x_var, y_var, color_var),
                  "dumbbell" = create_dumbbell_plot(data, x_var, y_var, size_var, color_var),
                  
                  # Statistical Charts
                  "forest" = create_forest_plot(data, x_var, y_var, size_var, group_var),
                  "bland_altman" = create_bland_altman_plot(data, x_var, y_var),
                  
                  # Time Series
                  "stacked_area" = create_stacked_area_chart(data, x_var, y_var, group_var),
                  
                  # Multivariate
                  "bubble" = create_bubble_chart(data, x_var, y_var, size_var, color_var),
                  "sunburst" = create_sunburst_chart(data, x_var, y_var, size_var),
                  "treemap" = create_treemap(data, x_var, y_var, size_var, if (nzchar(color_var)) color_var else NULL),
                  
                  # 3D & Special
                  "parallel" = create_parallel_coords(data),
                  "radar" = create_radar_chart(data),
                  
                  stop("Unknown chart type")
      )

      if (is.null(p) || !inherits(p, "plotly")) {
        if (notify) showNotification("Chart could not be created for this selection.", type = "error")
        output$main_chart <- renderPlotly({ empty_plot("Chart could not be created") })
        return(NULL)
      }
      
      # Apply publication theme if selected
      stage <- "theme"
      if (theme != "default") {
        p <- tryCatch(apply_publication_theme(p, theme = theme), error = function(e) p)
      }

      # Apply palette and styling options
      stage <- "palette"
      palette_name <- input$color_palette %||% "viridis"
      palette_colors <- tryCatch({
        if (palette_name %in% c("viridis", "magma", "plasma", "inferno")) {
          option_map <- c(viridis = "D", magma = "A", plasma = "B", inferno = "C")
          if (requireNamespace("viridisLite", quietly = TRUE)) {
            viridisLite::viridis(8, option = option_map[[palette_name]])
          } else {
            c("#0ea5a4", "#0284c7", "#10b981", "#f59e0b", "#ef4444", "#8b5cf6", "#22c55e", "#14b8a6")
          }
        } else if (palette_name == "set2") {
          c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3")
        } else {
          if (requireNamespace("viridisLite", quietly = TRUE)) {
            viridisLite::viridis(8, option = "D")
          } else {
            c("#0ea5a4", "#0284c7", "#10b981", "#f59e0b", "#ef4444", "#8b5cf6", "#22c55e", "#14b8a6")
          }
        }
      }, error = function(e) c("#0ea5a4", "#0284c7", "#10b981", "#f59e0b", "#ef4444", "#8b5cf6", "#22c55e", "#14b8a6"))

      p <- tryCatch(p %>% layout(colorway = palette_colors), error = function(e) p)

      # Only style when plotly has data traces to avoid warnings
      stage <- "style"
      has_traces <- !is.null(p$x$data) && length(p$x$data) > 0
      if (has_traces) {
        if (chart_type %in% c("scatter", "beeswarm", "bubble")) {
          p <- tryCatch(plotly::style(p, marker = list(size = point_size, opacity = opacity)), error = function(e) p)
        } else if (chart_type %in% c("line", "stacked_area")) {
          p <- tryCatch(plotly::style(p, line = list(width = 2, color = palette_colors[1]), opacity = opacity), error = function(e) p)
        } else if (chart_type %in% c("bar", "histogram", "density", "box", "violin", "pie")) {
          p <- tryCatch(plotly::style(p, marker = list(opacity = opacity)), error = function(e) p)
        }
      }

      stage <- "layout"
      p <- tryCatch(p %>% layout(
        margin = list(l = 55, r = 20, t = 60, b = 55),
        hoverlabel = list(bgcolor = "rgba(15, 23, 42, 0.9)", font = list(color = "#f8fafc"))
      ), error = function(e) p)

      # Store chart
      stage <- "store"
      global_data$charts$main <- p
      
      # Render chart
      stage <- "render"
      output$main_chart <- renderPlotly({
        tryCatch({
          if (is.null(p)) {
            empty_plot()
          } else {
            plotly::plotly_build(plotly::config(p, displaylogo = FALSE))
          }
        }, error = function(e) {
          empty_plot(paste("Render error:", e$message))
        })
      })

      # Render large chart for modal
      stage <- "render_large"
      output$main_chart_large <- renderPlotly({
        tryCatch({
          req(global_data$charts$main)
          plotly::plotly_build(global_data$charts$main)
        }, error = function(e) {
          empty_plot(paste("Render error:", e$message))
        })
      })
      
      # Generate and show AI insights (skip during auto-update to avoid blocking)
      stage <- "insights"
      output$chart_insights <- renderUI({
        tryCatch({
          if (!isTRUE(insights)) {
            return(div(style = "color: var(--text-muted); font-size: 13px;",
                       "AI insights are paused during auto-update. Click Create Chart to generate."))
          }
          generate_chart_insights(data, x_var, y_var, chart_type, model = input$ollama_model)
        }, error = function(e) {
          div(style = "color: var(--text-muted); font-size: 13px;",
              paste("AI insights unavailable:", e$message))
        })
      })
      
      # Show code
      stage <- "code"
      output$chart_code <- renderText({
        generate_chart_code(chart_type, x_var, y_var, color_var, size_var, facet_var)
      })
      
      # Trigger chart created event
      stage <- "trigger"
      if (requireNamespace("gargoyle", quietly = TRUE)) {
        trg <- get0("trigger", envir = asNamespace("gargoyle"), mode = "function")
        if (is.function(trg)) {
          try(trg("chart_created"), silent = TRUE)
        }
      }
      
      if (notify) showNotification("Chart created successfully!", type = "message")
      
    }, error = function(e) {
      if (notify) showNotification(paste("Error creating chart:", e$message), type = "error")
      output$main_chart <- renderPlotly({
        empty_plot(paste("Chart error:", e$message, "\n", stage, "\n", chart_context))
      })
    })
  }

  observeEvent(input$create_chart, {
    build_chart(notify = TRUE, insights = TRUE)
  })

  chart_inputs <- reactive({
    list(
      input$chart_type, input$x_var, input$y_var,
      input$color_var, input$size_var, input$group_var, input$facet_var,
      input$chart_theme, input$color_palette, input$point_size, input$opacity
    )
  })
  observeEvent(shiny::debounce(chart_inputs, 600)(), {
    if (isTRUE(input$auto_create_chart)) {
      build_chart(notify = FALSE, insights = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$expand_chart, {
    req(global_data$charts$main)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Chart Preview (Expanded)"),
      plotlyOutput("main_chart_large", height = "80vh")
    ))
  })
  
  # ============================================
  # NATURAL LANGUAGE QUERY
  # ============================================

  output$query_response <- renderUI({
    div(class = "empty-state",
      tags$i(class = "fas fa-comment-dots"),
      tags$h4("Ask your first question"),
      tags$p("Describe an analysis goal in plain language to generate insights and runnable code.")
    )
  })

  output$generated_code <- renderText({
    "# Suggested code will appear here after you submit a query"
  })

  output$query_result <- renderUI({
    div(class = "query-result-empty",
      tags$i(class = "fas fa-table"),
      "Query results will appear here after processing."
    )
  })

  output$query_history <- renderUI({
    div(class = "query-history-empty",
      tags$i(class = "fas fa-history"),
      "No queries yet. Your recent prompts will appear here."
    )
  })

  observeEvent(input$sample_nlp_query, {
    sample_query <- input$sample_nlp_query %||% ""
    if (!nzchar(sample_query)) {
      return()
    }

    updateTextAreaInput(session, "nlp_query", value = sample_query)
    updateSelectInput(session, "sample_nlp_query", selected = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$submit_query, {
    req(global_data$data)
    req(input$nlp_query)
    
    tryCatch({
      result <- NULL
      withProgress(message = "Processing query...", value = 0, {
        query <- input$nlp_query
        query_type <- input$query_type
        selected_model <- input$ollama_model
        if (is.null(selected_model) || selected_model == "") {
          if (length(ollama_config$available_models) > 0) {
            selected_model <- ollama_config$available_models[1]
          } else {
            showNotification("No Ollama models available. Please start Ollama or refresh models.", type = "error")
            return()
          }
        }
        
        incProgress(0.4)
        
        # Process query with Ollama
        result <- process_natural_language_query(
          query,
          query_type,
          global_data$data,
          model = selected_model,
          temperature = input$temperature,
          max_tokens = input$max_tokens
        )
        
        incProgress(0.8)
      
      # Add to history
      global_data$query_history <- c(list(list(
        query = query,
        type = query_type,
        timestamp = Sys.time(),
        result = result
      )), global_data$query_history)
      
      # Display response
        output$query_response <- renderUI({
          insights_list <- result$insights
          if (is.character(insights_list)) {
            insights_list <- unlist(strsplit(insights_list, "\n|;"))
            insights_list <- insights_list[nzchar(trimws(insights_list))]
          }
          div(
            h5("Analysis Result:"),
            div(style = "white-space: pre-wrap;", format_ai_text(result$explanation)),
            if (!is.null(insights_list) && length(insights_list) > 0) {
              div(class = "alert alert-info",
                  h6("Key Insights:"),
                  tags$ul(lapply(insights_list, function(x) tags$li(format_ai_text(x))))
              )
            }
          )
        })
      
      # Display generated code
        output$generated_code <- renderText({
          result$code
        })

        # Store latest query data
        global_data$query_code <- result$code
        global_data$query_type <- query_type
        global_data$query_result <- result$data_result
      
      # Display query result if applicable
        if (!is.null(result$data_result)) {
          output$query_result <- renderUI({
            if (is.data.frame(result$data_result)) {
              DTOutput("query_dt")
            } else {
              plotlyOutput("query_plot", height = "400px")
            }
          })
          
          if (is.data.frame(result$data_result)) {
            output$query_dt <- renderDT({
              datatable(result$data_result, options = list(pageLength = 10))
            })
          } else {
            output$query_plot <- renderPlotly({
              result$data_result
            })
          }
        } else {
          output$query_result <- renderUI({
            div(class = "query-result-empty",
              tags$i(class = "fas fa-table"),
              "No structured table or plot was returned for this query."
            )
          })
        }
      
      # Update query history
      output$query_history <- renderUI({
        if (length(global_data$query_history) == 0) {
          return(div(class = "query-history-empty",
            tags$i(class = "fas fa-history"),
            "No queries yet. Your recent prompts will appear here."
          ))
        }
        history_items <- lapply(global_data$query_history[seq_len(min(5, length(global_data$query_history)))], function(item) {
          div(class = "query-history-item",
              strong(substr(item$query, 1, 50), ifelse(nchar(item$query) > 50, "...", "")),
              br(),
              span(class = "query-history-time", 
                   format(item$timestamp, "%H:%M:%S"))
          )
        })
        do.call(tagList, history_items)
      })
      
        incProgress(1)
      })

      if (is.null(result)) {
        return(NULL)
      }
      
      showNotification("Query processed!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error processing query:", e$message), type = "error")
    })
  })

  observeEvent(input$run_code, {
    req(global_data$data)
    req(global_data$query_code)

    tryCatch({
      code <- global_data$query_code
      env <- new.env(parent = globalenv())
      env$data <- global_data$data

      # Ensure common libraries are available
      suppressWarnings({
        library(dplyr)
        library(ggplot2)
        library(plotly)
      })

      parsed <- parse(text = code)
      result <- NULL
      for (expr in parsed) {
        result <- eval(expr, env)
      }

      # If transform code updated data, store it
      if (!is.null(env$data) && is.data.frame(env$data) && !identical(env$data, global_data$data)) {
        global_data$data <- env$data
        global_data$transformed_data <- env$data
      }

      # Render result if returned
      output$query_result <- renderUI({
        if (is.data.frame(result)) {
          DTOutput("query_dt")
        } else {
          plotlyOutput("query_plot", height = "400px")
        }
      })

      if (is.data.frame(result)) {
        output$query_dt <- renderDT({
          datatable(result, options = list(pageLength = 10, scrollX = TRUE))
        })
      } else if (inherits(result, "ggplot")) {
        output$query_plot <- renderPlotly({
          ggplotly(result)
        })
      } else if (inherits(result, "plotly")) {
        output$query_plot <- renderPlotly({
          result
        })
      }

      showNotification("Generated code executed successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error running code:", e$message), type = "error")
    })
  })
  
  # ============================================
  # AUTO EDA REPORT
  # ============================================
  
  observeEvent(input$auto_eda, {
    req(global_data$data)
    
    # Switch to EDA tab
    updateTabsetPanel(session, "main_tabs", selected = "eda")
    
    generate_eda_report()
  })
  
  observeEvent(input$generate_summary, {
    req(global_data$data)
    updateTabsetPanel(session, "main_tabs", selected = "eda")
    generate_eda_report()
  })
  
  apply_pub_theme <- function(p, title = NULL, height = NULL, legend = TRUE) {
    p <- p %>% layout(
      title = if (!is.null(title)) list(
        text = title,
        x = 0.02,
        xanchor = "left",
        font = list(size = 16, color = "#0f172a")
      ) else NULL,
      font = list(family = "Inter, Arial", size = 12, color = "#0f172a"),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      margin = list(l = 60, r = 30, t = 60, b = 50),
      xaxis = list(
        showline = TRUE,
        linecolor = "rgba(15,23,42,0.35)",
        gridcolor = "rgba(148,163,184,0.25)",
        zeroline = FALSE,
        ticks = "outside"
      ),
      yaxis = list(
        showline = TRUE,
        linecolor = "rgba(15,23,42,0.35)",
        gridcolor = "rgba(148,163,184,0.25)",
        zeroline = FALSE,
        ticks = "outside"
      ),
      legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 11)),
      showlegend = legend
    )
    if (!is.null(height)) {
      p$x$layout$height <- height
    }
    p
  }

  generate_eda_report <- function(force_full = FALSE) {
    full_data <- global_data$data
    selected_sections <- input$eda_chart_types %||% c(
      "data_preview", "histograms", "box_plots", "correlation",
      "pie_charts", "summary_stats", "quality_report"
    )
    has_section <- function(x) x %in% selected_sections
    # Performance sampling settings (from settings modal)
    perf_threshold <- input$perf_sample_threshold %||% 50000
    perf_sample_size <- input$perf_sample_size %||% 50000
    perf_auto <- if (!is.null(input$perf_auto_sample)) input$perf_auto_sample else TRUE

    # Respect one-off full-run override
    perf_auto_effective <- if (isTRUE(force_full)) FALSE else perf_auto

    data_sig <- paste(
      nrow(full_data),
      ncol(full_data),
      paste(names(full_data), collapse = "|"),
      paste(sapply(full_data, function(x) class(x)[1]), collapse = "|"),
      sum(is.na(full_data)),
      sep = "::"
    )
    options_sig <- paste(selected_sections, collapse = "|")
    sampling_sig <- paste(perf_threshold, perf_sample_size, perf_auto_effective, force_full, sep = "|")
    cache_key <- paste(data_sig, options_sig, sampling_sig, sep = "||")
    if (!isTRUE(force_full) && identical(cache_key, eda_cache$last_key)) {
      showNotification("EDA already up to date.", type = "message")
      return(invisible(NULL))
    }
    eda_cache$last_key <- cache_key

    sampled_info <- get_analysis_data(full_data, threshold = perf_threshold, sample_size = perf_sample_size, auto_sample = perf_auto_effective)
    data <- sampled_info$data

    # Container for per-component timings
    component_times <- list()

    # Start profiling timer (approximate time for EDA generation)
    eda_start <- proc.time()

    # Basic statistics (profiled)
    stats_profile <- profile_expr({
      list(
        rows = nrow(full_data),
        cols = ncol(full_data),
        missing_pct = round(100 * sum(is.na(full_data)) / (nrow(full_data) * ncol(full_data)), 1),
        numeric_count = sum(sapply(full_data, is.numeric)),
        categorical_count = sum(sapply(full_data, function(x) is.factor(x) || is.character(x)))
      )
    })
    component_times$basic_stats <- stats_profile$time

    output$stat_rows <- renderUI({ div(class = "value", stats_profile$result$rows) })
    output$stat_cols <- renderUI({ div(class = "value", stats_profile$result$cols) })
    output$stat_missing <- renderUI({ div(class = "value", paste0(stats_profile$result$missing_pct, "%")) })
    output$stat_numeric <- renderUI({ div(class = "value", stats_profile$result$numeric_count) })
    output$stat_categorical <- renderUI({ div(class = "value", stats_profile$result$categorical_count) })

  # Show performance status
  output$perf_status <- renderUI({
      if (sampled_info$sampled) {
        tagList(
          tags$div(style = "font-size: 12px; color: #444;", paste0("Using sample: ", sampled_info$sample_n, " of ", sampled_info$original_n, " rows for faster analysis."))
        )
      } else {
        tags$div(style = "font-size: 12px; color: #444;", "Using full dataset for EDA.")
    }
  })

  observeEvent(input$copy_code, {
    session$sendCustomMessage("copyText", list(id = "generated_code"))
  })

  observeEvent(input$copy_transform, {
    session$sendCustomMessage("copyText", list(id = "transform_code"))
  })

  observeEvent(input$clipboard_copied, {
    showNotification("Copied to clipboard.", type = "message")
  })
    output$sample_preview_info <- renderUI({
      if (sampled_info$sampled) {
        div(
          style = "font-size: 12px; color: #444; margin-top: 6px;",
          paste0("Sample in use: ", sampled_info$sample_n, " of ", sampled_info$original_n, " rows."),
          div(style = "margin-top: 6px;",
              actionButton("expand_sample_preview", "View Sample", class = "btn-secondary btn-sm"))
        )
      } else {
        div(style = "font-size: 12px; color: #444; margin-top: 6px;", "Using full dataset.")
      }
    })
    # Keep stat cards based on full_data (avoid inconsistent overrides from sampled data)
    
    # Data preview (profiled)
    if (has_section("data_preview")) {
      data_preview_profile <- profile_expr({
        datatable(
          if (isTRUE(input$show_sample_preview) && sampled_info$sampled) head(data, 100) else head(full_data, 100),
          options = list(pageLength = 10, scrollX = TRUE, scrollY = "300px")
        )
      })
      component_times$data_preview <- data_preview_profile$time
      output$data_preview <- renderDT({ data_preview_profile$result })
    } else {
      output$data_preview <- renderDT({ NULL })
    }
    
    # Distribution plots (Histograms) - profiled
    if (has_section("histograms")) {
      dist_profile <- profile_expr({
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        if (length(numeric_cols) == 0) {
          return(apply_pub_theme(empty_plot("No numeric columns for histograms"), title = "No numeric columns for histograms", height = 420))
        }
        plots <- lapply(numeric_cols[seq_len(min(6, length(numeric_cols)))], function(col) {
          plot_ly(data, x = ~get(col), type = "histogram", marker = list(color = "#3b82f6", opacity = 0.75)) %>%
            layout(title = paste("Distribution of", col), xaxis = list(title = col), yaxis = list(title = "Count"))
        })
        p <- subplot(plots, nrows = ceiling(length(plots)/2))
        apply_pub_theme(p, title = "Distribution Analysis", height = 600, legend = FALSE)
      })
      component_times$distributions <- dist_profile$time
      output$dist_plots <- renderPlotly({ dist_profile$result })
    } else {
      output$dist_plots <- renderPlotly({ NULL })
    }
    
    # Density plots - profiled
    if (has_section("density")) {
      density_profile <- profile_expr({
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        if (length(numeric_cols) == 0) {
          return(apply_pub_theme(empty_plot("No numeric columns for density plots"), title = "No numeric columns for density plots", height = 420))
        }
        plots <- lapply(numeric_cols[seq_len(min(6, length(numeric_cols)))], function(col) {
          plot_ly(data, x = ~get(col), type = "histogram", histnorm = "probability density", marker = list(color = "#8b5cf6", opacity = 0.5)) %>%
            layout(title = paste("Density of", col), xaxis = list(title = col), yaxis = list(title = "Density"))
        })
        p <- subplot(plots, nrows = ceiling(length(plots)/2))
        apply_pub_theme(p, title = "Density Plots", height = 600, legend = FALSE)
      })
      component_times$density <- density_profile$time
      output$eda_density_plots <- renderPlotly({ density_profile$result })
    } else {
      output$eda_density_plots <- renderPlotly({ NULL })
    }
    
    # Correlation matrix - profiled
    if (has_section("correlation")) {
      correlation_profile <- profile_expr({
        numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
        if (ncol(numeric_data) < 2) {
          return(apply_pub_theme(empty_plot("Need at least 2 numeric columns for correlation"), title = "Need at least 2 numeric columns for correlation", height = 420))
        }
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        p <- plot_ly(x = names(numeric_data), y = names(numeric_data), z = cor_matrix, type = "heatmap", colors = grDevices::colorRampPalette(c("#2563eb", "#ffffff", "#ef4444"))(10))
        apply_pub_theme(p, title = "Correlation Matrix", height = 450)
      })
      component_times$correlation <- correlation_profile$time
      output$correlation_plot <- renderPlotly({ correlation_profile$result })
    } else {
      output$correlation_plot <- renderPlotly({ NULL })
    }
    
    # Box plots by category - profiled
    if (has_section("box_plots")) {
      box_profile <- profile_expr({
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
        
        if (length(numeric_cols) >= 1 && length(categorical_cols) >= 1) {
          cat_col <- categorical_cols[1]
          num_col <- numeric_cols[1]
          p <- plot_ly(data, x = ~get(cat_col), y = ~get(num_col), type = "box", marker = list(color = "#3b82f6")) %>%
            layout(xaxis = list(title = cat_col), yaxis = list(title = num_col))
          apply_pub_theme(p, title = paste("Box Plot:", num_col, "by", cat_col), height = 500)
        } else if (length(numeric_cols) >= 1) {
          plots <- lapply(numeric_cols[seq_len(min(4, length(numeric_cols)))], function(col) {
            plot_ly(data, y = ~get(col), type = "box", name = col, marker = list(color = "#3b82f6"))
          })
          p <- subplot(plots, nrows = 1, shareY = FALSE)
          apply_pub_theme(p, title = "Box Plots of Numeric Variables", height = 500, legend = FALSE)
        }
      })
      component_times$boxplots <- box_profile$time
      output$box_plots <- renderPlotly({ box_profile$result })
    } else {
      output$box_plots <- renderPlotly({ NULL })
    }
    
    # Violin plots - profiled
    if (has_section("violin_plots")) {
      violin_profile <- profile_expr({
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
        
        if (length(numeric_cols) >= 1 && length(categorical_cols) >= 1) {
          cat_col <- categorical_cols[1]
          num_col <- numeric_cols[1]
          p <- plot_ly(data, x = ~get(cat_col), y = ~get(num_col), type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE), fillcolor = "#3b82f6", opacity = 0.6) %>%
            layout(xaxis = list(title = cat_col), yaxis = list(title = num_col))
          apply_pub_theme(p, title = paste("Violin Plot:", num_col, "by", cat_col), height = 500)
        } else if (length(numeric_cols) >= 1) {
          plots <- lapply(numeric_cols[seq_len(min(4, length(numeric_cols)))], function(col) {
            plot_ly(data, y = ~get(col), type = "violin", name = col, box = list(visible = TRUE), meanline = list(visible = TRUE), fillcolor = "#3b82f6", opacity = 0.6)
          })
          p <- subplot(plots, nrows = 1, shareY = FALSE)
          apply_pub_theme(p, title = "Violin Plots of Numeric Variables", height = 500, legend = FALSE)
        }
      })
      component_times$violin <- violin_profile$time
      output$eda_violin_plots <- renderPlotly({ violin_profile$result })
    } else {
      output$eda_violin_plots <- renderPlotly({ NULL })
    }
    
    # Scatter matrix - profiled
    if (has_section("scatter_matrix")) {
      scatter_profile <- profile_expr({
        numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
        if (ncol(numeric_data) < 2) {
        return(apply_pub_theme(empty_plot("Need at least 2 numeric columns for scatter matrix"), title = "Need at least 2 numeric columns for scatter matrix", height = 420))
        }
        cols_to_use <- names(numeric_data)[seq_len(min(5, ncol(numeric_data)))]
        dims <- lapply(cols_to_use, function(col) list(label = col, values = numeric_data[[col]]))
        p <- plot_ly(type = "splom", dimensions = dims, marker = list(color = "#3b82f6", size = 4, opacity = 0.6))
        apply_pub_theme(p, title = "Scatter Plot Matrix", height = 600)
      })
      component_times$scatter_matrix <- scatter_profile$time
      output$eda_scatter_matrix <- renderPlotly({ scatter_profile$result })
    } else {
      output$eda_scatter_matrix <- renderPlotly({ NULL })
    }
    
    # Categorical variable analysis (Pie) - profiled
    if (has_section("pie_charts")) {
      categorical_profile <- profile_expr({
        categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
        
        if (length(categorical_cols) >= 1) {
          cat_col <- categorical_cols[1]
          value_counts <- table(data[[cat_col]])
          p <- plot_ly(labels = names(value_counts), values = as.numeric(value_counts), type = "pie", hole = 0.35, marker = list(colors = c("#2563eb", "#7c3aed", "#f97316", "#14b8a6", "#f43f5e", "#0ea5e9")))
          apply_pub_theme(p, title = paste("Distribution of", cat_col), height = 500)
        }
      })
      component_times$categorical <- categorical_profile$time
      output$categorical_plots <- renderPlotly({ categorical_profile$result })
    } else {
      output$categorical_plots <- renderPlotly({ NULL })
    }
    
    # Bar charts for categorical variables - profiled
    if (has_section("bar_charts")) {
      bar_profile <- profile_expr({
        categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
        
        if (length(categorical_cols) >= 1) {
          plots <- lapply(categorical_cols[seq_len(min(4, length(categorical_cols)))], function(col) {
            value_counts <- table(data[[col]])
            plot_ly(x = names(value_counts), y = as.numeric(value_counts), type = "bar", marker = list(color = "#3b82f6", opacity = 0.85)) %>% layout(title = col, xaxis = list(title = ""), yaxis = list(title = "Count"))
          })
          p <- subplot(plots, nrows = ceiling(length(plots)/2), shareY = FALSE)
          apply_pub_theme(p, title = "Categorical Variable Distributions", height = 500, legend = FALSE)
        }
      })
      component_times$bar_charts <- bar_profile$time
      output$eda_bar_charts <- renderPlotly({ bar_profile$result })
    } else {
      output$eda_bar_charts <- renderPlotly({ NULL })
    }
    
    # Q-Q plots - profiled
    if (has_section("qq_plots")) {
      qq_profile <- profile_expr({
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        if (length(numeric_cols) == 0) {
        return(apply_pub_theme(empty_plot("No numeric columns for Q-Q plots"), title = "No numeric columns for Q-Q plots", height = 420))
        }
        plots <- lapply(numeric_cols[seq_len(min(4, length(numeric_cols)))], function(col) {
          qq_data <- qqnorm(data[[col]], plot.it = FALSE)
          plot_ly(x = qq_data$x, y = qq_data$y, type = "scatter", mode = "markers", marker = list(color = "#3b82f6", size = 5, opacity = 0.6)) %>% add_trace(x = range(qq_data$x), y = range(qq_data$x) * sd(data[[col]], na.rm = TRUE) + mean(data[[col]], na.rm = TRUE), type = "scatter", mode = "lines", line = list(color = "#ef4444", dash = "dash")) %>% layout(title = paste("Q-Q:", col), xaxis = list(title = "Theoretical"), yaxis = list(title = "Sample"), showlegend = FALSE)
        })
        p <- subplot(plots, nrows = ceiling(length(plots)/2))
        apply_pub_theme(p, title = "Normality Diagnostics", height = 500, legend = FALSE)
      })
      component_times$qq <- qq_profile$time
      output$eda_qq_plots <- renderPlotly({ qq_profile$result })
    } else {
      output$eda_qq_plots <- renderPlotly({ NULL })
    }
    
    # Missing values plot - profiled
    if (has_section("missing_values")) {
      missing_profile <- profile_expr({
        missing_counts <- sapply(data, function(x) sum(is.na(x)))
        missing_df <- data.frame(Variable = names(missing_counts), Missing = missing_counts, Percent = round(100 * missing_counts / nrow(data), 1))
        missing_df <- missing_df[order(-missing_df$Missing), ]
        p <- plot_ly(missing_df, x = ~Variable, y = ~Missing, type = "bar", text = ~paste0(Percent, "%"), textposition = "outside", marker = list(color = ifelse(missing_df$Percent > 0, "#ef4444", "#10b981"))) %>% layout(xaxis = list(title = "", categoryorder = "total descending"), yaxis = list(title = "Missing Count"))
        apply_pub_theme(p, title = "Missing Values by Variable", height = 400, legend = FALSE)
      })
      component_times$missing <- missing_profile$time
      output$eda_missing_plot <- renderPlotly({ missing_profile$result })
    } else {
      output$eda_missing_plot <- renderPlotly({ NULL })
    }
    
    # Summary statistics table - profiled
    if (has_section("summary_stats")) {
      summary_profile <- profile_expr({
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        if (length(numeric_cols) > 0) {
          summary_stats <- data.frame(
            Variable = numeric_cols,
            Mean = sapply(data[numeric_cols], function(x) round(mean(x, na.rm = TRUE), 2)),
            Median = sapply(data[numeric_cols], function(x) round(median(x, na.rm = TRUE), 2)),
            SD = sapply(data[numeric_cols], function(x) round(sd(x, na.rm = TRUE), 2)),
            Min = sapply(data[numeric_cols], function(x) round(min(x, na.rm = TRUE), 2)),
            Max = sapply(data[numeric_cols], function(x) round(max(x, na.rm = TRUE), 2)),
            Q1 = sapply(data[numeric_cols], function(x) round(quantile(x, 0.25, na.rm = TRUE), 2)),
            Q3 = sapply(data[numeric_cols], function(x) round(quantile(x, 0.75, na.rm = TRUE), 2)),
            Missing = sapply(data[numeric_cols], function(x) sum(is.na(x)))
          )
          datatable(summary_stats, options = list(pageLength = 10, scrollX = TRUE))
        }
      })
      component_times$summary_table <- summary_profile$time
      output$summary_stats_table <- renderDT({ summary_profile$result })
    } else {
      output$summary_stats_table <- renderDT({ NULL })
    }
    
    # AI-generated insights (profiled)
    if (has_section("ai_insights")) {
      ai_profile <- profile_expr({ generate_ai_insights(data, model = input$ollama_model) })
      component_times$ai_insights <- ai_profile$time
      output$ai_insights <- renderUI({ ai_profile$result })
    } else {
      output$ai_insights <- renderUI({ NULL })
    }
    
    # EDA summary
    if (has_section("quality_report")) {
      summary_ui_profile <- profile_expr({
        div(
          h5("Data Structure Summary"),
          p(paste("This dataset contains", nrow(data), "rows and", ncol(data), "columns.")),
          p(paste("Memory usage:", round(object.size(data) / 1024, 1), "KB")),
          h5("Variable Types"),
          p(paste("Numeric:", sum(sapply(data, is.numeric)), "|",
                  "Character:", sum(sapply(data, is.character)), "|",
                  "Factor:", sum(sapply(data, is.factor)), "|",
                  "Date:", sum(sapply(data, function(x) inherits(x, "Date"))))),
          h5("Missing Values"),
          p(paste("Total missing values:", sum(is.na(data)),
                  "(", round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 1), "%)"))
        )
      })
      component_times$summary_ui <- summary_ui_profile$time
      output$eda_summary <- renderUI({ summary_ui_profile$result })
    } else {
      output$eda_summary <- renderUI({ NULL })
    }

    # End profiling
    eda_elapsed <- (proc.time() - eda_start)[[3]]
    perf_stats(list(eda_time = eda_elapsed, component_times = component_times, sampled = sampled_info$sampled, sample_n = sampled_info$sample_n, original_n = sampled_info$original_n))

    showNotification("EDA Report generated!", type = "message")
  }
  
  # Refresh EDA button handler
  observeEvent(input$refresh_eda, {
    req(global_data$data)
    generate_eda_report()
  })

  # Run EDA on full dataset once (one-off exact run)
  observeEvent(input$run_eda_full, {
    req(global_data$data)
    showNotification("Running EDA on full dataset (one-off). This may take longer...", type = "message")
    tryCatch({
      generate_eda_report(force_full = TRUE)
      showNotification("One-off full EDA run complete.", type = "message")
    }, error = function(e) {
      showNotification(paste("Full EDA failed:", e$message), type = "error")
    })
  })
  
  # ============================================
  # EDA EXPAND CHART HANDLERS
  # ============================================
  
  # Expand Histograms
  observeEvent(input$expand_eda_histograms, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Distribution Analysis (Histograms)"),
      plotlyOutput("eda_histograms_large", height = "80vh")
    ))
    output$eda_histograms_large <- renderPlotly({
      data <- global_data$data
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      if (length(numeric_cols) > 0) {
        plots <- lapply(numeric_cols[seq_len(min(9, length(numeric_cols)))], function(col) {
          plot_ly(data, x = ~get(col), type = "histogram", 
                  marker = list(color = "#3b82f6", opacity = 0.75)) %>%
            layout(title = col, xaxis = list(title = col), yaxis = list(title = "Count"))
        })
        p <- subplot(plots, nrows = ceiling(length(plots)/3))
        apply_pub_theme(p, title = "Distribution Analysis", height = 720, legend = FALSE)
      }
    })
  })
  
  # Expand Density
  observeEvent(input$expand_eda_density, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Density Plots"),
      plotlyOutput("eda_density_large", height = "80vh")
    ))
    output$eda_density_large <- renderPlotly({
      data <- global_data$data
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      if (length(numeric_cols) > 0) {
        plots <- lapply(numeric_cols[seq_len(min(9, length(numeric_cols)))], function(col) {
          plot_ly(data, x = ~get(col), type = "histogram", histnorm = "probability density",
                  marker = list(color = "#8b5cf6", opacity = 0.5)) %>%
            layout(title = col, xaxis = list(title = col), yaxis = list(title = "Density"))
        })
        p <- subplot(plots, nrows = ceiling(length(plots)/3))
        apply_pub_theme(p, title = "Density Plots", height = 720, legend = FALSE)
      }
    })
  })
  
  # Expand Box Plots
  observeEvent(input$expand_eda_boxplots, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Box Plots"),
      plotlyOutput("eda_boxplots_large", height = "80vh")
    ))
    output$eda_boxplots_large <- renderPlotly({
      data <- global_data$data
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      plots <- lapply(numeric_cols[seq_len(min(8, length(numeric_cols)))], function(col) {
        plot_ly(data, y = ~get(col), type = "box", name = col, marker = list(color = "#3b82f6"))
      })
      p <- subplot(plots, nrows = 2, shareY = FALSE)
      apply_pub_theme(p, title = "Box Plots", height = 720, legend = FALSE)
    })
  })
  
  # Expand Violin Plots
  observeEvent(input$expand_eda_violin, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Violin Plots"),
      plotlyOutput("eda_violin_large", height = "80vh")
    ))
    output$eda_violin_large <- renderPlotly({
      data <- global_data$data
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      plots <- lapply(numeric_cols[seq_len(min(8, length(numeric_cols)))], function(col) {
        plot_ly(data, y = ~get(col), type = "violin", name = col,
                box = list(visible = TRUE), meanline = list(visible = TRUE),
                fillcolor = "#3b82f6", opacity = 0.6)
      })
      p <- subplot(plots, nrows = 2, shareY = FALSE)
      apply_pub_theme(p, title = "Violin Plots", height = 720, legend = FALSE)
    })
  })
  
  # Expand Scatter Matrix
  observeEvent(input$expand_eda_scatter, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Scatter Matrix"),
      plotlyOutput("eda_scatter_large", height = "80vh")
    ))
    output$eda_scatter_large <- renderPlotly({
      data <- global_data$data
      numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
      if (ncol(numeric_data) >= 2) {
        cols_to_use <- names(numeric_data)[seq_len(min(6, ncol(numeric_data)))]
        p <- plot_ly(numeric_data[cols_to_use]) %>%
          plotly::splom(dimensions = lapply(cols_to_use, function(col) list(label = col, values = ~get(col))),
                marker = list(color = "#3b82f6", size = 4, opacity = 0.6))
        apply_pub_theme(p, title = "Scatter Matrix", height = 720)
      }
    })
  })
  
  # Expand Correlation
  observeEvent(input$expand_eda_correlation, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Correlation Matrix"),
      plotlyOutput("eda_correlation_large", height = "80vh")
    ))
    output$eda_correlation_large <- renderPlotly({
      data <- global_data$data
      numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
      if (ncol(numeric_data) >= 2) {
        cor_matrix <- cor(numeric_data, use = "complete.obs")
      p <- plot_ly(x = names(numeric_data), y = names(numeric_data), z = cor_matrix, type = "heatmap",
        colors = grDevices::colorRampPalette(c("#2563eb", "#ffffff", "#ef4444"))(10))
      apply_pub_theme(p, title = "Correlation Matrix", height = 720)
      }
    })
  })
  
  # Expand Pie Charts
  observeEvent(input$expand_eda_pie, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Categorical Analysis (Pie)"),
      plotlyOutput("eda_pie_large", height = "80vh")
    ))
    output$eda_pie_large <- renderPlotly({
      data <- global_data$data
      categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      if (length(categorical_cols) >= 1) {
        plots <- lapply(categorical_cols[seq_len(min(4, length(categorical_cols)))], function(col) {
          value_counts <- table(data[[col]])
          plot_ly(labels = names(value_counts), values = as.numeric(value_counts), type = "pie",
                  textinfo = "label+percent", hole = 0.35,
                  marker = list(colors = c("#2563eb", "#7c3aed", "#f97316", "#14b8a6", "#f43f5e", "#0ea5e9"))) %>%
            layout(title = col)
        })
        p <- subplot(plots, nrows = 2)
        apply_pub_theme(p, title = "Categorical Distributions", height = 720)
      }
    })
  })
  
  # Expand Bar Charts
  observeEvent(input$expand_eda_bar, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Categorical Bar Charts"),
      plotlyOutput("eda_bar_large", height = "80vh")
    ))
    output$eda_bar_large <- renderPlotly({
      data <- global_data$data
      categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      if (length(categorical_cols) >= 1) {
        plots <- lapply(categorical_cols[seq_len(min(6, length(categorical_cols)))], function(col) {
          value_counts <- table(data[[col]])
          plot_ly(x = names(value_counts), y = as.numeric(value_counts), type = "bar",
                  marker = list(color = "#3b82f6", opacity = 0.85)) %>%
            layout(title = col, xaxis = list(title = ""), yaxis = list(title = "Count"))
        })
        p <- subplot(plots, nrows = 2, shareY = FALSE)
        apply_pub_theme(p, title = "Categorical Bar Charts", height = 720, legend = FALSE)
      }
    })
  })
  
  # Expand Q-Q Plots
  observeEvent(input$expand_eda_qq, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Q-Q Plots (Normality Check)"),
      plotlyOutput("eda_qq_large", height = "80vh")
    ))
    output$eda_qq_large <- renderPlotly({
      data <- global_data$data
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      if (length(numeric_cols) > 0) {
        plots <- lapply(numeric_cols[seq_len(min(6, length(numeric_cols)))], function(col) {
          qq_data <- qqnorm(data[[col]], plot.it = FALSE)
          plot_ly(x = qq_data$x, y = qq_data$y, type = "scatter", mode = "markers",
                  marker = list(color = "#3b82f6", size = 5, opacity = 0.6)) %>%
            add_trace(x = range(qq_data$x), y = range(qq_data$x) * sd(data[[col]], na.rm = TRUE) + mean(data[[col]], na.rm = TRUE),
                      type = "scatter", mode = "lines", line = list(color = "#ef4444", dash = "dash")) %>%
            layout(title = col, xaxis = list(title = "Theoretical"), yaxis = list(title = "Sample"), showlegend = FALSE)
        })
        p <- subplot(plots, nrows = 2)
        apply_pub_theme(p, title = "Normality Diagnostics", height = 720, legend = FALSE)
      }
    })
  })
  
  # Expand Missing Values
  observeEvent(input$expand_eda_missing, {
    req(global_data$data)
    showModal(chart_modal(
      title = tagList(icon("expand"), "Missing Values Analysis"),
      plotlyOutput("eda_missing_large", height = "80vh")
    ))
    output$eda_missing_large <- renderPlotly({
      data <- global_data$data
      missing_counts <- sapply(data, function(x) sum(is.na(x)))
      missing_df <- data.frame(Variable = names(missing_counts), Missing = missing_counts,
                               Percent = round(100 * missing_counts / nrow(data), 1))
      missing_df <- missing_df[order(-missing_df$Missing), ]
      p <- plot_ly(missing_df, x = ~Variable, y = ~Missing, type = "bar",
          text = ~paste0(Percent, "%"), textposition = "outside",
          marker = list(color = ifelse(missing_df$Percent > 0, "#ef4444", "#10b981"))) %>%
        layout(xaxis = list(title = "", categoryorder = "total descending"),
           yaxis = list(title = "Missing Count"))
      apply_pub_theme(p, title = "Missing Values by Variable", height = 720, legend = FALSE)
    })
  })
  
  # Expand Summary Table
  observeEvent(input$expand_eda_summary, {
    req(global_data$data)
    showModal(modalDialog(
      title = tagList(icon("expand"), "Summary Statistics"),
      size = "l",
      DTOutput("eda_summary_large"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    output$eda_summary_large <- renderDT({
      data <- global_data$data
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      if (length(numeric_cols) > 0) {
        summary_stats <- data.frame(
          Variable = numeric_cols,
          Mean = sapply(data[numeric_cols], function(x) round(mean(x, na.rm = TRUE), 3)),
          Median = sapply(data[numeric_cols], function(x) round(median(x, na.rm = TRUE), 3)),
          SD = sapply(data[numeric_cols], function(x) round(sd(x, na.rm = TRUE), 3)),
          Min = sapply(data[numeric_cols], function(x) round(min(x, na.rm = TRUE), 3)),
          Max = sapply(data[numeric_cols], function(x) round(max(x, na.rm = TRUE), 3)),
          Q1 = sapply(data[numeric_cols], function(x) round(quantile(x, 0.25, na.rm = TRUE), 3)),
          Q3 = sapply(data[numeric_cols], function(x) round(quantile(x, 0.75, na.rm = TRUE), 3)),
          IQR = sapply(data[numeric_cols], function(x) round(IQR(x, na.rm = TRUE), 3)),
          Missing = sapply(data[numeric_cols], function(x) sum(is.na(x)))
        )
        datatable(summary_stats, options = list(pageLength = 20, scrollX = TRUE))
      }
    })
  })
  
  # Expand Data Preview
  observeEvent(input$expand_eda_preview, {
    req(global_data$data)
    showModal(modalDialog(
      title = tagList(icon("expand"), "Data Preview (Full)"),
      size = "l",
      DTOutput("eda_preview_large"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    output$eda_preview_large <- renderDT({
      datatable(global_data$data, 
                options = list(pageLength = 25, scrollX = TRUE, scrollY = "60vh"),
                filter = "top")
    })
  })

  # Expand Sample Preview (shows the sampled subset in a modal)
  observeEvent(input$expand_sample_preview, {
    req(global_data$data)
    showModal(modalDialog(
      title = tagList(icon("expand"), "Sample Preview"),
      size = "l",
      DTOutput("eda_sample_preview_large"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    output$eda_sample_preview_large <- renderDT({
      # Use current sampling settings
      perf_threshold <- input$perf_sample_threshold %||% 50000
      perf_sample_size <- input$perf_sample_size %||% 50000
      perf_auto <- if (!is.null(input$perf_auto_sample)) input$perf_auto_sample else TRUE
      sampled_info <- get_analysis_data(global_data$data, threshold = perf_threshold, sample_size = perf_sample_size, auto_sample = perf_auto)
      datatable(sampled_info$data, options = list(pageLength = 25, scrollX = TRUE, scrollY = "60vh"), filter = "top")
    })
  })
  
  # ============================================
  # AI CHAT
  # ============================================

  output$chat_messages <- renderUI({
    div(class = "empty-state",
      tags$i(class = "fas fa-robot"),
      tags$h4("Start a conversation"),
      tags$p("Ask about trends, anomalies, transformations, or chart ideas based on your dataset.")
    )
  })
  
  # Build data context string for chat
  chat_data_context <- reactive({
    if (is.null(global_data$data)) return(NULL)
    d <- global_data$data
    paste0(
      "Dataset: ", nrow(d), " rows × ", ncol(d), " columns.\n",
      "Columns: ", paste(names(d), collapse = ", "), ".\n",
      "Types: ", paste(paste0(names(d), " (", sapply(d, function(x) class(x)[1]), ")"), collapse = ", "), ".\n",
      "Sample values (first row): ", paste(paste0(names(d), "=", sapply(d, function(x) as.character(x[1]))), collapse = "; ")
    )
  })
  
  # Render context panel in sidebar
  output$chat_context <- renderUI({
    if (is.null(global_data$data)) {
      return(div(style = "color: var(--text-muted); font-size: 13px; padding: 8px 0;",
        tags$i(class = "fas fa-info-circle"), " No dataset loaded. Load data to give the AI context."
      ))
    }
    d <- global_data$data
    num_cols <- sum(sapply(d, is.numeric))
    cat_cols <- ncol(d) - num_cols
    tagList(
      div(style = "font-size: 13px; color: var(--text-secondary); line-height: 1.7;",
        div(tags$i(class = "fas fa-table", style = "color: var(--accent-primary); margin-right: 6px;"),
            tags$strong(paste0(nrow(d), " rows × ", ncol(d), " cols"))),
        div(tags$i(class = "fas fa-hashtag", style = "color: var(--accent-success); margin-right: 6px;"),
            paste0(num_cols, " numeric")),
        div(tags$i(class = "fas fa-font", style = "color: var(--accent-warning); margin-right: 6px;"),
            paste0(cat_cols, " categorical")),
        div(style = "margin-top: 8px; font-size: 12px; color: var(--text-muted);",
            tags$i(class = "fas fa-check-circle", style = "color: var(--accent-success); margin-right: 4px;"),
            "AI has full data context")
      )
    )
  })
  
  observeEvent(input$send_message, {
    req(input$chat_input)
    req(input$chat_input != "")
    
    message <- input$chat_input
    
    # Add user message to history
    global_data$chat_history <- c(global_data$chat_history, list(list(
      role = "user",
      content = message,
      timestamp = Sys.time()
    )))
    
    # Get AI response
    response <- ollama_chat(message, history = global_data$chat_history,
                           model = input$ollama_model, 
                           temperature = input$temperature,
                           max_tokens = input$max_tokens,
                           data_context = chat_data_context())
    
    # Add AI response to history
    global_data$chat_history <- c(global_data$chat_history, list(list(
      role = "assistant",
      content = response,
      timestamp = Sys.time()
    )))
    
    # Update chat display
    output$chat_messages <- renderUI({
      messages <- lapply(global_data$chat_history, function(msg) {
        if (msg$role == "user") {
          div(class = "chat-bubble-row user-row", style = "display: flex; justify-content: flex-end; margin: 8px 0;",
              div(class = "chat-bubble chat-user",
                  format_chat_text(msg$content))
          )
        } else {
          div(class = "chat-bubble-row ai-row", style = "display: flex; justify-content: flex-start; margin: 8px 0;",
              div(class = "chat-bubble-avatar", style = "width: 28px; height: 28px; border-radius: 50%; background: var(--gradient-primary); display: flex; align-items: center; justify-content: center; font-size: 12px; color: white; margin-right: 10px; flex-shrink: 0; margin-top: 4px;",
                  tags$i(class = "fas fa-robot")),
              div(class = "chat-bubble chat-ai",
                  format_chat_text(msg$content))
          )
        }
      })
      do.call(tagList, messages)
    })
    
    # Clear input
    updateTextAreaInput(session, "chat_input", value = "")
    
    # Scroll to bottom (wrapped in tryCatch in case shinyjs not available)
    tryCatch({
      shinyjs::runjs("
        var chatDiv = document.querySelector('[id^=\"chat_messages\"]');
        if (chatDiv) {
          chatDiv.scrollTop = chatDiv.scrollHeight;
        }
      ")
    }, error = function(e) {
      # Silently ignore if shinyjs not available
    })
  })
  
  observeEvent(input$clear_chat, {
    global_data$chat_history <- list()
    output$chat_messages <- renderUI({
      div(class = "empty-state",
        tags$i(class = "fas fa-comments"),
        tags$h4("Chat cleared"),
        tags$p("Start a new conversation whenever you're ready.")
      )
    })
  })
  
  # ============================================
  # DATA TRANSFORMATION
  # ============================================
  
  output$transform_ui <- renderUI({
    req(input$transform_type)
    
    switch(input$transform_type,
           "filter" = tagList(
             selectInput("filter_col", "Column:", choices = names(global_data$data)),
             selectInput("filter_op", "Operator:", 
                        choices = c("==" = "==", "!=" = "!=", ">" = ">", 
                                   "<" = "<", ">=" = ">=", "<=" = "<=",
                                   "%in%" = "%in%", "is.na" = "is.na")),
             textInput("filter_val", "Value:", placeholder = "e.g., 'Male' or 100")
           ),
           "select" = tagList(
             checkboxGroupInput("select_cols", "Select Columns:", 
                               choices = names(global_data$data),
                               selected = names(global_data$data)[seq_len(min(5, ncol(global_data$data)))])
           ),
           "rename" = tagList(
             selectInput("rename_old", "Column to Rename:", choices = names(global_data$data)),
             textInput("rename_new", "New Name:", placeholder = "Enter new column name")
           ),
           "mutate_type" = tagList(
             selectInput("mutate_col", "Column:", choices = names(global_data$data)),
             selectInput("mutate_new_type", "New Type:",
                        choices = c("numeric", "character", "factor", "Date", "logical"))
           ),
           "missing" = tagList(
             selectInput("missing_action", "Action:",
                        choices = c("Remove rows with NA" = "remove",
                                   "Fill NA with 0" = "zero",
                                   "Fill NA with mean" = "mean",
                                   "Fill NA with median" = "median",
                                   "Fill NA with mode" = "mode")),
             selectInput("missing_col", "Column (optional):", 
                        choices = c("All columns" = "all", names(global_data$data)))
           ),
           "new_var" = tagList(
             textInput("new_var_name", "New Variable Name:", placeholder = "e.g., log_sales"),
             textAreaInput("new_var_expr", "Expression:", 
                          placeholder = "e.g., log(sales + 1)",
                          rows = 3)
           ),
           "group_summarize" = tagList(
             selectInput("group_by_col", "Group By:", choices = names(global_data$data)),
             selectInput("summarize_cols", "Summarize Columns:", 
                        choices = names(global_data$data), multiple = TRUE),
             selectInput("summarize_fun", "Function:",
                        choices = c("mean", "sum", "median", "min", "max", "sd", "n"))
           ),
           "sort" = tagList(
             selectInput("sort_col", "Sort By:", choices = names(global_data$data)),
             selectInput("sort_dir", "Direction:", choices = c("Ascending" = "asc", "Descending" = "desc"))
           ),
           "join" = tagList(
             fileInput("join_file", "Upload Second Dataset:",
                      accept = c(".csv", ".tsv", ".xlsx", ".rds")),
             selectInput("join_type", "Join Type:",
                        choices = c("Inner Join" = "inner", 
                                   "Left Join" = "left",
                                   "Right Join" = "right",
                                   "Full Join" = "full")),
             textInput("join_by", "Join By (comma-separated):", placeholder = "e.g., id, name")
           )
    )
  })
  
  observeEvent(input$apply_transform, {
    req(global_data$data)
    req(input$transform_type)
    
    tryCatch({
      data <- global_data$data
      transform_code <- ""
      
      switch(input$transform_type,
             "filter" = {
               col <- input$filter_col
               op <- input$filter_op
               val <- input$filter_val
               
               if (op == "%in%") {
                 vals <- strsplit(val, ",")[[1]]
                 vals <- trimws(gsub("['\"]", "", vals))
                 data <- data %>% filter(get(col) %in% !!vals)
                 transform_code <- glue::glue("data %>% filter({col} %in% c({paste(vals, collapse=', ')}))")
               } else if (op == "is.na") {
                 data <- data %>% filter(is.na(get(col)))
                 transform_code <- glue::glue("data %>% filter(is.na({col}))")
               } else {
                 if (is.numeric(data[[col]])) {
                   val_num <- as.numeric(val)
                   expr_str <- paste(col, op, val_num)
                   data <- data %>% filter(!!rlang::parse_expr(expr_str))
                   transform_code <- glue::glue("data %>% filter({col} {op} {val_num})")
                 } else {
                   expr_str <- paste(col, op, shQuote(val))
                   data <- data %>% filter(!!rlang::parse_expr(expr_str))
                   transform_code <- glue::glue("data %>% filter({col} {op} '{val}')")
                 }
               }
             },
             "select" = {
               data <- data %>% select(all_of(input$select_cols))
               transform_code <- glue::glue("data %>% select({paste(input$select_cols, collapse=', ')})")
             },
             "rename" = {
               data <- data %>% rename(!!input$rename_new := !!input$rename_old)
               transform_code <- glue::glue("data %>% rename({input$rename_new} = {input$rename_old})")
             },
             "mutate_type" = {
               col <- input$mutate_col
               new_type <- input$mutate_new_type
               
               if (new_type == "numeric") {
                 data[[col]] <- as.numeric(data[[col]])
               } else if (new_type == "character") {
                 data[[col]] <- as.character(data[[col]])
               } else if (new_type == "factor") {
                 data[[col]] <- as.factor(data[[col]])
               } else if (new_type == "Date") {
                 data[[col]] <- as.Date(data[[col]])
               } else if (new_type == "logical") {
                 data[[col]] <- as.logical(data[[col]])
               }
               transform_code <- glue::glue("data <- data %>% mutate({col} = as.{new_type}({col}))")
             },
             "missing" = {
               if (input$missing_col == "all") {
                 if (input$missing_action == "remove") {
                   data <- data %>% drop_na()
                   transform_code <- "data %>% drop_na()"
                 } else if (input$missing_action == "zero") {
                   data <- data %>% mutate(across(everything(), ~replace_na(., 0)))
                   transform_code <- "data %>% mutate(across(everything(), ~replace_na(., 0)))"
                 } else if (input$missing_action == "mean") {
                   data <- data %>% mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))
                   transform_code <- "data %>% mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))"
                 } else if (input$missing_action == "median") {
                   data <- data %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))
                   transform_code <- "data %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))"
                 }
               } else {
                 col <- input$missing_col
                 if (input$missing_action == "remove") {
                   data <- data %>% drop_na(!!sym(col))
                   transform_code <- glue::glue("data %>% drop_na({col})")
                 } else if (input$missing_action == "zero") {
                   data <- data %>% mutate(!!sym(col) := replace_na(!!sym(col), 0))
                   transform_code <- glue::glue("data %>% mutate({col} = replace_na({col}, 0))")
                 } else if (input$missing_action == "mean") {
                   data <- data %>% mutate(!!sym(col) := replace_na(!!sym(col), mean(!!sym(col), na.rm = TRUE)))
                   transform_code <- glue::glue("data %>% mutate({col} = replace_na({col}, mean({col}, na.rm = TRUE)))")
                 }
               }
             },
             "new_var" = {
               data <- data %>% mutate(!!input$new_var_name := !!rlang::parse_expr(input$new_var_expr))
               transform_code <- glue::glue("data %>% mutate({input$new_var_name} = {input$new_var_expr})")
             },
             "group_summarize" = {
               data <- data %>% 
                 group_by(!!sym(input$group_by_col)) %>%
                 summarize(across(all_of(input$summarize_cols), 
                                .fns = list(mean = mean, sum = sum), 
                                na.rm = TRUE, .names = "{.col}_{.fn}"))
               transform_code <- glue::glue("data %>% group_by({input$group_by_col}) %>% summarize(across({paste(input$summarize_cols, collapse=', ')}, {input$summarize_fun}, na.rm = TRUE))")
             },
             "sort" = {
               if (input$sort_dir == "asc") {
                 data <- data %>% arrange(!!sym(input$sort_col))
                 transform_code <- glue::glue("data %>% arrange({input$sort_col})")
               } else {
                 data <- data %>% arrange(desc(!!sym(input$sort_col)))
                 transform_code <- glue::glue("data %>% arrange(desc({input$sort_col}))")
               }
             },
             "join" = {
               if (!is.null(input$join_file)) {
                 join_data <- switch(tolower(tools::file_ext(input$join_file$name)),
                                    "csv" = utils::read.csv(input$join_file$datapath),
                                    "xlsx" = readxl::read_excel(input$join_file$datapath),
                                    "rds" = readRDS(input$join_file$datapath))
                 
                 join_by_cols <- strsplit(input$join_by, ",")[[1]] %>% trimws()
                 
                 if (input$join_type == "inner") {
                   data <- data %>% inner_join(join_data, by = join_by_cols)
                 } else if (input$join_type == "left") {
                   data <- data %>% left_join(join_data, by = join_by_cols)
                 } else if (input$join_type == "right") {
                   data <- data %>% right_join(join_data, by = join_by_cols)
                 } else if (input$join_type == "full") {
                   data <- data %>% full_join(join_data, by = join_by_cols)
                 }
                 transform_code <- glue::glue("data %>% {input$join_type}_join(join_data, by = c({paste(paste0('\"', join_by_cols, '\"'), collapse=', ')}))")
               }
             }
      )
      
      # Update transformed data
      global_data$transformed_data <- data
      
      # Show code
      output$transform_code <- renderText({
        transform_code
      })
      
      # Show preview
      output$transformed_preview <- renderDT({
        datatable(head(data, 100), options = list(pageLength = 10))
      })
      
      showNotification("Transformation applied!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error applying transformation:", e$message), type = "error")
    })
  })
  
  observeEvent(input$reset_data, {
    global_data$transformed_data <- global_data$original_data
    output$transformed_preview <- renderDT({
      datatable(head(global_data$original_data, 100), options = list(pageLength = 10))
    })
    output$transform_code <- renderText("# Data reset to original")
    showNotification("Data reset to original!", type = "message")
  })
  
  # ============================================
  # DOWNLOAD HANDLERS
  # ============================================
  
  # ============================================
  # EXPORT TAB
  # ============================================
  
  # Export preview
  output$export_preview <- renderDT({
    req(global_data$data)
    data_to_export <- if (isTRUE(input$export_transformed) && !is.null(global_data$transformed_data)) {
      global_data$transformed_data
    } else {
      global_data$data
    }
    datatable(head(data_to_export, 100), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Export summary
  output$export_summary <- renderUI({
    req(global_data$data)
    data_to_export <- if (isTRUE(input$export_transformed) && !is.null(global_data$transformed_data)) {
      global_data$transformed_data
    } else {
      global_data$data
    }
    
    div(
      tags$p(tags$strong("Rows: "), nrow(data_to_export)),
      tags$p(tags$strong("Columns: "), ncol(data_to_export)),
      tags$p(tags$strong("Size: "), format(object.size(data_to_export), units = "auto")),
      tags$p(tags$strong("Format: "), toupper(input$export_format))
    )
  })
  
  # Export history (stored in reactiveVal)
  export_history <- reactiveVal(list())
  
  output$export_history <- renderUI({
    history <- export_history()
    if (length(history) == 0) {
      return(p(class = "text-muted", "No exports yet"))
    }
    tags$ul(
      lapply(history, function(h) {
        tags$li(paste(h$filename, "-", h$time))
      })
    )
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      ext <- input$export_format
      paste0("data_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      data_to_export <- if (isTRUE(input$export_transformed) && !is.null(global_data$transformed_data)) {
        global_data$transformed_data
      } else {
        global_data$data
      }
      
      switch(input$export_format,
        "csv" = write.csv(data_to_export, file, row.names = FALSE),
        "xlsx" = writexl::write_xlsx(data_to_export, file),
        "rds" = saveRDS(data_to_export, file),
        "json" = jsonlite::write_json(data_to_export, file),
        "tsv" = write.table(data_to_export, file, sep = "\t", row.names = FALSE)
      )
      
      # Update export history
      history <- export_history()
      history <- c(history, list(list(filename = basename(file), time = format(Sys.time(), "%H:%M:%S"))))
      export_history(history)
      
      showNotification("Data exported successfully!", type = "message")
    }
  )
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      switch(input$report_type,
        "eda_html" = paste0("EDA_Report_", Sys.Date(), ".html"),
        "stats_csv" = paste0("Summary_Stats_", Sys.Date(), ".csv"),
        "corr_csv" = paste0("Correlation_Matrix_", Sys.Date(), ".csv"),
        "profile_html" = paste0("Data_Profile_", Sys.Date(), ".html")
      )
    },
    content = function(file) {
      req(global_data$data)
      data <- global_data$data
      
      switch(input$report_type,
        "eda_html" = {
          # Generate basic HTML report
          html_content <- paste0(
            "<html><head><title>EDA Report</title>",
            "<style>body{font-family:Arial,sans-serif;padding:20px;} table{border-collapse:collapse;width:100%;} th,td{border:1px solid #ddd;padding:8px;text-align:left;} th{background:#667eea;color:white;}</style></head>",
            "<body><h1>EDA Report - ", Sys.Date(), "</h1>",
            "<h2>Dataset Overview</h2>",
            "<p>Rows: ", nrow(data), " | Columns: ", ncol(data), "</p>",
            "<h2>Summary Statistics</h2>",
            "<pre>", paste(capture.output(summary(data)), collapse = "\n"), "</pre>",
            "</body></html>"
          )
          writeLines(html_content, file)
        },
        "stats_csv" = {
          numeric_cols <- names(data)[sapply(data, is.numeric)]
          if (length(numeric_cols) > 0) {
            stats <- data.frame(
              Variable = numeric_cols,
              Mean = sapply(data[numeric_cols], mean, na.rm = TRUE),
              Median = sapply(data[numeric_cols], median, na.rm = TRUE),
              SD = sapply(data[numeric_cols], sd, na.rm = TRUE),
              Min = sapply(data[numeric_cols], min, na.rm = TRUE),
              Max = sapply(data[numeric_cols], max, na.rm = TRUE)
            )
            write.csv(stats, file, row.names = FALSE)
          }
        },
        "corr_csv" = {
          numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
          if (ncol(numeric_data) >= 2) {
            cor_matrix <- cor(numeric_data, use = "complete.obs")
            write.csv(cor_matrix, file)
          }
        },
        "profile_html" = {
          html_content <- paste0(
            "<html><head><title>Data Profile</title>",
            "<style>body{font-family:Arial,sans-serif;padding:20px;} table{border-collapse:collapse;width:100%;margin-bottom:20px;} th,td{border:1px solid #ddd;padding:8px;} th{background:#667eea;color:white;}</style></head>",
            "<body><h1>Data Profile</h1>",
            "<h2>Column Summary</h2><table><tr><th>Column</th><th>Type</th><th>Non-NA</th><th>Unique</th></tr>",
            paste(sapply(names(data), function(col) {
              paste0("<tr><td>", col, "</td><td>", class(data[[col]])[1], "</td><td>", sum(!is.na(data[[col]])), "</td><td>", length(unique(data[[col]])), "</td></tr>")
            }), collapse = ""),
            "</table></body></html>"
          )
          writeLines(html_content, file)
        }
      )
      showNotification("Report generated!", type = "message")
    }
  )
  
  # Download chart
  output$download_chart <- downloadHandler(
    filename = function() {
      fmt <- if (!is.null(input$chart_export_format) && nzchar(input$chart_export_format)) {
        input$chart_export_format
      } else if (!is.null(input$chart_format) && nzchar(input$chart_format)) {
        input$chart_format
      } else {
        "png"
      }
      paste0("chart_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
    },
    content = function(file) {
      req(global_data$charts$main)
      p <- global_data$charts$main

      fmt <- if (!is.null(input$chart_format) && nzchar(input$chart_format)) input$chart_format else (input$export_format %||% "png")
      width <- input$chart_export_width %||% input$export_width %||% 6
      height <- input$chart_export_height %||% input$export_height %||% 4
      dpi <- input$chart_export_dpi %||% 300
      theme <- input$chart_theme %||% "default"
      export_theme <- if (theme == "default") "apa" else theme

      if (fmt == "html") {
        htmlwidgets::saveWidget(p, file, selfcontained = TRUE)
        showNotification("Chart exported!", type = "message")
        return(invisible(NULL))
      }

      p_export <- apply_publication_theme(p, theme = export_theme, font_size = 12)
      tryCatch({
        export_publication_quality(p_export, file, width = width, height = height, dpi = dpi, format = fmt)
        showNotification("Chart exported!", type = "message")
      }, error = function(e) {
        showNotification(paste("Chart export failed:", e$message), type = "error")
      })
    }
  )
  
  # ============================================
  # OUTLIER DETECTION TAB
  # ============================================
  
  # Update outlier variable choices when data loads
  observe({
    req(global_data$data)
    numeric_cols <- names(global_data$data)[sapply(global_data$data, is.numeric)]
    updateSelectInput(session, "outlier_var", choices = numeric_cols)
  })
  
  # Store outlier results
  outlier_results <- reactiveVal(NULL)
  
  # Detect outliers
  observeEvent(input$detect_outliers_btn, {
    req(global_data$data)
    req(input$outlier_var)
    
    data <- global_data$data
    col <- input$outlier_var
    vals <- data[[col]]
    vals <- vals[!is.na(vals)]
    
    # Calculate bounds based on method
    bounds <- switch(input$outlier_method,
      "iqr" = {
        q1 <- quantile(vals, 0.25)
        q3 <- quantile(vals, 0.75)
        iqr <- q3 - q1
        list(lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr)
      },
      "iqr3" = {
        q1 <- quantile(vals, 0.25)
        q3 <- quantile(vals, 0.75)
        iqr <- q3 - q1
        list(lower = q1 - 3 * iqr, upper = q3 + 3 * iqr)
      },
      "zscore2" = {
        mu <- mean(vals)
        sigma <- sd(vals)
        list(lower = mu - 2 * sigma, upper = mu + 2 * sigma)
      },
      "zscore3" = {
        mu <- mean(vals)
        sigma <- sd(vals)
        list(lower = mu - 3 * sigma, upper = mu + 3 * sigma)
      },
      "mad" = {
        med <- median(vals)
        mad_val <- mad(vals)
        list(lower = med - 3 * mad_val, upper = med + 3 * mad_val)
      },
      "iforest" = {
        # Simple percentile-based approximation for isolation forest
        list(lower = quantile(vals, 0.01), upper = quantile(vals, 0.99))
      }
    )
    
    # Find outliers
    outlier_mask <- data[[col]] < bounds$lower | data[[col]] > bounds$upper
    outlier_mask[is.na(outlier_mask)] <- FALSE
    outlier_indices <- which(outlier_mask)
    
    outlier_results(list(
      column = col,
      method = input$outlier_method,
      lower = bounds$lower,
      upper = bounds$upper,
      indices = outlier_indices,
      count = length(outlier_indices),
      pct = round(100 * length(outlier_indices) / nrow(data), 2)
    ))
    
    showNotification(paste("Found", length(outlier_indices), "outliers"), type = "message")
  })
  
  # Outlier stats outputs
  output$outlier_count <- renderUI({
    res <- outlier_results()
    div(class = "value", if (!is.null(res)) res$count else "-")
  })
  
  output$outlier_pct <- renderUI({
    res <- outlier_results()
    div(class = "value", if (!is.null(res)) paste0(res$pct, "%") else "-")
  })
  
  output$outlier_lower <- renderUI({
    res <- outlier_results()
    div(class = "value", if (!is.null(res)) round(res$lower, 2) else "-")
  })
  
  output$outlier_upper <- renderUI({
    res <- outlier_results()
    div(class = "value", if (!is.null(res)) round(res$upper, 2) else "-")
  })
  
  # Outlier plot
  output$outlier_plot <- renderPlotly({
    req(global_data$data)
    req(input$outlier_var)
    
    data <- global_data$data
    col <- input$outlier_var
    res <- outlier_results()
    
    # Create box plot with outliers highlighted
    p <- plotly::plot_ly(x = numeric(0), y = numeric(0),
                         type = "scatter", mode = "markers",
                         marker = list(opacity = 0),
                         showlegend = FALSE, hoverinfo = "skip") %>%
      add_boxplot(y = data[[col]], name = col, boxpoints = "outliers",
                  marker = list(color = "#667eea"),
                  line = list(color = "#667eea")) %>%
      layout(
        title = paste("Outlier Analysis:", col),
        yaxis = list(title = col),
        showlegend = FALSE
      )
    
    # Add bounds lines if available
    if (!is.null(res)) {
      p <- p %>%
        add_trace(y = c(res$lower, res$lower), x = c(-0.5, 0.5), type = "scatter", mode = "lines",
                  line = list(color = "#ef4444", dash = "dash"), name = "Lower Bound") %>%
        add_trace(y = c(res$upper, res$upper), x = c(-0.5, 0.5), type = "scatter", mode = "lines",
                  line = list(color = "#ef4444", dash = "dash"), name = "Upper Bound")
    }
    
    p
  })
  
  # Outlier table
  output$outlier_table <- renderDT({
    req(global_data$data)
    res <- outlier_results()
    
    if (is.null(res) || length(res$indices) == 0) {
      return(datatable(data.frame(Message = "No outliers detected or click 'Detect Outliers' first")))
    }
    
    outlier_data <- global_data$data[res$indices, , drop = FALSE]
    outlier_data$Row <- res$indices
    outlier_data <- outlier_data[, c("Row", setdiff(names(outlier_data), "Row"))]
    
    datatable(outlier_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Expand outlier chart
  observeEvent(input$expand_outlier_chart, {
    req(global_data$data)
    req(input$outlier_var)
    
    showModal(chart_modal(
      title = tagList(icon("expand"), "Outlier Analysis"),
      plotlyOutput("outlier_plot_large", height = "80vh")
    ))
    
    output$outlier_plot_large <- renderPlotly({
      data <- global_data$data
      col <- input$outlier_var
      res <- outlier_results()
      
      # Create histogram with outliers highlighted
      outlier_mask <- if (!is.null(res)) seq_len(nrow(data)) %in% res$indices else rep(FALSE, nrow(data))
      
      p <- plotly::plot_ly(x = numeric(0), y = numeric(0),
                           type = "scatter", mode = "markers",
                           marker = list(opacity = 0),
                           showlegend = FALSE, hoverinfo = "skip") %>%
        add_histogram(x = data[[col]][!outlier_mask], name = "Normal", 
                     marker = list(color = "#667eea", opacity = 0.7)) %>%
        add_histogram(x = data[[col]][outlier_mask], name = "Outliers",
                     marker = list(color = "#ef4444", opacity = 0.8)) %>%
        layout(
          title = paste("Distribution with Outliers Highlighted:", col),
          xaxis = list(title = col),
          yaxis = list(title = "Count"),
          barmode = "overlay"
        )
      
      if (!is.null(res)) {
        p <- p %>%
          add_trace(x = c(res$lower, res$lower), y = c(0, max(table(cut(data[[col]], 30)))), 
                   type = "scatter", mode = "lines",
                   line = list(color = "#ef4444", dash = "dash", width = 2), name = "Lower Bound") %>%
          add_trace(x = c(res$upper, res$upper), y = c(0, max(table(cut(data[[col]], 30)))),
                   type = "scatter", mode = "lines",
                   line = list(color = "#ef4444", dash = "dash", width = 2), name = "Upper Bound")
      }
      
      p
    })
  })
  
  # Apply outlier action
  observeEvent(input$apply_outlier_action, {
    req(global_data$data)
    res <- outlier_results()
    req(res)
    req(length(res$indices) > 0)
    
    data <- global_data$data
    col <- res$column
    
    data <- switch(input$outlier_action,
      "remove" = {
        data[-res$indices, ]
      },
      "cap" = {
        data[[col]][data[[col]] < res$lower] <- res$lower
        data[[col]][data[[col]] > res$upper] <- res$upper
        data
      },
      "na" = {
        data[[col]][res$indices] <- NA
        data
      },
      "median" = {
        med <- median(data[[col]], na.rm = TRUE)
        data[[col]][res$indices] <- med
        data
      },
      "mean" = {
        mu <- mean(data[[col]], na.rm = TRUE)
        data[[col]][res$indices] <- mu
        data
      }
    )
    
    global_data$data <- data
    global_data$transformed_data <- data
    outlier_results(NULL)
    
    showNotification(paste("Applied action:", input$outlier_action), type = "message")
  })
  
  # Reset outlier data
  observeEvent(input$reset_outliers, {
    req(global_data$original_data)
    global_data$data <- global_data$original_data
    global_data$transformed_data <- global_data$original_data
    outlier_results(NULL)
    showNotification("Data reset to original", type = "message")
  })
  
  output$download_eda_report <- downloadHandler(
    filename = function() {
      paste0("EDA_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Generate HTML report
      rmarkdown::render(system.file("app/eda_report_template.Rmd", package = "DataExplorerPro"),
                       output_file = file,
                       params = list(data = global_data$data))
    }
  )
  
  # ============================================
  # CORRELATION & OUTLIER DETECTION
  # ============================================
  
  observeEvent(input$show_correlations, {
    req(global_data$data)
    updateTabsetPanel(session, "main_tabs", selected = "eda")
    generate_eda_report()
  })
  
  observeEvent(input$detect_outliers, {
    req(global_data$data)
    
    data <- global_data$data
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    outliers <- lapply(numeric_cols, function(col) {
      vals <- data[[col]]
      q1 <- quantile(vals, 0.25, na.rm = TRUE)
      q3 <- quantile(vals, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      outlier_count <- sum(vals < lower | vals > upper, na.rm = TRUE)
      list(column = col, outliers = outlier_count, pct = round(100 * outlier_count / sum(!is.na(vals)), 1))
    })
    
    showModal(modalDialog(
      title = "Outlier Detection Results",
      tableOutput("outlier_table"),
      easyClose = TRUE
    ))
    
    output$outlier_table <- renderTable({
      do.call(rbind, outliers)
    })
  })
  
  # ============================================
  # DASHBOARD TAB - NEW FEATURE
  # ============================================
  
  # Dashboard reactive values
  dashboard_data <- reactiveValues(
    template = NULL,
    config = NULL
  )

  build_dashboard_ui <- function(template) {
    div(class = "dashboard-grid",
      style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px;",
      lapply(template$layout$panels, function(panel) {
        div(class = "dashboard-panel",
          style = paste0("grid-column: ", panel$col, " / span ", panel$width, "; ",
                         "grid-row: ", panel$row, " / span ", panel$height, ";"),
          h5(panel$title),
          if (panel$type == "stat") {
            renderUI({
              data <- global_data$data
              if (is.null(data)) {
                return(div(class = "stat-value", "-"))
              }
              value <- switch(panel$title,
                              "Total Records" = nrow(data),
                              "Variables" = ncol(data),
                              "Missing %" = round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 1),
                              "Numeric Vars" = sum(sapply(data, is.numeric)),
                              "N/A")
              div(class = "stat-value", value)
            })
          } else if (panel$type == "chart") {
            plotlyOutput(paste0("dashboard_", panel$id), height = "220px")
          } else if (panel$type == "table") {
            DTOutput(paste0("dashboard_", panel$id), height = "220px")
          } else if (panel$type == "network") {
            div(class = "text-muted", "Network panel not available in this build.")
          }
        )
      })
    )
  }

  render_dashboard_from_template <- function(template) {
    output$dashboard_content <- renderUI({ build_dashboard_ui(template) })
    output$dashboard_content_full <- renderUI({ build_dashboard_ui(template) })

    if (is.null(global_data$data)) return(invisible(NULL))
    data <- global_data$data
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    cat_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

    for (panel in template$layout$panels) {
      local({
        p <- panel
        out_id <- paste0("dashboard_", p$id)
        if (p$type == "chart") {
          output[[out_id]] <- renderPlotly({
            ct <- p$chart_type
            if (ct == "histogram") {
              if (length(numeric_cols) < 1) return(empty_plot("Need numeric columns"))
              create_histogram(data, numeric_cols[1])
            } else if (ct == "heatmap") {
              if (length(numeric_cols) < 2) return(empty_plot("Need 2 numeric columns"))
              create_heatmap(data, numeric_cols[1], numeric_cols[2])
            } else if (ct == "scatter") {
              if (length(numeric_cols) < 2) return(empty_plot("Need 2 numeric columns"))
              create_scatter_plot(data, numeric_cols[1], numeric_cols[2])
            } else if (ct == "box") {
              if (length(numeric_cols) < 1) return(empty_plot("Need numeric columns"))
              if (length(cat_cols) >= 1) {
                create_box_plot(data, cat_cols[1], numeric_cols[1])
              } else {
                data2 <- data
                data2$.__all__ <- "All"
                create_box_plot(data2, ".__all__", numeric_cols[1])
              }
            } else if (ct == "bar") {
              x_var <- if (length(cat_cols) >= 1) cat_cols[1] else names(data)[1]
              y_var <- if (length(numeric_cols) >= 1) numeric_cols[1] else ""
              create_bar_chart(data, x_var, y_var)
            } else if (ct == "line") {
              if (length(numeric_cols) < 2) return(empty_plot("Need 2 numeric columns"))
              create_line_chart(data, numeric_cols[1], numeric_cols[2])
            } else {
              empty_plot("No chart available")
            }
          })
        } else if (p$type == "table") {
          output[[out_id]] <- renderDT({
            datatable(head(data, 50), options = list(pageLength = 10, scrollX = TRUE))
          })
        }
      })
    }
  }
  
  # Load dashboard template
  observeEvent(input$load_dashboard_template, {
    req(input$dashboard_template)
    
    tryCatch({
      source("../../R/dashboard_engine.R")
      template <- create_dashboard_template(input$dashboard_template)
      dashboard_data$template <- template
      
      # Render dashboard content and charts/tables
      render_dashboard_from_template(template)
      
      # Render template info
      output$dashboard_template_info <- renderUI({
        div(
          h6(template$name),
          p(template$description),
          tags$ul(
            tags$li("Panels: ", length(template$layout$panels)),
            tags$li("Grid: ", template$layout$rows, " rows × ", template$layout$cols, " columns")
          )
        )
      })
      
      showNotification("Dashboard template loaded!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading template:", e$message), type = "error")
    })
  })

  observeEvent(input$refresh_dashboard, {
    req(dashboard_data$template)
    render_dashboard_from_template(dashboard_data$template)
    showNotification("Dashboard refreshed.", type = "message")
  })

  observe({
    if (isTRUE(input$dashboard_auto_refresh)) {
      invalidateLater(30000, session)
      if (!is.null(dashboard_data$template)) {
        render_dashboard_from_template(dashboard_data$template)
      }
    }
  })

  observeEvent(input$apply_dashboard_config, {
    req(input$upload_dashboard_config)
    tryCatch({
      source("../../R/dashboard_engine.R")
      cfg <- load_dashboard_config(input$upload_dashboard_config$datapath)
      dashboard_data$template <- cfg
      render_dashboard_from_template(cfg)
      showNotification("Dashboard config applied.", type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to load config:", e$message), type = "error")
    })
  })

  observeEvent(input$dashboard_fullscreen, {
    req(dashboard_data$template)
    showModal(modalDialog(
      title = tagList(icon("expand"), "Dashboard (Fullscreen)"),
      class = "modal-fullscreen",
      uiOutput("dashboard_content_full"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$edit_dashboard_layout, {
    req(dashboard_data$template)
    showModal(modalDialog(
      title = "Edit Dashboard Layout (JSON)",
      size = "l",
      textAreaInput("dashboard_config_json", NULL,
                    value = jsonlite::toJSON(dashboard_data$template, pretty = TRUE, auto_unbox = TRUE),
                    rows = 18),
      footer = tagList(
        actionButton("apply_dashboard_json", "Apply", class = "btn-primary"),
        modalButton("Close")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$apply_dashboard_json, {
    req(input$dashboard_config_json)
    tryCatch({
      cfg <- jsonlite::fromJSON(input$dashboard_config_json, simplifyVector = TRUE)
      dashboard_data$template <- cfg
      render_dashboard_from_template(cfg)
      removeModal()
      showNotification("Dashboard layout updated.", type = "message")
    }, error = function(e) {
      showNotification(paste("Invalid JSON:", e$message), type = "error")
    })
  })
  
  # Export dashboard as HTML
  output$download_dashboard_html <- downloadHandler(
    filename = function() {
      paste0(input$dashboard_name, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(dashboard_data$template)
      source("../../R/dashboard_engine.R")
      
      dash_data <- list(
        title = input$dashboard_name,
        panels = dashboard_data$template$layout$panels
      )
      
      export_dashboard_html(dash_data, file)
      showNotification("Dashboard exported!", type = "message")
    }
  )
  
  # Save dashboard config
  output$download_dashboard_config <- downloadHandler(
    filename = function() {
      paste0(input$dashboard_name, "_config.json")
    },
    content = function(file) {
      req(dashboard_data$template)
      source("../../R/dashboard_engine.R")
      save_dashboard_config(dashboard_data$template, file)
      showNotification("Config saved!", type = "message")
    }
  )
  
  # ============================================
  # NETWORK TAB - NEW FEATURE
  # ============================================
  
  # Sankey variable selectors
  output$sankey_source_selector <- renderUI({
    req(global_data$data)
    categorical_cols <- names(global_data$data)[sapply(global_data$data, 
                                                        function(x) is.character(x) || is.factor(x))]
    selectInput("sankey_source", "Source Variable:", choices = categorical_cols)
  })
  
  output$sankey_target_selector <- renderUI({
    req(global_data$data)
    categorical_cols <- names(global_data$data)[sapply(global_data$data, 
                                                        function(x) is.character(x) || is.factor(x))]
    selectInput("sankey_target", "Target Variable:", choices = categorical_cols)
  })
  
  output$sankey_value_selector <- renderUI({
    req(global_data$data)
    numeric_cols <- names(global_data$data)[sapply(global_data$data, is.numeric)]
    selectInput("sankey_value", "Value (Weight):", 
                choices = c("Count" = "", numeric_cols))
  })
  
  # Create network
  observeEvent(input$create_network_btn, {
    req(global_data$data)
    req(input$network_type)
    
    tryCatch({
      source("../../R/network_viz.R")
      data <- global_data$data
      
      network <- switch(input$network_type,
        "correlation" = {
          create_correlation_network(data, 
                                    cor_threshold = input$cor_threshold,
                                    method = input$cor_method,
                                    layout = input$network_layout)
        },
        "sankey" = {
          req(input$sankey_source, input$sankey_target)
          create_sankey_diagram(data, input$sankey_source, input$sankey_target, 
                               if(input$sankey_value != "") input$sankey_value else NULL)
        },
        "custom" = {
          showNotification("Custom network requires edge list data", type = "info")
          NULL
        }
      )
      
      if (!is.null(network)) {
        output$network_output <- renderUI({
          if (input$network_type == "correlation") {
            if (requireNamespace("visNetwork", quietly = TRUE)) {
              visNetwork::visNetworkOutput("network_viz", height = "600px")
            } else {
              div(class = "alert alert-warning",
                "Install 'visNetwork' package: install.packages('visNetwork')")
            }
          } else if (input$network_type == "sankey") {
            if (requireNamespace("networkD3", quietly = TRUE)) {
              networkD3::sankeyNetworkOutput("network_viz", height = "600px")
            } else {
              div(class = "alert alert-warning",
                "Install 'networkD3' package: install.packages('networkD3')")
            }
          }
        })
        
        output$network_viz <- if (input$network_type == "correlation") {
          visNetwork::renderVisNetwork({ network })
        } else {
          networkD3::renderSankeyNetwork({ network })
        }
        
        showNotification("Network created!", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error creating network:", e$message), type = "error")
    })
  })
  
  # Calculate network metrics
  observeEvent(input$calculate_metrics_btn, {
    req(global_data$data)
    req(input$network_type == "correlation")
    
    tryCatch({
      source("../../R/network_viz.R")
      metrics <- calculate_network_metrics(global_data$data, 
                                           cor_threshold = input$cor_threshold)
      
      output$network_metrics_table <- renderDT({
        datatable(metrics, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      showNotification("Network metrics calculated!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error calculating metrics:", e$message), type = "error")
    })
  })
  
} # End of server function

