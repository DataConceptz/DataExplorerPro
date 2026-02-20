# DataExplorerPro - Main Application Entry Point
# AI-Powered Data Exploration Studio for RStudio

#' DataExplorerPro: AI-Powered Data Exploration Studio
#'
#' A comprehensive RStudio add-in for interactive data exploration,
#' automatic EDA reports, and natural language queries using Ollama.
#'
#' @section Available functions:
#' \code{\link{run_app}} - Launch the DataExplorerPro interface
#' \code{\link{generate_eda_report}} - Generate comprehensive EDA report
#' \code{\link{query_data_natural_language}} - Query data using natural language
#' \code{\link{ollama_chat}} - Chat with Ollama about your data
#'
#' @docType package
#' @name DataExplorerPro
#' @aliases DataExplorerPro-package
NULL

#' Query Data Using Natural Language
#'
#' Converts a natural language query into R code and executes it.
#'
#' @param query Natural language query
#' @param data A data frame to query
#' @param task_type Type of task (transform, visualize, stats, general)
#' @param model Ollama model to use
#' @return List with code, explanation, and result
#'
#' @examples
#' \dontrun{
#' result <- query_data_natural_language(
#'   "Show me the top 10 customers by sales",
#'   data = my_data
#' )
#' print(result$code)
#' print(result$explanation)
#' }
#'
#' @export
query_data_natural_language <- function(query, data, task_type = "transform",
                                        model = "llama3.2") {
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  data_context <- paste0(
    "Dataset has ", nrow(data), " rows and ", ncol(data), " columns. ",
    "Columns: ", paste(names(data), collapse = ", "), ". ",
    "Column types: ", paste(paste(names(data), sapply(data, function(x) class(x)[1]), sep = "="), collapse = "; ")
  )
  
  result <- generate_r_code(query, data_context, task_type, model)
  
  # Try to execute the code
  tryCatch({
    if (grepl("^data %>%|^data$", result$code)) {
      data_result <- eval(parse(text = result$code))
      result$data_result <- data_result
    }
  }, error = function(e) {
    result$execution_error <- e$message
  })
  
  return(result)
}

#' Generate Comprehensive EDA Report
#'
#' Creates a complete exploratory data analysis report for a dataset.
#'
#' @param data A data frame to analyze
#' @param output_format Output format (html, rmarkdown)
#' @param include_plots Whether to include visualizations
#' @param ai_insights Whether to generate AI-powered insights
#' @param model Ollama model for AI insights
#' @param open_browser Whether to open report in browser (default: TRUE)
#' @return Path to generated report
#'
#' @examples
#' \dontrun{
#' # Generate HTML report
#' report_path <- generate_eda_report(my_data)
#'
#' # Generate without AI insights
#' report_path <- generate_eda_report(my_data, ai_insights = FALSE)
#' }
#'
#' @export
generate_eda_report <- function(data, output_format = "html", 
                                include_plots = TRUE,
                                ai_insights = TRUE,
                                model = "llama3.2",
                                open_browser = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Generate report based on format
  if (output_format == "html") {
    report_path <- generate_html_eda_report(data, include_plots, ai_insights, model)
    
    if (open_browser && interactive()) {
      browseURL(report_path)
    }
    
    return(invisible(report_path))
  } else {
    stop("Only 'html' format is currently supported. Use run_app() for interactive EDA.")
  }
}

#' Chat with Ollama
#'
#' Sends a message to Ollama and returns the response.
#'
#' @param message The message to send
#' @param history Conversation history (list of messages)
#' @param model Ollama model to use (default: llama3.2)
#' @param temperature Temperature for generation (0-1, default: 0.7)
#' @param max_tokens Maximum tokens in response (default: 2000)
#' @return Character string with AI response
#'
#' @examples
#' \dontrun{
#' response <- ollama_chat("How do I filter data in R?")
#' cat(response)
#' }
#'
#' @export
ollama_chat <- function(message, history = NULL, model = "llama3.2",
                       temperature = 0.7, max_tokens = 2000) {
  
  # Build messages array
  messages <- list()
  
  # Add system prompt
  messages[[1]] <- list(
    role = "system",
    content = paste(
      "You are DataExplorerPro, an AI assistant for data analysis in R.",
      "You help users explore data, create visualizations, and write R code.",
      "Be concise, helpful, and provide accurate R code when requested.",
      "If asked about data, ask for more context if needed.",
      "Always format code blocks properly with R syntax highlighting."
    )
  )
  
  # Add history
  if (!is.null(history) && length(history) > 0) {
    for (h in history) {
      if (is.list(h) && "role" %in% names(h) && "content" %in% names(h)) {
        messages[[length(messages) + 1]] <- list(
          role = h$role,
          content = h$content
        )
      }
    }
  }
  
  # Add current message
  messages[[length(messages) + 1]] <- list(
    role = "user",
    content = message
  )
  
  # Build request body
  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_tokens = max_tokens,
    stream = FALSE
  )
  
  # Make API call
  tryCatch({
    response <- httr::POST(
      "http://localhost:11434/api/chat",
      body = body,
      encode = "json",
      content_type_json(),
      httr::timeout(60)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "parsed")
      if (!is.null(result$message$content)) {
        return(result$message$content)
      } else {
        return("Sorry, I couldn't generate a response. Please try again.")
      }
    } else if (httr::status_code(response) == 404) {
      return(paste("Model '", model, "' not found. Please pull it with: ollama pull", model))
    } else {
      return(paste("Error:", httr::status_code(response), "- Please check Ollama is running."))
    }
  }, error = function(e) {
    return(paste("Connection error:", e$message, ". Make sure Ollama is running on http://localhost:11434"))
  })
}

#' Check Ollama Connection
#'
#' Verifies if Ollama is running and accessible.
#'
#' @param base_url Ollama server URL (default: http://localhost:11434)
#' @return List with connection status and available models
#'
#' @examples
#' \dontrun{
#' status <- get_ollama_status()
#' if (status$connected) {
#'   message("Ollama is ready with ", length(status$models), " models")
#' } else {
#'   message("Start Ollama first: ollama serve")
#' }
#' }
#'
#' @export
get_ollama_status <- function(base_url = "http://localhost:11434") {
  tryCatch({
    response <- httr::POST(
      paste0(base_url, "/api/tags"),
      httr::timeout(5)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "parsed")
      models <- sapply(result$models, function(x) x$name)
      list(
        connected = TRUE,
        running = TRUE,
        models = models,
        needs_pull = length(models) == 0
      )
    } else {
      list(connected = FALSE, running = FALSE, models = character(0), needs_pull = FALSE)
    }
  }, error = function(e) {
    list(connected = FALSE, running = FALSE, models = character(0), needs_pull = FALSE, error = e$message)
  })
}

#' @rdname get_ollama_status
#' @export
check_ollama_connection <- function(base_url = "http://localhost:11434") {
  status <- get_ollama_status(base_url)
  status$connected
}

#' List Available Ollama Models
#'
#' Returns list of available Ollama models.
#'
#' @param base_url Ollama server URL
#' @return Character vector of model names
#'
#' @export
list_ollama_models <- function(base_url = "http://localhost:11434") {
  
  tryCatch({
    response <- httr::POST(
      paste0(base_url, "/api/tags"),
      httr::timeout(5)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "parsed")
      models <- sapply(result$models, function(x) x$name)
      return(models)
    }
  }, error = function(e) {
    return(character(0))
  })
}

#' Pull Ollama Model
#'
#' Downloads a model for use with Ollama.
#'
#' @param model Model name to pull
#' @param base_url Ollama server URL
#' @return Invisible NULL
#'
#' @export
pull_ollama_model <- function(model = "llama3.2",
                              base_url = "http://localhost:11434") {
  
  body <- list(name = model)
  
  tryCatch({
    response <- httr::POST(
      paste0(base_url, "/api/pull"),
      body = body,
      encode = "json",
      content_type_json(),
      httr::timeout(300)
    )
    
    if (httr::status_code(response) == 200) {
      message(paste("Successfully pulled model:", model))
    } else {
      message(paste("Failed to pull model:", httr::status_code(response)))
    }
  }, error = function(e) {
    message(paste("Error pulling model:", e$message))
  })
  
  invisible(NULL)
}

#' Detect Outliers in Data
#'
#' Identifies outliers in numeric columns using IQR method.
#'
#' @param data A data frame
#' @param column Column name to analyze
#' @param method Outlier detection method (iqr, zscore)
#' @param threshold Threshold for detection (default: 1.5 for IQR)
#' @return List with outlier information
#'
#' @export
detect_outliers <- function(data, column, method = "iqr", threshold = 1.5) {
  
  if (!column %in% names(data)) {
    stop("Column not found in data")
  }
  
  vals <- data[[column]]
  
  if (method == "iqr") {
    q1 <- quantile(vals, 0.25, na.rm = TRUE)
    q3 <- quantile(vals, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - threshold * iqr
    upper <- q3 + threshold * iqr
    
    outlier_mask <- vals < lower | vals > upper
    
    list(
      column = column,
      method = "IQR",
      threshold = threshold,
      lower_bound = lower,
      upper_bound = upper,
      n_outliers = sum(outlier_mask, na.rm = TRUE),
      percentage = round(100 * sum(outlier_mask, na.rm = TRUE) / sum(!is.na(vals)), 2),
      outlier_indices = which(outlier_mask),
      outlier_values = vals[outlier_mask]
    )
  } else if (method == "zscore") {
    z_scores <- abs(scale(vals))
    outlier_mask <- z_scores > threshold
    
    list(
      column = column,
      method = "Z-Score",
      threshold = threshold,
      n_outliers = sum(outlier_mask, na.rm = TRUE),
      percentage = round(100 * sum(outlier_mask, na.rm = TRUE) / sum(!is.na(vals)), 2),
      outlier_indices = which(outlier_mask),
      outlier_values = vals[outlier_mask]
    )
  }
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

# Package version
VERSION <- "1.0.0"
