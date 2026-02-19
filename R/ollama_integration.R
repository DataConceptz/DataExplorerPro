# DataExplorerPro - Ollama Integration Module
# AI-Powered Data Exploration Studio

#' Check Ollama Connection
#'
#' Verifies if Ollama is running and accessible.
#'
#' @param base_url Ollama server URL (default: http://localhost:11434)
#' @return Logical indicating connection status
#'
#' @export
check_ollama_connection <- function(base_url = "http://localhost:11434") {
  tryCatch({
    response <- httr::GET(
      paste0(base_url, "/api/tags"),
      httr::timeout(5)
    )
    return(httr::status_code(response) == 200)
  }, error = function(e) {
    return(FALSE)
  })
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
#' @param base_url Ollama server URL
#' @return Character string with AI response
#'
#' @export
ollama_chat <- function(message, history = NULL, model = "llama3.2",
                       temperature = 0.7, max_tokens = 2000,
                       base_url = "http://localhost:11434") {
  
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
      "Always format code blocks properly with R syntax highlighting.",
      "When generating R code, use tidyverse/dplyr syntax when possible."
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
      paste0(base_url, "/api/chat"),
      body = body,
      encode = "json",
      httr::content_type_json(),
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
    return(paste("Connection error:", e$message, ". Make sure Ollama is running on", base_url))
  })
}

#' Generate R Code from Natural Language
#'
#' Converts a natural language query into executable R code.
#'
#' @param query Natural language query
#' @param data_context Description of the dataset
#' @param task_type Type of task (transform, visualize, stats, general)
#' @param model Ollama model to use
#' @return List with code, explanation, and insights
#'
#' @export
generate_r_code <- function(query, data_context, task_type = "general",
                           model = "llama3.2") {
  
  prompt <- switch(task_type,
                   "transform" = paste0(
                     "You are an R data transformation expert using dplyr.\n\n",
                     "Query: '", query, "'\n\n",
                     "Data context: ", data_context, "\n\n",
                     "Return ONLY a JSON object (no markdown) with this structure:\n",
                     '{\n  "code": "complete dplyr code using %>% syntax",\n',
                     '  "explanation": "brief explanation of what the code does",\n',
                     '  "insights": ["key finding 1", "key finding 2"]\n',
                     "}\n\n",
                     "Example for 'filter rows where sales > 1000':\n",
                     '{\n  "code": "data %>% filter(sales > 1000)",\n',
                     '  "explanation": "Filters the dataset to only include rows where sales exceed 1000",\n',
                     '  "insights": ["This filters the dataset", "Reduces data to high-value transactions"]\n',
                     "}"
                   ),
                   "visualize" = paste0(
                     "You are an R visualization expert using ggplot2 and plotly.\n\n",
                     "Query: '", query, "'\n\n",
                     "Data context: ", data_context, "\n\n",
                     "Return ONLY a JSON object (no markdown) with this structure:\n",
                     '{\n  "code": "complete ggplot2 or plotly code",\n',
                     '  "explanation": "brief explanation of the visualization",\n',
                     '  "insights": ["what the chart reveals 1", "what the chart reveals 2"]\n',
                     "}\n\n",
                     "Example for 'scatter plot of price vs weight':\n",
                     '{\n  "code": "ggplot(data, aes(x = weight, y = price)) + geom_point() + theme_minimal()",\n',
                     '  "explanation": "Creates a scatter plot showing relationship between weight and price",\n',
                     '  "insights": ["Shows positive correlation", "Outliers visible at high values"]\n',
                     "}"
                   ),
                   "stats" = paste0(
                     "You are a statistical analysis expert in R.\n\n",
                     "Query: '", query, "'\n\n",
                     "Data context: ", data_context, "\n\n",
                     "Return ONLY a JSON object (no markdown) with this structure:\n",
                     '{\n  "code": "complete R statistical code",\n',
                     '  "explanation": "brief explanation of the statistical method",\n',
                     '  "insights": ["interpretation 1", "interpretation 2"]\n',
                     "}"
                   ),
                   paste0(
                     "You are a data analysis assistant in R.\n\n",
                     "Query: '", query, "'\n\n",
                     "Data context: ", data_context, "\n\n",
                     "Return ONLY a JSON object (no markdown) with this structure:\n",
                     '{\n  "code": "complete R code to answer the query",\n',
                     '  "explanation": "brief explanation",\n',
                     '  "insights": ["finding 1", "finding 2"]\n',
                     "}"
                   )
  )
  
  response <- ollama_chat(prompt, model = model)
  
  # Parse JSON response
  tryCatch({
    # Clean up response (remove markdown code blocks if present)
    clean_response <- gsub("```json\\s*", "", response)
    clean_response <- gsub("```\\s*", "", clean_response)
    clean_response <- trimws(clean_response)
    
    result <- jsonlite::fromJSON(clean_response)
    
    return(list(
      code = result$code %||% "# No code generated",
      explanation = result$explanation %||% "No explanation available",
      insights = result$insights %||% list()
    ))
  }, error = function(e) {
    # If parsing fails, return raw response
    return(list(
      code = "# Could not parse response. Please try rephrasing your query.",
      explanation = response,
      insights = list()
    ))
  })
}

#' Generate Data Insights
#'
#' Uses AI to generate insights about a dataset.
#'
#' @param data A data frame
#' @param model Ollama model to use
#' @return Character vector of insights
#'
#' @export
generate_data_insights <- function(data, model = "llama3.2") {
  
  # Create data summary
  data_summary <- paste0(
    "Dataset: ", nrow(data), " rows, ", ncol(data), " columns\n",
    "Columns: ", paste(names(data), collapse = ", "), "\n",
    "Column types: ", paste(sapply(data, function(x) class(x)[1]), collapse = ", "), "\n",
    "Missing values: ", sum(is.na(data)), " (", round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 1), "%)\n"
  )
  
  # Add numeric summaries
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) > 0) {
    data_summary <- paste0(data_summary, "\nNumeric columns summary:\n")
    for (col in numeric_cols[1:min(5, length(numeric_cols))]) {
      data_summary <- paste0(data_summary, 
                            col, ": mean=", round(mean(data[[col]], na.rm = TRUE), 2),
                            ", sd=", round(sd(data[[col]], na.rm = TRUE), 2),
                            ", range=[", round(min(data[[col]], na.rm = TRUE), 2), 
                            ", ", round(max(data[[col]], na.rm = TRUE), 2), "]\n")
    }
  }
  
  prompt <- paste0(
    "You are a data analysis expert. Generate 5 key insights about this dataset.\n\n",
    data_summary, "\n\n",
    "Focus on:\n",
    "1. Patterns and trends\n",
    "2. Anomalies or outliers\n",
    "3. Correlations between variables\n",
    "4. Data quality issues\n",
    "5. Recommendations for further analysis\n\n",
    "Return as numbered list, one insight per line, starting with numbers 1-5."
  )
  
  response <- ollama_chat(prompt, model = model)
  
  # Parse insights
  insights <- strsplit(response, "\n")[[1]]
  insights <- insights[grepl("^\\d", insights) | grepl("^[•-]", insights)]
  
  return(insights)
}

#' Suggest Visualizations
#'
#' AI-powered visualization recommendations for a dataset.
#'
#' @param data A data frame
#' @param goal Analysis goal or question
#' @param model Ollama model to use
#' @return List of suggested visualizations
#'
#' @export
suggest_visualizations <- function(data, goal = "explore", model = "llama3.2") {
  
  data_context <- paste0(
    "Dataset: ", nrow(data), " rows, ", ncol(data), " columns\n",
    "Columns: ", paste(names(data), collapse = ", "), "\n",
    "Types: ", paste(sapply(data, function(x) class(x)[1]), collapse = ", ")
  )
  
  prompt <- paste0(
    "You are a data visualization expert. Suggest 5 visualization types for this dataset.\n\n",
    data_context, "\n\n",
    "Goal: ", goal, "\n\n",
    "Return as JSON array:\n",
    '[\n  {"type": "chart_type", "purpose": "what it shows", "code": "ggplot2 code"},\n  ...\n]\n\n',
    "Use ggplot2 syntax. Include variables from the dataset."
  )
  
  response <- ollama_chat(prompt, model = model)
  
  tryCatch({
    clean_response <- gsub("```json\\s*", "", response)
    clean_response <- gsub("```\\s*", "", clean_response)
    result <- jsonlite::fromJSON(clean_response)
    return(result)
  }, error = function(e) {
    return(list(
      list(type = "scatter", purpose = "Explore relationships", code = "ggplot(data, aes(x, y)) + geom_point()"),
      list(type = "histogram", purpose = "Show distributions", code = "ggplot(data, aes(x)) + geom_histogram()")
    ))
  })
}

#' Explain Statistical Results
#'
#' Converts statistical output into plain English explanations.
#'
#' @param test_name Name of the statistical test
#' @param results Results from the test (list or data frame)
#' @param model Ollama model to use
#' @return Character explanation
#'
#' @export
explain_stat_results <- function(test_name, results, model = "llama3.2") {
  
  results_json <- jsonlite::toJSON(results, auto_unbox = TRUE)
  
  prompt <- paste0(
    "Explain these statistical results in plain English:\n\n",
    "Test: ", test_name, "\n",
    "Results: ", results_json, "\n\n",
    "Include:\n",
    "- What the test does\n",
    "- Key findings (numbers in context)\n",
    "- Statistical significance interpretation\n",
    "- Practical implications\n\n",
    "Keep it concise and beginner-friendly."
  )
  
  return(ollama_chat(prompt, model = model))
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
      httr::content_type_json(),
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

#' List Available Models
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

# Helper: null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
