# DataExplorerPro - Network Visualization Engine
# Correlation networks, graph analysis, and relationship visualization

#' Create Correlation Network Graph
#'
#' Creates an interactive network graph showing correlations between variables
#'
#' @param data Data frame with numeric variables
#' @param cor_threshold Minimum absolute correlation to show edge (0-1)
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @param layout Network layout algorithm ("circle", "fr", "kk", "sphere")
#' @export
create_correlation_network <- function(data, cor_threshold = 0.3, 
                                        method = "pearson", 
                                        layout = "fr") {
  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for network visualizations. Please install it.")
  }
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required for interactive networks. Please install it.")
  }
  
  # Select only numeric columns
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) < 2) {
    stop("Need at least 2 numeric variables to create correlation network")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = method)
  
  # Create edge list from correlation matrix
  edges <- data.frame(
    from = character(),
    to = character(),
    weight = numeric(),
    correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(nrow(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      cor_val <- cor_matrix[i, j]
      if (abs(cor_val) >= cor_threshold && !is.na(cor_val)) {
        edges <- rbind(edges, data.frame(
          from = rownames(cor_matrix)[i],
          to = colnames(cor_matrix)[j],
          weight = abs(cor_val),
          correlation = cor_val,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(edges) == 0) {
    stop("No correlations above threshold. Try lowering cor_threshold.")
  }
  
  # Create nodes data frame
  nodes <- data.frame(
    id = colnames(numeric_data),
    label = colnames(numeric_data),
    title = paste("Variable:", colnames(numeric_data)),
    stringsAsFactors = FALSE
  )
  
  # Calculate node degrees for sizing
  node_degree <- table(c(edges$from, edges$to))
  nodes$value <- ifelse(nodes$id %in% names(node_degree), 
                        as.numeric(node_degree[nodes$id]), 0)
  nodes$value <- nodes$value * 5 + 10  # Scale for visualization
  
  # Color nodes by clustering if possible
  if (nrow(edges) >= 3) {
    g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
    communities <- igraph::cluster_louvain(g)
    nodes$group <- igraph::membership(communities)[nodes$id]
  } else {
    nodes$group <- 1
  }
  
  # Prepare edges for visNetwork
  edges$value <- edges$weight * 5  # Scale edge width
  edges$title <- sprintf("Correlation: %.3f", edges$correlation)
  edges$color <- ifelse(edges$correlation > 0, 
                        list(color = "#10b981", highlight = "#059669"),
                        list(color = "#ef4444", highlight = "#dc2626"))
  edges$smooth <- TRUE
  
  # Create interactive network
  network <- visNetwork::visNetwork(nodes, edges, height = "600px", width = "100%") %>%
    visNetwork::visNodes(
      shape = "dot",
      font = list(size = 14, color = "#1f2937"),
      borderWidth = 2,
      borderWidthSelected = 4
    ) %>%
    visNetwork::visEdges(
      smooth = list(enabled = TRUE, type = "continuous"),
      width = 2
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1),
      nodesIdSelection = TRUE
    ) %>%
    visNetwork::visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -50),
      stabilization = list(iterations = 100)
    ) %>%
    visNetwork::visInteraction(
      navigationButtons = TRUE,
      hover = TRUE
    ) %>%
    visNetwork::visLegend(
      useGroups = TRUE,
      position = "right"
    )
  
  network
}

#' Create Network from Adjacency Matrix
#'
#' Create a network graph from a custom adjacency matrix
#'
#' @param adjacency_matrix Square matrix representing connections
#' @param threshold Minimum value to show edge
#' @param directed Whether edges are directed
#' @export
create_network_from_matrix <- function(adjacency_matrix, threshold = 0, 
                                        directed = FALSE) {
  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for network visualizations. Please install it.")
  }
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required for interactive networks. Please install it.")
  }
  
  # Create igraph object
  g <- igraph::graph_from_adjacency_matrix(
    adjacency_matrix,
    mode = if (directed) "directed" else "undirected",
    weighted = TRUE
  )
  
  # Remove edges below threshold
  if (threshold > 0) {
    g <- igraph::delete_edges(g, which(igraph::E(g)$weight < threshold))
  }
  
  # Convert to visNetwork format
  network_data <- visNetwork::toVisNetworkData(g)
  
  # Enhance nodes
  network_data$nodes$title <- paste("Node:", network_data$nodes$id)
  network_data$nodes$value <- igraph::degree(g) * 5 + 10
  
  # Enhance edges
  network_data$edges$title <- sprintf("Weight: %.2f", network_data$edges$weight)
  network_data$edges$value <- network_data$edges$weight * 2
  
  # Create visualization
  visNetwork::visNetwork(
    nodes = network_data$nodes,
    edges = network_data$edges,
    height = "600px",
    width = "100%"
  ) %>%
    visNetwork::visNodes(shape = "dot", font = list(size = 14)) %>%
    visNetwork::visEdges(arrows = if (directed) "to" else NULL) %>%
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visNetwork::visPhysics(solver = "forceAtlas2Based") %>%
    visNetwork::visInteraction(navigationButtons = TRUE)
}

#' Calculate Network Metrics
#'
#' Calculate various network centrality and connectivity metrics
#'
#' @param data Data frame with numeric variables (for correlation network)
#' @param cor_threshold Correlation threshold for edge creation
#' @return Data frame with network metrics
#' @export
calculate_network_metrics <- function(data, cor_threshold = 0.3) {
  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for network analysis. Please install it.")
  }
  
  # Select numeric columns
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  
  # Apply threshold
  adj_matrix <- ifelse(abs(cor_matrix) >= cor_threshold & row(cor_matrix) != col(cor_matrix), 
                       abs(cor_matrix), 0)
  
  # Create graph
  g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected", 
                                            weighted = TRUE, diag = FALSE)
  
  # Calculate metrics
  metrics <- data.frame(
    variable = colnames(numeric_data),
    degree = igraph::degree(g),
    betweenness = igraph::betweenness(g, normalized = TRUE),
    closeness = igraph::closeness(g, normalized = TRUE),
    eigenvector = igraph::eigen_centrality(g)$vector,
    stringsAsFactors = FALSE
  )
  
  # Add clustering coefficient if possible
  if (igraph::vcount(g) > 2) {
    metrics$clustering <- igraph::transitivity(g, type = "local")
  }
  
  # Sort by degree (most connected variables first)
  metrics <- metrics[order(-metrics$degree), ]
  
  metrics
}

#' Create Sankey Diagram
#'
#' Create a flow/sankey diagram for categorical relationships
#'
#' @param data Data frame with categorical variables
#' @param source_var Source node column
#' @param target_var Target node column
#' @param value_var Optional value/weight column
#' @export
create_sankey_diagram <- function(data, source_var, target_var, value_var = NULL) {
  
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("Package 'networkD3' is required for Sankey diagrams. Please install it.")
  }
  
  # Create flow data
  if (!is.null(value_var) && value_var != "") {
    flows <- data %>%
      group_by(!!sym(source_var), !!sym(target_var)) %>%
      summarise(value = sum(!!sym(value_var), na.rm = TRUE), .groups = "drop")
  } else {
    flows <- data %>%
      group_by(!!sym(source_var), !!sym(target_var)) %>%
      summarise(value = n(), .groups = "drop")
  }
  
  # Create node list
  nodes <- data.frame(
    name = unique(c(as.character(flows[[source_var]]), 
                    as.character(flows[[target_var]]))),
    stringsAsFactors = FALSE
  )
  
  # Create links with node indices
  flows$source_id <- match(flows[[source_var]], nodes$name) - 1
  flows$target_id <- match(flows[[target_var]], nodes$name) - 1
  
  # Create Sankey diagram
  networkD3::sankeyNetwork(
    Links = flows,
    Nodes = nodes,
    Source = "source_id",
    Target = "target_id",
    Value = "value",
    NodeID = "name",
    fontSize = 12,
    nodeWidth = 30,
    sinksRight = FALSE,
    height = 500,
    width = 800
  )
}

#' Create Force-Directed Graph
#'
#' Create a force-directed graph layout for general network data
#'
#' @param nodes Data frame with node information (must have 'id' column)
#' @param edges Data frame with edge information (must have 'from' and 'to' columns)
#' @export
create_force_directed_graph <- function(nodes, edges) {
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required for network visualizations. Please install it.")
  }
  
  # Validate input
  if (!"id" %in% names(nodes)) {
    stop("Nodes data frame must have an 'id' column")
  }
  
  if (!all(c("from", "to") %in% names(edges))) {
    stop("Edges data frame must have 'from' and 'to' columns")
  }
  
  # Add default label if not present
  if (!"label" %in% names(nodes)) {
    nodes$label <- nodes$id
  }
  
  # Calculate node sizes based on degree if not specified
  if (!"value" %in% names(nodes)) {
    degree_count <- table(c(edges$from, edges$to))
    nodes$value <- ifelse(nodes$id %in% names(degree_count),
                          as.numeric(degree_count[nodes$id]) * 5 + 10,
                          10)
  }
  
  # Create network
  visNetwork::visNetwork(nodes, edges, height = "600px", width = "100%") %>%
    visNetwork::visNodes(
      shape = "dot",
      font = list(size = 14),
      scaling = list(min = 10, max = 50)
    ) %>%
    visNetwork::visEdges(
      smooth = list(enabled = TRUE, type = "continuous"),
      arrows = "to"
    ) %>%
    visNetwork::visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50,
        centralGravity = 0.01,
        springLength = 100,
        springConstant = 0.08
      ),
      stabilization = list(iterations = 200)
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE),
      nodesIdSelection = TRUE
    ) %>%
    visNetwork::visInteraction(
      navigationButtons = TRUE,
      hover = TRUE,
      zoomView = TRUE
    )
}

#' Detect Network Structure in Data
#'
#' Attempt to detect potential network relationships in data
#'
#' @param data Data frame to analyze
#' @return List with detected network potential
#' @export
detect_network_structure <- function(data) {
  
  # Check for numeric correlations
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  has_correlations <- length(numeric_cols) >= 2
  
  # Check for categorical relationships
  categorical_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  has_categorical <- length(categorical_cols) >= 2
  
  # Check for potential edge list (from/to columns)
  col_names_lower <- tolower(names(data))
  has_from <- any(grepl("from|source|sender", col_names_lower))
  has_to <- any(grepl("to|target|receiver|destination", col_names_lower))
  is_edge_list <- has_from && has_to
  
  list(
    can_create_correlation_network = has_correlations,
    numeric_variables = numeric_cols,
    can_create_sankey = has_categorical,
    categorical_pairs = if (length(categorical_cols) >= 2) {
      combn(categorical_cols, 2, simplify = FALSE)
    } else {
      list()
    },
    is_edge_list = is_edge_list,
    suggested_visualization = if (has_correlations) {
      "correlation_network"
    } else if (has_categorical) {
      "sankey"
    } else if (is_edge_list) {
      "force_directed"
    } else {
      "none"
    }
  )
}
