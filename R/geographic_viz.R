# Geographic module removed — retained as placeholder to avoid accidental imports
# If you need geographic features again, re-add a proper mapping module and dependencies (leaflet)

stop_geographic_module_removed <- function(...) {
  stop("Geographic visualizations have been removed from this build. If you need them, re-enable the geographic module and install leaflet.")
}

#' Create Bubble Map
#'
#' Create an interactive bubble map with sized markers
#'
#' @param data Data frame with coordinates and values
#' @param lat_var Column name for latitude
#' @param lon_var Column name for longitude
#' @param size_var Column name for bubble size
#' @param color_var Optional column for bubble color
#' @param label_var Optional column for labels
#' @export
create_bubble_map <- function(data, lat_var, lon_var, size_var, 
                               color_var = NULL, label_var = NULL) {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for geographic visualizations. Please install it.")
  }
  
  # Create base map
  map <- leaflet::leaflet(data) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
  
  # Determine colors
  if (!is.null(color_var) && color_var != "") {
    if (is.numeric(data[[color_var]])) {
      pal <- leaflet::colorNumeric("viridis", domain = data[[color_var]])
      colors <- pal(data[[color_var]])
    } else {
      pal <- leaflet::colorFactor("Set3", domain = data[[color_var]])
      colors <- pal(data[[color_var]])
    }
  } else {
    colors <- "#667eea"
  }
  
  # Create labels
  if (!is.null(label_var) && label_var != "") {
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      data[[label_var]],
      size_var,
      format(data[[size_var]], big.mark = ",")
    ) %>% lapply(htmltools::HTML)
  } else {
    labels <- sprintf(
      "%s: %s",
      size_var,
      format(data[[size_var]], big.mark = ",")
    ) %>% lapply(htmltools::HTML)
  }
  
  # Add circle markers
  map <- map %>%
    leaflet::addCircleMarkers(
      lng = data[[lon_var]],
      lat = data[[lat_var]],
      radius = sqrt(data[[size_var]]) / max(sqrt(data[[size_var]])) * 30,
      fillColor = colors,
      fillOpacity = 0.7,
      color = "white",
      weight = 2,
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    )
  
  # Add legend if color variable is used
  if (!is.null(color_var) && color_var != "") {
    map <- map %>%
      leaflet::addLegend(
        pal = pal,
        values = data[[color_var]],
        title = color_var,
        position = "bottomright"
      )
  }
  
  map
}

#' Create Coordinate Plot Map
#'
#' Simple coordinate plotting on interactive map
#'
#' @param data Data frame with coordinates
#' @param lat_var Column name for latitude
#' @param lon_var Column name for longitude
#' @param popup_vars Optional vector of column names to show in popup
#' @export
create_coordinate_map <- function(data, lat_var, lon_var, popup_vars = NULL) {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for geographic visualizations. Please install it.")
  }
  
  # Create popup content
  if (!is.null(popup_vars)) {
    popup_content <- apply(data[, popup_vars, drop = FALSE], 1, function(row) {
      paste(names(row), row, sep = ": ", collapse = "<br/>")
    })
  } else {
    popup_content <- paste("Lat:", data[[lat_var]], "<br/>Lon:", data[[lon_var]])
  }
  
  # Create map
  map <- leaflet::leaflet(data) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addMarkers(
      lng = data[[lon_var]],
      lat = data[[lat_var]],
      popup = popup_content,
      clusterOptions = if (nrow(data) > 100) leaflet::markerClusterOptions() else NULL
    )
  
  map
}

#' Create Heatmap (Density) Map
#'
#' Create a density heatmap of geographic points
#'
#' @param data Data frame with coordinates
#' @param lat_var Column name for latitude
#' @param lon_var Column name for longitude
#' @param intensity_var Optional column for intensity values
#' @export
create_density_map <- function(data, lat_var, lon_var, intensity_var = NULL) {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for geographic visualizations. Please install it.")
  }
  
  if (!requireNamespace("leaflet.extras", quietly = TRUE)) {
    warning("Package 'leaflet.extras' recommended for heatmaps. Falling back to circle markers.")
    
    # Fallback to density-based circle markers
    map <- leaflet::leaflet(data) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
      leaflet::addCircleMarkers(
        lng = data[[lon_var]],
        lat = data[[lat_var]],
        radius = 5,
        fillColor = "#ff6b6b",
        fillOpacity = 0.4,
        color = "transparent"
      )
    return(map)
  }
  
  # Create heatmap
  if (!is.null(intensity_var) && intensity_var != "") {
    intensity <- data[[intensity_var]]
  } else {
    intensity <- rep(1, nrow(data))
  }
  
  map <- leaflet::leaflet(data) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
    leaflet.extras::addHeatmap(
      lng = data[[lon_var]],
      lat = data[[lat_var]],
      intensity = intensity,
      blur = 20,
      max = 1,
      radius = 15
    )
  
  map
}

#' Geocode Address
#'
#' Convert address to coordinates (requires API key for production use)
#' This is a placeholder function - in production, use a geocoding service
#'
#' @param address Address string
#' @export
geocode_address <- function(address) {
  
  # Placeholder function - returns random US coordinates
  warning("This is a placeholder. For production, use a geocoding service like Google Maps API or Nominatim.")
  
  list(
    address = address,
    lat = runif(1, 25, 50),
    lon = runif(1, -125, -65),
    success = FALSE,
    message = "Geocoding service not configured. Using placeholder coordinates."
  )
}

#' Detect Geographic Columns (module disabled)
#'
#' Geographic features were removed from this release. This stub returns an empty detection result.
#' @param data Data frame to analyze
#' @return List with detection placeholders
#' @export
detect_geographic_columns <- function(data) {
  list(
    has_coordinates = FALSE,
    latitude = NULL,
    longitude = NULL,
    location_columns = character(0),
    suggested_map_type = NULL
  )
}
