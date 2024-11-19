library(sf)

fct_centAsCols <- function(data, geometry_col = "geometry", crs = NULL) {
  # Ensure the data is an sf object
  if (!inherits(data, "sf")) {
    stop("The input data must be an sf object.")
  }
  
  # Transform CRS if specified
  if (!is.null(crs)) {
    data <- st_transform(data, crs)
  }
  
  # Calculate centroids
  centroids <- st_centroid(data[[geometry_col]])
  
  # Extract coordinates into columns
  coords <- st_coordinates(centroids)
  data$centroid_x <- coords[, 1]
  data$centroid_y <- coords[, 2]
  
  return(data)
}
