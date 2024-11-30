# R-script data_distCoast.R

# References --------------------------------------------------------------

# Shaoefiles IBGE

# Setup -------------------------------------------------------------------

# rm(list = ls())

# Function ----------------------------------------------------------------
data_distCoast <- function(grouped_by="micro"){
  
  source("volume/etl/fct_centAsCols.R")
  
  # Get BR territory border
  basemap_lines <- rnaturalearth::ne_countries(scale = 110, country = "brazil", returnclass = "sf")[1] %>% 
    st_cast("MULTILINESTRING")
  # plot(basemap_lines)
  
  # Get only the coast
  xmax <- -30
  xmin <- -53
  ymax <-  5
  ymin <- -35
  bbox <- st_polygon(
    list(rbind(
      c(xmin,ymin), 
      c(xmin,ymax), 
      c(xmax,ymax), 
      c(xmax,ymin),
      c(xmin,ymin)
    ))) %>% st_sfc(.)
  st_crs(bbox) <- 4326
  
  crop_lines <- st_intersection(basemap_lines, bbox) %>% 
    suppressWarnings()
  # plot(crop_lines)
  
  # df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
  #   dplyr::mutate(dplyr::across(everything(), as.character)) %>%
  #   suppressMessages()
  
  shp <- sf::st_read("volume/data/clean_data/micro/shp/") %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(
      cd_micro = as.character(cd_micro)
    ) %>% 
    fct_centAsCols(.)
  
  df_shp <- sf::st_read(paste0("volume/data/clean_data/", grouped_by, "/shp")) %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(
      cd_micro = as.character(cd_micro)
    ) %>%
    dplyr::select(cd_micro)
  
  distances_coast <- st_distance(x = df_shp, y = crop_lines)
  
  df_shp$dist_coast <- as.numeric(distances_coast)
  
  df_dcoast <- df_shp %>% 
    st_drop_geometry(.) %>%
    as.data.frame(.) %>%
    select(cd_micro, dist_coast) %>%
    mutate(dist_coast = (as.integer(dist_coast)/1000)/1000) # Thousand Kilometers units
  
  rio::export(df_dcoast, "volume/data/curated_data/micro/df_dcoast_micro.csv")
  
}

# data_distCoast()
