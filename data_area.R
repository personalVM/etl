# R-script data_area.R

# References --------------------------------------------------------------

# Shaoefiles IBGE
# https://www.ibge.gov.br/geociencias/downloads-geociencias.html

# Function ----------------------------------------------------------------
data_area <- function(grouped_by = "micro") {
  df_area <- sf::st_read(paste0("volume/data/clean_data/", grouped_by, "/shp")) %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(
      cd_micro = as.character(cd_micro)
    ) %>%
    dplyr::select(cd_micro)
  df_area$area <- as.numeric(sf::st_area(df_area)) # Take care of units
  df_area$area <- df_area$area / 1000
  df_area <- df_area %>% sf::st_drop_geometry()

  return(df_area)
}

# data_area()




