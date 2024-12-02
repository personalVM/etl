# R-script data_internalMigration.R


# References --------------------------------------------------------------


# Function ----------------------------------------------------------------
data_internalMigration <- function(grouped_by = "micro") {

  source("volume/etl/util_loadPackages.R")
  source("volume/etl/data_locations.R")
  
  # df_locations <- readr::read_csv(paste0("volume/data/curated_data/munic/df_locations_munic.csv")) %>%
  #   # df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
  #   dplyr::mutate(dplyr::across(everything(), as.character)) %>%
  #   suppressMessages()
  
  if (grouped_by == "munic") {
    df_internalMigration_munic <- readr::read_tsv("volume/data/clean_data/munic/tabela1505_internalmig_munic.tsv") %>% 
      janitor::clean_names(.) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
      dplyr::select(cd_munic = cod, everything()) %>%
      dplyr::select(-municipio, -total) %>% 
      dplyr::arrange(desc(nao_naturais_do_municipio)) %>%
      suppressMessages() %>%
      suppressWarnings()
    rio::export(df_internalMigration_munic, "volume/data/curated_data/munic/df_internalMigration_munic.csv")
  } else if (grouped_by == "micro") {
    df_internalMigration_micro <- readr::read_tsv("volume/data/clean_data/micro/tabela1505_internalmig_micro.tsv") %>% 
      janitor::clean_names(.) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
      dplyr::select(cd_micro = cod, everything()) %>%
      dplyr::select(-microrregiao_geografica, -total) %>% 
      dplyr::arrange(desc(nao_naturais_do_municipio)) %>%
      suppressMessages() %>%
      suppressWarnings()
    rio::export(df_internalMigration_micro, "volume/data/curated_data/micro/df_internalMigration_micro.csv")
  }
}

# data_internalMigration()
# df_imig = data_imig(grouped_by = "mun")

# ggplot(df_imig[1:20, ], aes(x = reorder(nm_micro, imig), y = imig)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(title = "Total Imigration by Micro-Region", x = "Micro-Region", y = "Total Imigration") +
#   theme_minimal()
