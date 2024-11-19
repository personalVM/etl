# R-script data_emig.R


# References --------------------------------------------------------------


# data_imig function ---------------------------------------------------
data_emig <- function(grouped_by = "micro") {
  # Getting BR location info
  source("volume/etl/util_loadPackages.R")
  source("volume/etl/data_locations.R")

  df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()

  if (grouped_by == "munic") {
    df_emig3173 <- readr::read_tsv("volume/data/clean_data/munic/tabela3173_emig_munic.tsv") %>%
      stats::setNames(c("cd_munic", "nm_munic", "emig_total", "emig_africa", "emig_southafrica", "emig_angola", "emig_other_african_countries", "emig_central_america", "emig_north_america", "emig_canada", "emig_united_states", "emig_mexico", "emig_south_america", "emig_argentina", "emig_bolivia", "emig_chile", "emig_french_guiana", "emig_paraguay", "emig_suriname", "emig_uruguay", "emig_venezuela", "emig_other_south_american_countries", "emig_asia", "emig_china", "emig_japan", "emig_other_asian_countries", "emig_europe", "emig_germany", "emig_austria", "emig_belgium", "emig_spain", "emig_france", "emig_netherlands", "emig_ireland", "emig_italy", "emig_norway", "emig_portugal", "emig_united_kingdom", "emig_sweden", "emig_switzerland", "emig_other_european_countries", "emig_oceania", "emig_australia", "emig_new_zealand", "emig_other_oceanian_countries", "emig_not_declared")) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
      dplyr::arrange(desc(emig_total)) %>%
      dplyr::select(-nm_munic) %>%
      suppressMessages() %>%
      suppressWarnings()
    df_emig1497 <- readr::read_tsv("volume/data/clean_data/munic/tabela1497_emig_munic.tsv") %>%
      janitor::clean_names(.) %>%
      dplyr::arrange(desc(total)) %>%
      stats::setNames(c("cd_munic", "nm_munic", "inhabitants", "brazilians", "brazilian_native", "brazilian_naturalized", "foreigners")) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
      dplyr::select(-nm_munic) %>%
      suppressMessages() %>%
      suppressWarnings()
    df_emig <- df_locations %>%
      dplyr::left_join(., df_emig1497) %>%
      dplyr::left_join(., df_emig3173) %>%
      dplyr::arrange(desc(inhabitants)) %>%
      dplyr::select(-nm_munic, -cd_micro, -nm_micro, -cd_meso, -nm_meso, -cd_rgime, -nm_rgime, -cd_rgint, -nm_rgint, -cd_state, -sg_state, -nm_state, -cd_region, -sg_region, -nm_region)
    rio::export(df_emig, "volume/data/curated_bucket/munic/df_emigrants_munic.csv")
  } else if (grouped_by == "micro") {
    df_emig3173 <- readr::read_tsv("volume/data/clean_data/micro/tabela3173_emig_micro.tsv") %>%
      stats::setNames(c("cd_micro", "nm_micro", "emig_total", "emig_africa", "emig_southafrica", "emig_angola", "emig_other_african_countries", "emig_central_america", "emig_north_america", "emig_canada", "emig_united_states", "emig_mexico", "emig_south_america", "emig_argentina", "emig_bolivia", "emig_chile", "emig_french_guiana", "emig_paraguay", "emig_suriname", "emig_uruguay", "emig_venezuela", "emig_other_south_american_countries", "emig_asia", "emig_china", "emig_japan", "emig_other_asian_countries", "emig_europe", "emig_germany", "emig_austria", "emig_belgium", "emig_spain", "emig_france", "emig_netherlands", "emig_ireland", "emig_italy", "emig_norway", "emig_portugal", "emig_united_kingdom", "emig_sweden", "emig_switzerland", "emig_other_european_countries", "emig_oceania", "emig_australia", "emig_new_zealand", "emig_other_oceanian_countries", "emig_not_declared")) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
      dplyr::arrange(desc(emig_total)) %>%
      dplyr::select(-nm_micro) %>%
      suppressMessages() %>%
      suppressWarnings()
    df_emig1497 <- readr::read_tsv("volume/data/clean_data/micro/tabela1497_emig_micro.tsv") %>%
      janitor::clean_names(.) %>%
      dplyr::arrange(desc(total)) %>%
      stats::setNames(c("cd_micro", "nm_micro", "inhabitants", "brazilians", "brazilian_native", "brazilian_naturalized", "foreigners")) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
      dplyr::select(-nm_micro) %>%
      suppressMessages() %>%
      suppressWarnings()
    df_emig <- df_locations %>%
      dplyr::select(cd_micro, nm_micro, cd_meso, nm_meso, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>%
      dplyr::left_join(., df_emig1497) %>%
      dplyr::left_join(., df_emig3173) %>%
      dplyr::arrange(desc(inhabitants)) %>%
      dplyr::select(-nm_micro, -cd_meso, -nm_meso, -cd_state, -sg_state, -nm_state, -cd_region, -sg_region, -nm_region)
    rio::export(df_emig, "volume/data/curated_data/micro/df_emigrants_micro.csv")
  }
}


# "curated_bucket/micro/df_emigrants.csv"
# df_emig = data_emig(grouped_by = "munic")

# ggplot(df_imig[1:20, ], aes(x = reorder(nm_micro, imig), y = imig)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(title = "Total Imigration by Micro-Region", x = "Micro-Region", y = "Total Imigration") +
#   theme_minimal()








# ggplot(df_emig[1:10, ], aes(x = reorder(nm_micro, emig_total), y = emig_total)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(title = "Total Emigration by Micro-Region", x = "Micro-Region", y = "Total Emigration") +
#   theme_minimal()
