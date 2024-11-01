# R-script data_emig.R


# References --------------------------------------------------------------


# data_imig function ---------------------------------------------------
data_emig <- function(grouped_by="micro"){
  
  # Getting BR location info
  source("BRdb/modules/compute/scripts/util_loadPackages.R")
  source("BRdb/modules/compute/scripts/data_locations.R")
  df_locations <- data_loc()
  
  if(grouped_by == "mun"){
    df_emig3173 <- readr::read_tsv("clean_bucket/munic/tabela3173_emig_mun.tsv") %>%
      stats::setNames(c("cd_mun", "nm_mun", "emig_total", "emig_africa", "emig_southafrica", "emig_angola", "emig_other_african_countries", "emig_central_america", "emig_north_america", "emig_canada", "emig_united_states", "emig_mexico", "emig_south_america", "emig_argentina", "emig_bolivia", "emig_chile", "emig_french_guiana", "emig_paraguay", "emig_suriname", "emig_uruguay", "emig_venezuela", "emig_other_south_american_countries", "emig_asia", "emig_china", "emig_japan", "emig_other_asian_countries", "emig_europe", "emig_germany", "emig_austria", "emig_belgium", "emig_spain", "emig_france", "emig_netherlands", "emig_ireland", "emig_italy", "emig_norway", "emig_portugal", "emig_united_kingdom", "emig_sweden", "emig_switzerland", "emig_other_european_countries", "emig_oceania", "emig_australia", "emig_new_zealand", "emig_other_oceanian_countries", "emig_not_declared")) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(across(3:ncol(.), as.numeric)) %>% 
      dplyr::arrange(desc(emig_total)) %>%
      dplyr::select(-nm_mun) %>% 
      suppressMessages() %>% 
      suppressWarnings()
    df_emig1497 <- readr::read_tsv("clean_bucket/munic/tabela1497_emig_mun.tsv") %>%
      janitor::clean_names(.) %>% 
      dplyr::arrange(desc(total)) %>%
      stats::setNames(c("cd_mun", "nm_mun", "inhabitants", "brazilians", "brazilian_native", "brazilian_naturalized", "foreigners")) %>%
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(across(3:ncol(.), as.numeric)) %>% 
      dplyr::select(-nm_mun) %>% 
      suppressMessages() %>% 
      suppressWarnings()
    df_emig <- df_locations %>% 
      dplyr::left_join(., df_emig1497) %>% 
      dplyr::left_join(., df_emig3173) %>% 
      dplyr::arrange(desc(inhabitants))
    rio::export(df_emig, "curated_bucket/munic/df_emigrants.csv")
  }else
    if(grouped_by == "micro"){
      df_emig3173 <- readr::read_tsv("clean_bucket/micro/tabela3173_emig_micro.tsv") %>%
        stats::setNames(c("cd_micro", "nm_micro", "emig_total", "emig_africa", "emig_southafrica", "emig_angola", "emig_other_african_countries", "emig_central_america", "emig_north_america", "emig_canada", "emig_united_states", "emig_mexico", "emig_south_america", "emig_argentina", "emig_bolivia", "emig_chile", "emig_french_guiana", "emig_paraguay", "emig_suriname", "emig_uruguay", "emig_venezuela", "emig_other_south_american_countries", "emig_asia", "emig_china", "emig_japan", "emig_other_asian_countries", "emig_europe", "emig_germany", "emig_austria", "emig_belgium", "emig_spain", "emig_france", "emig_netherlands", "emig_ireland", "emig_italy", "emig_norway", "emig_portugal", "emig_united_kingdom", "emig_sweden", "emig_switzerland", "emig_other_european_countries", "emig_oceania", "emig_australia", "emig_new_zealand", "emig_other_oceanian_countries", "emig_not_declared")) %>%
        dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
        dplyr::mutate(across(3:ncol(.), as.numeric)) %>% 
        dplyr::arrange(desc(emig_total)) %>%
        dplyr::select(-nm_micro) %>% 
        suppressMessages() %>% 
        suppressWarnings()
      df_emig1497 <- readr::read_tsv("clean_bucket/micro/tabela1497_emig_micro.tsv") %>%
        janitor::clean_names(.) %>% 
        dplyr::arrange(desc(total)) %>%
        stats::setNames(c("cd_micro", "nm_micro", "inhabitants", "brazilians", "brazilian_native", "brazilian_naturalized", "foreigners")) %>%
        dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
        dplyr::mutate(across(3:ncol(.), as.numeric)) %>% 
        dplyr::select(-nm_micro) %>% 
        suppressMessages() %>% 
        suppressWarnings()
      df_emig <- df_locations %>%
        dplyr::select(cd_micro, nm_micro, cd_meso, nm_meso, cd_rgime, nm_rgime, cd_rgint, nm_rgint, cd_uf, sg_uf, nm_uf, cd_reg, sg_reg, nm_reg) %>% 
        dplyr::left_join(., df_emig1497) %>% 
        dplyr::left_join(., df_emig3173) %>% 
        dplyr::arrange(desc(inhabitants))
      rio::export(df_emig, "curated_bucket/micro/df_emigrants.csv")
    }
  
  
  return(df_emig)
  
}

# df_emig = data_emig(grouped_by = "mun")

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






