# R-script data_nationalAccounts.R

# References --------------------------------------------------------------

# https://sidra.ibge.gov.br/tabela/21

# Setup -------------------------------------------------------------------

# rm(list = ls())
gc()

data_nationalAccounts_IBGE <- function(grouped_by="micro"){
  
  # Getting BR location info
  df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()
  
  # Table 21 - DEPRECATED for now.
  
  df_nationalAccounts <- readr::read_tsv("volume/data/clean_data/munic/tabela5938_nationalAccounts_micro.tsv") %>%
    stats::setNames(c("cd_micro", "nm_micro", "year", "gdp", "taxes", "addedValue", "addedValue_agriculture", "addedValue_industry",	"addedValue_services",	"addedValue_government")) %>% 
    # dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "X", "0"))) %>%
    # dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "-", "0"))) %>%
    dplyr::select(-nm_micro) %>% 
    dplyr::mutate(cd_micro=as.character(cd_micro)) %>% 
    dplyr::mutate(year=as.character(year)) %>% 
    dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  df_na <- left_join(df_locations, df_nationalAccounts) %>% 
    dplyr::arrange(desc(year), desc(gdp))
  rio::export(df_na, "volume/data/curated_data/micro/df_nationalAccounts_micro.csv")  
  
}

# data_nationalAccounts_IBGE()

