# R-script draft_ECI.R


# References --------------------------------------------------------------

# https://oec.world/en/resources/methods#eci-subnational
# https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

# Function ----------------------------------------------------------------
data_eci_subn <- function(input_filter=0.002, tratment = T){
  
  # Setup
  # rm(list = ls())
  gc()
  # options(scipen = 666, stringsAsFactors = F)
  # source("volume/util_loadPackages.R")
  # source("src/data_loc.R")
  # source("src/data_exports.R")
  # source("src/data_pci_oec.R")
  
  # BR locations
  df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()
  
  # Load exports data by sh4
  df_exp <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_exports_", grouped_by, ".csv")) %>%
    # dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    dplyr::filter(year==2010) %>% 
    select(cd_micro, contains("_sh4_"))
    suppressMessages()
  
  # Load PCI values
  df_pci <- readr::read_csv("volume/data/clean_data/pci_2010_oec_curated.csv")
  
  # ECI_subn calculation for 2010
  df_eci_subn <- df_exp %>%
    dplyr::group_by(cd_micro, cd_sh4) %>% 
    dplyr::mutate(x_rp=sum(exp, na.rm = T)) %>% 
    dplyr::group_by(cd_micro) %>% 
    dplyr::mutate(x_r=sum(exp, na.rm = T)) %>% 
    dplyr::group_by(cd_sh4) %>% 
    dplyr::mutate(x_p=sum(exp, na.rm = T)) %>% 
    dplyr::group_by() %>% 
    dplyr::mutate(x=sum(exp, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    # dplyr::mutate(prop_x=x_rp/x_p) %>%
    dplyr::mutate(prop_x=x_rp/x_r) %>%
    dplyr::mutate(rca=ifelse( ((x_rp/x_p)/(x_r/x)) >= 1, 1, 0)) %>% 
    dplyr::mutate(rca=ifelse(prop_x < input_filter, 0, rca)) %>%
    dplyr::left_join(., df_pci) %>% 
    dplyr::mutate(rca_pci=rca*pci) %>% 
    dplyr::group_by(cd_micro) %>% 
    dplyr::summarise(eci_subn=mean(rca_pci, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(dplyr::desc(eci_subn)) %>% 
    suppressMessages()
  
  if(tratment == T){
    df_eci_subn <- left_join(df_eci_subn, br_loc)
    df_eci_subn[which(df_eci_subn$cd_micro==22009), "eci_subn"] = df_eci_subn[which((df_eci_subn$sg_uf=="PI")&(df_eci_subn$cd_micro!=22009)), "eci_subn"] %>% pull() %>% mean(., na.rm = T)
    df_eci_subn[which(df_eci_subn$cd_micro==24002), "eci_subn"] = df_eci_subn[which((df_eci_subn$sg_uf=="RN")&(df_eci_subn$cd_micro!=24002)), "eci_subn"] %>% pull() %>% mean(., na.rm = T)
  }
  
  return(df_eci_subn)
  
}

# eci_subn <- data_eci_subn()

