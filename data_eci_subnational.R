# R-script draft_ECI.R


# References --------------------------------------------------------------

# https://oec.world/en/resources/methods#eci-subnational
# https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

# Function ----------------------------------------------------------------
data_eci_subn <- function(grouped_by="micro", input_filter=0.002, tratment = T){
  
  # Setup
  # rm(list = ls())
  gc()
  # options(scipen = 666, stringsAsFactors = F)
  # source("volume/util_loadPackages.R")
  # source("src/data_loc.R")
  # source("src/data_exports.R")
  # source("src/data_pci_oec.R")
  
  # BR locations
  df_locations_munic <- readr::read_csv(paste0("volume/data/curated_data/munic/df_locations_munic.csv")) %>%
  # df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()
  
  # Load PCI values
  readr::read_csv("volume/data/clean_data/pci_oec.csv") %>% filter(year==2010) %>% tail()
  df_pci <- readr::read_csv("volume/data/clean_data/pci_2010_oec_curated.csv") %>% 
    mutate(cd_sh4=as.character(cd_sh4), pci=as.numeric(pci))
  
  # Load exports data by sh4:
  # TODO: get the original data.
  exp <- vroom::vroom(file = "volume/data/clean_data/munic/EXP_COMPLETA_MUN.csv") %>%
    janitor::clean_names() %>%
    dplyr::select(cd_munic = co_mun, year = co_ano, sg_state = sg_uf_mun, cd_sh4 = sh4, exports = vl_fob) %>%
    dplyr::mutate(
      exports = dplyr::if_else(is.na(exports), 0, exports),
      cd_munic = dplyr::case_when(
        sg_state == "SP" ~ cd_munic + 100000, # SP
        sg_state == "GO" ~ cd_munic - 100000, # GO
        sg_state == "MS" ~ cd_munic - 200000, # MS
        sg_state == "DF" ~ cd_munic - 100000, # DF
        TRUE ~ cd_munic # Keep unchanged if no match
      )
    ) %>%
    dplyr::mutate(
      cd_munic = as.character(cd_munic),
      year = as.character(year),
      cd_sh4 = as.character(cd_sh4)
    ) %>%
    dplyr::group_by(cd_munic, year, cd_sh4) %>%
    dplyr::summarise(exports = sum(exports, na.rm = T), .groups = "drop") %>%
    dplyr::filter(year=="2010") %>%
    select(-year) %>%
    as.data.frame(.) 
  gc()
  exp2 <- exp %>% 
    left_join(., df_locations_munic) %>% 
    dplyr::group_by(cd_micro, cd_sh4) %>%
    summarise(exp=sum(exports))
    dplyr::summarise(exports = sum(exports, na.rm = T), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = cd_sh4,
      values_from = exports,
      names_prefix = "",
      values_fill = list(exports = 0)
    )
    
  exp3 <- exp2 %>%
    tidyr::pivot_longer(
      # cols      = -c(cd_micro, year),
      cols      = cd_micro,
      names_to  = "cd_sh4",
      values_to = "exp"
    )

  # ECI_subn calculation for 2010
  # df_eci_subn <- 
    exp2 %>%
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
    dplyr::left_join(., df_pci) 
    %>% 
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

