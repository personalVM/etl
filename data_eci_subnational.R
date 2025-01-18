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
  # readr::read_csv("volume/data/clean_data/pci_oec.csv") %>% filter(year==2010) %>% tail()
  # df_pci <- readr::read_csv("volume/data/clean_data/pci_hs4_hs96.csv") %>% 
  #   mutate(cd_sh4=as.character(hs4_id), pci=as.numeric(pci)) %>% 
  #   group_by(cd_sh4) %>% 
  #   summarise(pci=mean(pci)) %>% 
  #   ungroup()
  df_pci <- readr::read_delim(
    file = "volume/data/clean_data/pci_hs4_hs96.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(
      cd_sh4=as.character(hs4_id),
      nm_sh4=as.character(hs4),
    ) %>% 
    dplyr::select(cd_sh4, nm_sh4, x1998, x1999, starts_with("x20")) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  df_pci$pci <- rowMeans(df_pci[,3:ncol(df_pci)], na.rm = T)
  df_pci <- df_pci %>% select(cd_sh4, pci)
  
  
  
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
    # summarise(exp=sum(exports)) %>% 
    # ungroup() %>% 
    dplyr::summarise(exp = sum(exports, na.rm = T), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = cd_sh4,
      values_from = exp,
      names_prefix = "",
      values_fill = list(exp = 0)
    )
    
  exp3 <- exp2 %>%
    tidyr::pivot_longer(
      cols      = 2:length(exp2),
      names_to  = "cd_sh4",
      values_to = "exp"
    )
  
  # ECI_subn calculation for 2010
  df_eci_subn <- exp3 %>%
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
    df_eci_subn <- left_join(loc, df_eci_subn)
    library(randomForest)
    set.seed(123)
    
    train_data <- df_eci_subn[-which(is.na(df_eci_subn$eci_subn)), ]
    test_data <- df_eci_subn[which(is.na(df_eci_subn$eci_subn)), ]
    rf_model <- randomForest(eci_subn ~ ., data = train_data, ntree = 500) 
    predictions <- predict(rf_model, newdata = test_data)
    library(caret)
    # confusionMatrix(predictions, test_data$eci_subn) 
    # plot(rf_model)
    
    test_data$eci_subn = as.vector(predictions)
    
    data = rbind(train_data, test_data)[, c("cd_micro", "eci_subn")]
    data
  }
  
  data %>%
    rio::export(., "volume/data/curated_data/micro/df_eci_micro.csv")
  
  return(df_eci_subn)
  
}

# eci_subn <- data_eci_subn()

