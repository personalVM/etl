# R-script data_diversity.R

# References --------------------------------------------------------------

# https://oec.world/en/resources/methods#eci-subnational
# https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

# Function to get diversity values ----------------------------------------
data_diversity <- function(input_filter = 0, grouped_by = "micro") {
  # Setup
  # rm(list = ls())
  gc()
  options(scipen = 666, stringsAsFactors = F)
  source("volume/etl/util_loadPackages.R")

  # Getting BR location info
  df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()

  # Loading exports data
  df_exp <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_exports_", grouped_by, ".csv")) %>%
    suppressMessages() %>%
    {
      if ("cd_munic" %in% names(.)) {
        mutate(., cd_munic = as.character(cd_munic), year = as.character(year))
      } else if ("cd_micro" %in% names(.)) {
        mutate(., cd_micro = as.character(cd_micro), year = as.character(year))
      } else {
        .
      }
    } %>%
    dplyr::select("cd_micro", "year", contains("_sh4_")) %>%
    tidyr::pivot_longer(
      cols = contains("_sh4_"),
      names_to = "sh4",
      values_to = "exp"
    ) %>%
    dplyr::filter(year == 2010) # The algorythm must be further developed to get also the other years.


  df_expl <- left_join(df_locations, df_exp) %>%
    mutate(exp = ifelse(is.na(exp), 0, exp))

  if (grouped_by == "mun") {
    df_exp1 <- df_expl %>%
      group_by(cd_mun, sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_mun) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(sh4) %>%
      mutate(x_p = sum(exp, na.rm = T)) %>%
      group_by() %>%
      mutate(x = sum(exp, na.rm = T)) %>%
      ungroup() %>%
      # mutate(prop_x=x_rp/x_p) %>%
      mutate(prop_x = x_rp / x_r) %>%
      mutate(rca = ifelse(((x_rp / x_p) / (x_r / x)) >= 1, 1, 0)) %>%
      mutate(rca = ifelse(prop_x < input_filter, 0, rca)) %>%
      group_by(cd_mun) %>%
      summarise(diversity = sum(rca, na.rm = T)) %>%
      ungroup() %>%
      arrange(desc(diversity))
  } else if (grouped_by == "micro") {
    df_exp1 <- df_expl %>%
      group_by(cd_micro, sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_micro) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(sh4) %>%
      mutate(x_p = sum(exp, na.rm = T)) %>%
      group_by() %>%
      mutate(x = sum(exp, na.rm = T)) %>%
      ungroup() %>%
      # mutate(prop_x=x_rp/x_p) %>%
      mutate(prop_x = x_rp / x_r) %>%
      mutate(rca = ifelse(((x_rp / x_p) / (x_r / x)) >= 1, 1, 0)) %>%
      mutate(rca = ifelse(prop_x < input_filter, 0, rca)) %>%
      group_by(cd_micro) %>%
      summarise(diversity = sum(rca, na.rm = T)) %>%
      ungroup() %>%
      arrange(desc(diversity))
  } else if (grouped_by == "meso") {
    df_exp1 <- df_expl %>%
      group_by(cd_meso, sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_meso) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(sh4) %>%
      mutate(x_p = sum(exp, na.rm = T)) %>%
      group_by() %>%
      mutate(x = sum(exp, na.rm = T)) %>%
      ungroup() %>%
      # mutate(prop_x=x_rp/x_p) %>%
      mutate(prop_x = x_rp / x_r) %>%
      mutate(rca = ifelse(((x_rp / x_p) / (x_r / x)) >= 1, 1, 0)) %>%
      mutate(rca = ifelse(prop_x < input_filter, 0, rca)) %>%
      group_by(cd_meso) %>%
      summarise(diversity = sum(rca, na.rm = T)) %>%
      ungroup() %>%
      arrange(desc(diversity))
  } else if (grouped_by == "rgime") {
    df_exp1 <- df_expl %>%
      group_by(cd_rgime, sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_rgime) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(sh4) %>%
      mutate(x_p = sum(exp, na.rm = T)) %>%
      group_by() %>%
      mutate(x = sum(exp, na.rm = T)) %>%
      ungroup() %>%
      # mutate(prop_x=x_rp/x_p) %>%
      mutate(prop_x = x_rp / x_r) %>%
      mutate(rca = ifelse(((x_rp / x_p) / (x_r / x)) >= 1, 1, 0)) %>%
      mutate(rca = ifelse(prop_x < input_filter, 0, rca)) %>%
      group_by(cd_rgime) %>%
      summarise(diversity = sum(rca, na.rm = T)) %>%
      ungroup() %>%
      arrange(desc(diversity))
  } else if (grouped_by == "rgint") {
    df_exp1 <- df_expl %>%
      group_by(cd_rgint, sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_rgint) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(sh4) %>%
      mutate(x_p = sum(exp, na.rm = T)) %>%
      group_by() %>%
      mutate(x = sum(exp, na.rm = T)) %>%
      ungroup() %>%
      # mutate(prop_x=x_rp/x_p) %>%
      mutate(prop_x = x_rp / x_r) %>%
      mutate(rca = ifelse(((x_rp / x_p) / (x_r / x)) >= 1, 1, 0)) %>%
      mutate(rca = ifelse(prop_x < input_filter, 0, rca)) %>%
      group_by(cd_rgint) %>%
      summarise(diversity = sum(rca, na.rm = T)) %>%
      ungroup() %>%
      arrange(desc(diversity))
  } else if (grouped_by == "uf") {
    df_exp1 <- df_expl %>%
      group_by(cd_uf, sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_uf) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(sh4) %>%
      mutate(x_p = sum(exp, na.rm = T)) %>%
      group_by() %>%
      mutate(x = sum(exp, na.rm = T)) %>%
      ungroup() %>%
      # mutate(prop_x=x_rp/x_p) %>%
      mutate(prop_x = x_rp / x_r) %>%
      mutate(rca = ifelse(((x_rp / x_p) / (x_r / x)) >= 1, 1, 0)) %>%
      mutate(rca = ifelse(prop_x < input_filter, 0, rca)) %>%
      group_by(cd_uf) %>%
      summarise(diversity = sum(rca, na.rm = T)) %>%
      ungroup() %>%
      arrange(desc(diversity))
  } else if (grouped_by == "rg") {
    df_exp1 <- df_expl %>%
      group_by(cd_rg, sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_rg) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(sh4) %>%
      mutate(x_p = sum(exp, na.rm = T)) %>%
      group_by() %>%
      mutate(x = sum(exp, na.rm = T)) %>%
      ungroup() %>%
      # mutate(prop_x=x_rp/x_p) %>%
      mutate(prop_x = x_rp / x_r) %>%
      mutate(rca = ifelse(((x_rp / x_p) / (x_r / x)) >= 1, 1, 0)) %>%
      mutate(rca = ifelse(prop_x < input_filter, 0, rca)) %>%
      group_by(cd_rg) %>%
      summarise(diversity = sum(rca, na.rm = T)) %>%
      ungroup() %>%
      arrange(desc(diversity))
  }

  return(df_exp1)
}

data_diversidade(grouped_by = "micro")
