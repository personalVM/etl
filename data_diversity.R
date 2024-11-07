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
  source("volume/etl/data_locations.R")
  source("volume/etl/data_exports.R")

  # Getting BR location info
  df_locations <- readr::read_csv(paste0("volume/data/curated_bucket/munic/df_locations_", grouped_by, ".csv")) %>%
    suppressMessages()

  # Loading exports data
  df_exp <- readr::read_csv(paste0("volume/data/curated_bucket/munic/df_exports_", grouped_by, ".csv")) %>%
    suppressMessages()

  df_expl <- left_join(br_loc, df_exp) %>%
    mutate(exp = ifelse(is.na(exp), 1, exp))

  if (grouped_by == "mun") {
    df_exp1 <- df_expl %>%
      group_by(cd_mun, cd_sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_mun) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(cd_sh4) %>%
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
      group_by(cd_micro, cd_sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_micro) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(cd_sh4) %>%
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
      group_by(cd_meso, cd_sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_meso) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(cd_sh4) %>%
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
      group_by(cd_rgime, cd_sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_rgime) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(cd_sh4) %>%
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
      group_by(cd_rgint, cd_sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_rgint) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(cd_sh4) %>%
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
      group_by(cd_uf, cd_sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_uf) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(cd_sh4) %>%
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
      group_by(cd_rg, cd_sh4) %>%
      mutate(x_rp = sum(exp, na.rm = T)) %>%
      group_by(cd_rg) %>%
      mutate(x_r = sum(exp, na.rm = T)) %>%
      group_by(cd_sh4) %>%
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
