# R-script data_exports.R

# Treating and grouping exports data 

# References --------------------------------------------------------------

# https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta
# Seção 2: Base de dados detalhada por Município da empresa exportadora/importadora e Posição do Sistema Harmonizado (SH4)

# data_exports function ---------------------------------------------------
data_exports <- function(grouped_by="micro"){
  
  # Getting BR location info
  source("volume/etl/util_loadPackages.R")
  source("volume/etl/data_locations.R")
  df_locations <- data_loc()
  
  # Loading exp data
  exp <- vroom::vroom(file = "data/clean_data/munic/EXP_COMPLETA_MUN.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::select(cd_mun=co_mun, year=co_ano, sg_uf=sg_uf_mun, cd_sh4=sh4, exports=vl_fob) %>% 
    dplyr::mutate(
      exports = dplyr::if_else(is.na(exports), 0, exports),
      cd_mun = dplyr::case_when(
        sg_uf == "SP" ~ cd_mun + 100000, # SP
        sg_uf == "GO" ~ cd_mun - 100000, # GO
        sg_uf == "MS" ~ cd_mun - 200000, # MS
        sg_uf == "DF" ~ cd_mun - 100000, # DF
        TRUE ~ cd_mun # Keep unchanged if no match
      )
    ) %>% 
    dplyr::mutate(
      cd_mun=as.character(cd_mun),
      year=as.character(year),
      cd_sh4=as.character(cd_sh4)
    ) %>% 
    dplyr::group_by(cd_mun, year, cd_sh4) %>%
    dplyr::summarise(exports = sum(exports, na.rm = T), .groups = "drop") %>% 
    as.data.frame() %>% 
    suppressMessages(); gc()
  
  exp_sh4 <- exp %>% 
    dplyr::group_by(cd_mun, year, cd_sh4) %>% 
    dplyr::summarise(exports = sum(exports, na.rm = T), .groups = "drop") %>% 
    tidyr::pivot_wider(
      names_from = cd_sh4,
      values_from = exports,
      names_prefix = "exports_sh4_",
      values_fill = list(exports = 0))
  exp_sh2 <- exp %>% 
    dplyr::mutate(cd_sh2=substr(cd_sh4, 1, 2)) %>% 
    dplyr::group_by(cd_mun, year, cd_sh2) %>% 
    dplyr::summarise(exports = sum(exports, na.rm = T), .groups = "drop") %>% 
    tidyr::pivot_wider(
      names_from = cd_sh2,
      values_from = exports,
      names_prefix = "exports_sh2_",
      values_fill = list(exports = 0))
  exp_total <- exp %>% 
    dplyr::group_by(cd_mun, year) %>% 
    dplyr::summarise(exports = sum(exports, na.rm = T), .groups = "drop")
  rm(exp); gc()
  exp_mun_plus <- df_locations %>% 
    dplyr::left_join(., exp_total) %>% 
    dplyr::left_join(., exp_sh2) %>% 
    dplyr::left_join(., exp_sh4) %>% 
    dplyr::arrange(desc(exports))

  if(grouped_by == "mun"){
    exp_mun_plus %>% 
      dplyr::select(-cd_micro, -nm_micro, -cd_meso, -nm_meso, -cd_uf, -sg_uf, -nm_uf, -cd_reg, -sg_reg, -nm_reg, -cd_rgime, -nm_rgime, -cd_rgint, -nm_rgint) %>% 
      rio::export(., "data/curated_data/munic/df_exports_mun.csv")
  }else
    if(grouped_by == "micro"){
      exp_mun_plus %>% 
        dplyr::group_by(cd_micro, year) %>% 
        dplyr::summarise(
          across(
            .cols = starts_with("exports"),
            .fns = sum
          ), .groups = "drop"
        ) %>% 
        rio::export(., "data/curated_data/micro/df_exports_micro.csv")
    }else
      if(grouped_by == "meso"){
        exp_mun_plus %>% 
          dplyr::group_by(cd_meso, year) %>% 
          dplyr::summarise(
            across(
              .cols = starts_with("exports"),
              .fns = sum
            ), .groups = "drop"
          ) %>% 
          rio::export(., "data/curated_data/meso/df_exports_meso.csv")
      }else
        if(grouped_by == "rgime"){
          exp_mun_plus %>% 
            dplyr::group_by(cd_rgime, year) %>% 
            dplyr::summarise(
              across(
                .cols = starts_with("exports"),
                .fns = sum
              ), .groups = "drop"
            ) %>% 
            rio::export(., "data/curated_data/rgime/df_exports_rgime.csv")
        }else
          if(grouped_by == "rgint"){
            exp_mun_plus %>% 
              dplyr::group_by(cd_rgint, year) %>% 
              dplyr::summarise(
                across(
                  .cols = starts_with("exports"),
                  .fns = sum
                ), .groups = "drop"
              ) %>% 
              rio::export(., "data/curated_data/rgint/df_exports_rgint.csv")
          }else
            if(grouped_by == "uf"){
              exp_mun_plus %>% 
                dplyr::group_by(sg_uf, year) %>% 
                dplyr::summarise(
                  across(
                    .cols = starts_with("exports"),
                    .fns = sum
                  ), .groups = "drop"
                ) %>% 
                rio::export(., "data/curated_data/uf/df_exports_uf.csv")
            }else
              if(grouped_by == "rg"){
                exp_mun_plus %>% 
                  dplyr::group_by(sg_reg, year) %>% 
                  dplyr::summarise(
                    across(
                      .cols = starts_with("exports"),
                      .fns = sum
                    ), .groups = "drop"
                  ) %>% 
                  rio::export(., "data/curated_data/regions/df_exports_regions.csv")
              }

  
  
  return(exp)
  
}

# data_exports(grouped_by = "mun")
