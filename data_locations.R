# R-script data_loc.R


# References --------------------------------------------------------------

# API de serviços IBGE
# https://servicodados.ibge.gov.br/
# https://servicodados.ibge.gov.br/api/v1/localidades/estados/


# Function ----------------------------------------------------------------
# Função para obter dados das localizações e divisões regionais do Brasil através da API de serviços IBGE
data_locations <- function(
    grouped_by="munic",
    use_backup=T
  ){
  
  # Setup
  # rm(list = ls())
  gc()
  options(scipen = 666, stringsAsFactors = F)
  source("volume/etl/util_loadPackages.R")
  
  if(use_backup==T){
    
    # Read back df_locatons_mun.csv data
    res <- readr::read_csv("volume/data/clean_data/munic/df_locations_munic.csv") %>% 
      dplyr::mutate(dplyr::across(everything(), as.character)) %>% 
      suppressMessages()
    
  } else if(use_backup==F){
    
    # Call servicosdados API at IBGE servers
    ufs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
    req <- httr::GET(paste0('https://servicodados.ibge.gov.br/api/v1/localidades/estados/', paste0(ufs, collapse = '|'), '/municipios'))
    if(!file.exists("data/data_locatons_br_ibge.csv") & req$status_code == 200){
      res <- httr::content(x = req, type = 'text', encoding = 'UTF-8') %>% 
        jsonlite::fromJSON(txt = ., flatten = TRUE) %>% 
        as.data.frame(x = .) %>% 
        janitor::clean_names(dat = .) %>% 
        dplyr::rename(
          'cd_munic'  = 'id',
          'nm_munic'  = 'nome',
          'cd_micro'  = 'microrregiao_id',
          'nm_micro'  = 'microrregiao_nome',
          'cd_meso'   = 'microrregiao_mesorregiao_id',
          'nm_meso'   = 'microrregiao_mesorregiao_nome',
          'cd_rgime'  = 'regiao_imediata_id',
          'nm_rgime'  = 'regiao_imediata_nome',
          'cd_rgint'  = 'regiao_imediata_regiao_intermediaria_id',
          'nm_rgint'  = 'regiao_imediata_regiao_intermediaria_nome',
          'cd_state'  = 'regiao_imediata_regiao_intermediaria_uf_id',
          'sg_state'  = 'regiao_imediata_regiao_intermediaria_uf_sigla',
          'nm_state'  = 'regiao_imediata_regiao_intermediaria_uf_nome',
          'cd_region' = 'regiao_imediata_regiao_intermediaria_uf_regiao_id',
          'sg_region' = 'regiao_imediata_regiao_intermediaria_uf_regiao_sigla',
          'nm_region' = 'regiao_imediata_regiao_intermediaria_uf_regiao_nome'
        ) %>% 
        dplyr::mutate(
          'cd_munic'  = as.character(cd_munic),
          'cd_micro'  = as.character(cd_micro),
          'cd_meso'   = as.character(cd_meso),
          'cd_rgime'  = as.character(cd_rgime),
          'cd_rgint'  = as.character(cd_rgint),
          'cd_state'  = as.character(cd_state),
          'cd_region' = as.character(cd_region)
        ) %>% 
        dplyr::select('cd_munic', 'nm_munic', 'cd_meso', 'nm_meso', 'cd_micro', 'nm_micro', 'cd_rgint', 'nm_rgint', 'cd_rgime', 'nm_rgime', 'cd_state', 'sg_state', 'nm_state', 'cd_region', 'sg_region', 'nm_region')
      
      rio::export(res, "volume/data/curated_data/munic/df_locations_munic.csv")
      
    } else if(req$status_code != 200){
      warning('Error: Bad request')
    }
  }
  
  if(grouped_by == "munic"){
    res %>% 
      select(cd_munic, nm_munic, cd_micro, nm_micro, cd_meso, nm_meso, cd_rgime, nm_rgime, cd_rgint, nm_rgint, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>% 
      rio::export(., "volume/data/curated_data/munic/df_locations_munic.csv")
  }else
    if(grouped_by == "micro"){
      res %>% 
        dplyr::group_by(cd_micro, nm_micro, cd_meso, nm_meso, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>% 
        dplyr::summarise(.groups = "drop") %>% 
        rio::export(., "volume/data/curated_data/micro/df_locations_micro.csv")
    }else
      if(grouped_by == "meso"){
        res %>% 
          dplyr::group_by(cd_meso, nm_meso, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>% 
          dplyr::summarise(.groups = "drop") %>% 
          rio::export(., "volume/data/curated_data/meso/df_locations_meso.csv")
      }else
        if(grouped_by == "rgime"){
          res %>% 
            dplyr::group_by(cd_rgime, nm_rgime, cd_rgint, nm_rgint, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>% 
            dplyr::summarise(.groups = "drop") %>% 
            rio::export(., "volume/data/curated_data/rgime/df_locations_rgime.csv")
        }else
          if(grouped_by == "rgint"){
            res %>% 
              dplyr::group_by(cd_rgint, nm_rgint, cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>% 
              dplyr::summarise(.groups = "drop") %>% 
              rio::export(., "volume/data/curated_data/rgint/df_locations_rgint.csv")
          }else
            if(grouped_by == "state"){
              res %>% 
                dplyr::group_by(cd_state, sg_state, nm_state, cd_region, sg_region, nm_region) %>% 
                dplyr::summarise(.groups = "drop") %>% 
                rio::export(., "volume/data/curated_data/state/df_locations_state.csv")
            }else
              if(grouped_by == "region"){
                res %>% 
                  dplyr::group_by(cd_region, sg_region, nm_region) %>% 
                  dplyr::summarise(.groups = "drop") %>% 
                  rio::export(., "volume/data/curated_data/reg/df_locations_reg.csv")
              }
  

}

# data_locations()

