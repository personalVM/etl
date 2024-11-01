# R-script data_loc.R


# References --------------------------------------------------------------

# API de serviços IBGE
# https://servicodados.ibge.gov.br/
# https://servicodados.ibge.gov.br/api/v1/localidades/estados/


# Function ----------------------------------------------------------------
# Função para obter dados das localizações e divisões regionais do Brasil através da API de serviços IBGE
data_loc <- function(
    grouped_by="mun",
    use_backup=T
  ){
  
  # Setup
  # rm(list = ls())
  gc()
  options(scipen = 666, stringsAsFactors = F)
  source("volume/etl/util_loadPackages.R")
  
  if(use_backup==T){
    
    # Read back df_locatons.csv data
    res <- readr::read_csv("volume/data/curated_data/munic/df_locations_mun.csv") %>% 
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
          'cd_mun'   = 'id',
          'nm_mun'   = 'nome',
          'cd_meso'  = 'microrregiao_mesorregiao_id',
          'nm_meso'  = 'microrregiao_mesorregiao_nome',
          'cd_micro' = 'microrregiao_id',
          'nm_micro' = 'microrregiao_nome',
          # 'cd_uf'='microrregiao_mesorregiao_uf_id',
          # 'sg_uf'='microrregiao_mesorregiao_uf_sigla',
          # 'nm_uf'='microrregiao_mesorregiao_uf_nome',
          # 'cd_rg'='microrregiao_mesorregiao_uf_regiao_id',
          # 'sg_rg'='microrregiao_mesorregiao_uf_regiao_sigla',
          # 'nm_rg'='microrregiao_mesorregiao_uf_regiao_nome',
          'cd_rgint' = 'regiao_imediata_regiao_intermediaria_id',
          'nm_rgint' = 'regiao_imediata_regiao_intermediaria_nome',
          'cd_rgime' = 'regiao_imediata_id',
          'nm_rgime' = 'regiao_imediata_nome',
          'cd_uf'    = 'regiao_imediata_regiao_intermediaria_uf_id',
          'sg_uf'    = 'regiao_imediata_regiao_intermediaria_uf_sigla',
          'nm_uf'    = 'regiao_imediata_regiao_intermediaria_uf_nome',
          'cd_rg'    = 'regiao_imediata_regiao_intermediaria_uf_regiao_id',
          'sg_rg'    = 'regiao_imediata_regiao_intermediaria_uf_regiao_sigla',
          'nm_rg'    = 'regiao_imediata_regiao_intermediaria_uf_regiao_nome'
        ) %>% 
        dplyr::mutate(
          'cd_mun'   = as.character(cd_mun),
          'cd_meso'  = as.character(cd_meso),
          'cd_micro' = as.character(cd_micro),
          'cd_rgint' = as.character(cd_rgint),
          'cd_rgime' = as.character(cd_rgime),
          'cd_uf'    = as.character(cd_uf),
          'cd_rg'    = as.character(cd_rg)
        ) %>% 
        dplyr::select('cd_mun', 'nm_mun', 'cd_meso', 'nm_meso', 'cd_micro', 'nm_micro', 'cd_rgint', 'nm_rgint', 'cd_rgime', 'nm_rgime', 'cd_uf', 'sg_uf', 'nm_uf', 'cd_rg', 'sg_rg', 'nm_rg')
      
      rio::export(res, "data/curated_data/munic/df_locations.csv")
      
    } else if(req$status_code != 200){
      warning('Error: Bad request')
    }
  }
  
  if(grouped_by == "mun"){
    res <- res
  }else
    if(grouped_by == "micro"){
      res <- res %>% 
        dplyr::group_by(cd_micro, nm_micro, sg_uf, nm_uf, sg_rg, nm_rg) %>% 
        dplyr::summarise(.groups = "drop")
    }else
      if(grouped_by == "meso"){
        res <- res %>% 
          dplyr::group_by(cd_meso, nm_meso, sg_uf, nm_uf, sg_rg, nm_rg) %>% 
          dplyr::summarise(.groups = "drop")
      }else
        if(grouped_by == "rgime"){
          res <- res %>% 
            dplyr::group_by(cd_rgime, nm_rgime, sg_uf, nm_uf, sg_rg, nm_rg) %>% 
            dplyr::summarise(.groups = "drop")
        }else
          if(grouped_by == "rgint"){
            res <- res %>% 
              dplyr::group_by(cd_rgint, nm_rgint, sg_uf, nm_uf, sg_rg, nm_rg) %>% 
              dplyr::summarise(.groups = "drop")
          }else
            if(grouped_by == "uf"){
              res <- res %>% 
                dplyr::group_by(cd_uf, sg_uf, nm_uf, sg_rg, nm_rg) %>% 
                dplyr::summarise(.groups = "drop")
            }else
              if(grouped_by == "rg"){
                res <- res %>% 
                  dplyr::group_by(cd_rg, sg_rg, nm_rg) %>% 
                  dplyr::summarise(.groups = "drop")
              }
  
  res <- res %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(dplyr::across(everything(), as.character))
  
  return(res)
  
}

# data_loc()

