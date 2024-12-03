# R-script data_habes.R

# References --------------------------------------------------------------

# https://sidra.ibge.gov.br/Tabela/3543
habesi = c("Total", "Educação", "Humanidades e artes", "Ciências sociais, negócios e direito", "Ciências, matemática e computação", "Engenharia, produção e construção", "Agricultura e veterinária", "Saúde e bem-estar-social", "Serviços")

# Function ----------------------------------------------------------------

data_education <- function(grouped_by="micro"){
  
  # Setup
  # rm(list = ls())
  gc()
  
  # Load location data
  df_locations <- readr::read_csv(paste0("volume/data/curated_data/", grouped_by, "/df_locations_", grouped_by, ".csv")) %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()
  
  # Load inhabitants 2010 data
  df_highereducation <- readr::read_tsv("volume/data/clean_data/micro/tabela3543_higherEducation_micro.tsv") %>%
    stats::setNames(c("cd_micro", "nm_micro", "inhabitants_with_higherEducation_total", "inhabitants_with_higherEducation_education", "inhabitants_with_higherEducation_humanities_arts", "inhabitants_with_higherEducation_socialSciences_business_law", "inhabitants_with_higherEducation_math_computing",	"inhabitants_with_higherEducation_engineering_production_construction",	"inhabitants_with_higherEducation_agriculture_veterinary",	"inhabitants_with_higherEducation_health_socialWellBeing",	"inhabitants_with_higherEducation_services", "inhabitants_with_bachelors_total", "inhabitants_with_bachelors_education", "inhabitants_with_bachelors_humanities_arts", "inhabitants_with_bachelors_socialSciences_business_law", "inhabitants_with_bachelors_math_computing",	"inhabitants_with_bachelors_engineering_production_construction",	"inhabitants_with_bachelors_agriculture_veterinary",	"inhabitants_with_bachelors_health_socialWellBeing",	"inhabitants_with_bachelors_services", "inhabitants_with_masters_total", "inhabitants_with_masters_education", "inhabitants_with_masters_humanities_arts", "inhabitants_with_masters_socialSciences_business_law", "inhabitants_with_masters_math_computing",	"inhabitants_with_masters_engineering_production_construction",	"inhabitants_with_masters_agriculture_veterinary",	"inhabitants_with_masters_health_socialWellBeing",	"inhabitants_with_masters_services", "inhabitants_with_phd_total", "inhabitants_with_phd_education", "inhabitants_with_phd_humanities_arts", "inhabitants_with_phd_socialSciences_business_law", "inhabitants_with_phd_math_computing",	"inhabitants_with_phd_engineering_production_construction",	"inhabitants_with_phd_agriculture_veterinary",	"inhabitants_with_phd_health_socialWellBeing",	"inhabitants_with_phd_services")) %>% 
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "-", "0"))) %>%
    dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
    dplyr::arrange(desc(inhabitants_with_higherEducation_total)) %>%
    dplyr::select(-nm_micro) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  df_lowereducation <- readr::read_tsv(file = "volume/data/clean_data/micro/tabela1554_lowerEducation_micro.tsv") %>% 
    dplyr::select(cd_micro=Cód., nm_micro=2, out=3, inhabitants_without_fundamental_education=4, inhabitants_with_fundamental_education=5, inhabitants_with_middle_education=6, inhabitants_with_higher_education=7, notdetermined=8) %>% 
    dplyr::select(-nm_micro, -out, -notdetermined) %>% 
    dplyr::mutate(cd_micro=as.character(cd_micro)) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  
  df_education <- dplyr::left_join(df_highereducation, df_lowereducation)

  df_education %>% 
    rio::export(., "volume/data/curated_data/micro/df_education_micro.csv")  
  

}


