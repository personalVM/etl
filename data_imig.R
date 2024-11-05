# R-script data_imig.R


# References --------------------------------------------------------------


# data_imig function ---------------------------------------------------
data_imig <- function(grouped_by="micro"){
  
  # Getting BR location info
  source("util_loadPackages.R")
  source("data_locations.R")
  df_locations <- data_loc()
  
  if(grouped_by == "mun"){
    df_imig <- readr::read_tsv("clean_bucket/munic/tabela2145_imig_mun.tsv") %>%
      janitor::clean_names(.) %>% 
      dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
      dplyr::mutate(immigrants=as.numeric(total)) %>%
      dplyr::select(cd_mun=cod, immigrants) %>% 
      dplyr::arrange(desc(immigrants)) %>%
      suppressMessages() %>% 
      suppressWarnings()
    rio::export(df_imig, "curated_bucket/munic/df_immigrants.csv")
  }else
    if(grouped_by == "micro"){
        df_imig <- readr::read_tsv("clean_bucket/micro/tabela2145_imig_micro.tsv") %>%
          janitor::clean_names(.) %>% 
          dplyr::mutate(across(everything(), ~ replace(., . == "-", "0"))) %>%
          dplyr::mutate(immigrants=as.numeric(total)) %>%
          dplyr::select(cd_micro=cod, immigrants) %>% 
          dplyr::arrange(desc(immigrants)) %>%
          suppressMessages() %>% 
          suppressWarnings()
        rio::export(df_imig, "curated_bucket/micro/df_immigrants.csv")
    }


  return(df_imig)

}

# df_imig = data_imig(grouped_by = "mun")

# ggplot(df_imig[1:20, ], aes(x = reorder(nm_micro, imig), y = imig)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(title = "Total Imigration by Micro-Region", x = "Micro-Region", y = "Total Imigration") +
#   theme_minimal()




