# R-script data_labor_CEMPRE-IBGE.R

# References --------------------------------------------------------------

# CEMPRE - Cadastro Central de Empresas

# https://sidra.ibge.gov.br/tabela/6450#/n6/all/v/706/p/last%201/c12762/116831,116866,116873,116881,116887,116897,116905,116911,116952,116960,116965,116985,116994,117007,117015,117029,117039,117048,117082,117089,117099,117136,117159,117179,117196,117229,117245,117261,117267,117297,117308,117311,117315,117326,117330,117335,117348,117364,117376,117438,117485,117501,117513,117521,117538,117544,117549,117556,117567,117575,117581,117594,117601,117609,117641,117654,117667,117674,117680,117685,117692,117697,117704,117711,117715,117731,117744,117752,117762,117775,117789,117811,117827,117835,117839,117844,117849,117852,117862,117875,117882,117889,117893,117897/l/v,p+c12762,t/cfg/nter,cod,/resultado
# https://sidra.ibge.gov.br/tabela/6450#/n6/all/v/706/p/2010/c12762/116831,116866,116873,116881,116887,116897,116905,116911,116952,116960,116965,116985,116994,117007,117015,117029,117039,117048,117082,117089,117099,117136,117159,117179,117196,117229,117245,117261,117267,117297,117308,117311,117315,117326,117330,117335,117348,117364,117376,117438,117485,117501,117513,117521,117538,117544,117549,117556,117567,117575,117581,117594,117601,117609,117641,117654,117667,117674,117680,117685,117692,117697,117704,117711,117715,117731,117744,117752,117762,117775,117789,117811,117827,117835,117839,117844,117849,117852,117862,117875,117882,117889,117893,117897/l/v,c12762,t+p/cfg/nter,cod,nt,/resultado


# Setup -------------------------------------------------------------------

# rm(list = ls())

# Function ----------------------------------------------------------------
data_labor_CEMPRE_IBGE <- function(grouped_by="micro"){
  
  df_locations <- readr::read_csv(paste0("volume/data/curated_data/munic/df_locations_munic.csv")) %>%
    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
    suppressMessages()
  
  df_firms <- readr::read_tsv("volume/data/clean_data/munic/tabela6450_cempre_firms_munic.tsv") %>%
    stats::setNames(c("cd_munic", "year", "firms_total", "firms_01_agriculture_and_livestock_services", "firms_02_forestry_production", "firms_03_fishing_and_aquaculture", "firms_05_coal_mining", "firms_07_metallic_mineral_extraction", "firms_08_non_metallic_mineral_extraction", "firms_09_mineral_extraction_support_activities", "firms_10_food_manufacturing", "firms_11_beverage_manufacturing", "firms_12_tobacco_manufacturing", "firms_13_textile_manufacturing", "firms_14_apparel_and_accessories_manufacturing", "firms_15_leather_goods_and_footwear_manufacturing", "firms_16_wood_product_manufacturing", "firms_17_paper_and_pulp_manufacturing", "firms_18_printing_and_recording_reproduction", "firms_19_petroleum_and_biofuels_manufacturing", "firms_20_chemical_manufacturing", "firms_21_pharmaceutical_manufacturing", "firms_22_rubber_and_plastics_manufacturing", "firms_23_non_metallic_minerals_manufacturing", "firms_24_metallurgy", "firms_25_metal_products_manufacturing_except_machinery", "firms_26_electronics_and_optical_equipment_manufacturing", "firms_27_electrical_equipment_manufacturing", "firms_28_machinery_and_equipment_manufacturing", "firms_29_automotive_vehicle_manufacturing", "firms_30_other_transport_equipment_manufacturing", "firms_31_furniture_manufacturing", "firms_32_miscellaneous_manufacturing", "firms_35_electricity_gas_and_other_utilities", "firms_36_water_supply_and_treatment", "firms_37_wastewater_treatment", "firms_38_waste_management_and_material_recovery", "firms_39_decontamination_and_waste_management_services", "firms_41_building_construction", "firms_42_infrastructure_construction", "firms_43_specialized_construction_services", "firms_45_vehicle_and_motorcycle_sales_and_repair", "firms_46_wholesale_trade_except_vehicles", "firms_47_retail_trade", "firms_49_land_transport", "firms_50_water_transport", "firms_51_air_transport", "firms_52_storage_and_transportation_support_activities", "firms_53_postal_and_delivery_services", "firms_55_accommodation", "firms_56_food_services", "firms_58_publishing_and_integrated_printing", "firms_59_film_and_tv_production", "firms_60_radio_and_television_activities", "firms_61_telecommunications", "firms_62_it_services", "firms_63_information_services", "firms_64_financial_services", "firms_65_insurance_and_pension_funding", "firms_66_financial_services_support_and_insurance", "firms_68_real_estate_activities", "firms_69_legal_accounting_and_auditing_services", "firms_70_business_management_and_consulting", "firms_71_architecture_engineering_and_testing_services", "firms_72_scientific_research_and_development", "firms_73_advertising_and_market_research", "firms_74_other_professional_scientific_and_technical_activities", "firms_75_veterinary_activities", "firms_77_non_real_estate_rentals_and_intangible_asset_management", "firms_78_staffing_and_labor_leasing", "firms_79_travel_agencies_and_tour_operators", "firms_80_security_and_investigation_activities", "firms_81_building_and_landscaping_services", "firms_82_office_and_administrative_support_services", "firms_84_public_administration_defense_and_social_security", "firms_85_education", "firms_86_human_healthcare_activities", "firms_87_healthcare_with_social_assistance", "firms_88_non_residential_social_assistance_services", "firms_90_artistic_and_creative_activities", "firms_91_cultural_and_environmental_heritage_activities", "firms_92_gambling_and_betting_activities", "firms_93_sports_recreation_and_leisure_activities", "firms_94_associative_organization_activities", "firms_95_equipment_and_personal_item_repair", "firms_96_other_personal_services", "firms_97_domestic_services", "firms_99_international_organizations_and_extraterritorial_institutions")) %>% 
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "-", "0"))) %>%
    dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
    dplyr::arrange(desc(firms_total)) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  df_labor <- readr::read_tsv("volume/data/clean_data/munic/tabela6450_cempre_labor_munic.tsv") %>%
    stats::setNames(c("cd_munic", "year", "labor_total", "labor_01_agriculture_and_livestock_services", "labor_02_forestry_production", "labor_03_fishing_and_aquaculture", "labor_05_coal_mining", "labor_07_metallic_mineral_extraction", "labor_08_non_metallic_mineral_extraction", "labor_09_mineral_extraction_support_activities", "labor_10_food_manufacturing", "labor_11_beverage_manufacturing", "labor_12_tobacco_manufacturing", "labor_13_textile_manufacturing", "labor_14_apparel_and_accessories_manufacturing", "labor_15_leather_goods_and_footwear_manufacturing", "labor_16_wood_product_manufacturing", "labor_17_paper_and_pulp_manufacturing", "labor_18_printing_and_recording_reproduction", "labor_19_petroleum_and_biofuels_manufacturing", "labor_20_chemical_manufacturing", "labor_21_pharmaceutical_manufacturing", "labor_22_rubber_and_plastics_manufacturing", "labor_23_non_metallic_minerals_manufacturing", "labor_24_metallurgy", "labor_25_metal_products_manufacturing_except_machinery", "labor_26_electronics_and_optical_equipment_manufacturing", "labor_27_electrical_equipment_manufacturing", "labor_28_machinery_and_equipment_manufacturing", "labor_29_automotive_vehicle_manufacturing", "labor_30_other_transport_equipment_manufacturing", "labor_31_furniture_manufacturing", "labor_32_miscellaneous_manufacturing", "labor_35_electricity_gas_and_other_utilities", "labor_36_water_supply_and_treatment", "labor_37_wastewater_treatment", "labor_38_waste_management_and_material_recovery", "labor_39_decontamination_and_waste_management_services", "labor_41_building_construction", "labor_42_infrastructure_construction", "labor_43_specialized_construction_services", "labor_45_vehicle_and_motorcycle_sales_and_repair", "labor_46_wholesale_trade_except_vehicles", "labor_47_retail_trade", "labor_49_land_transport", "labor_50_water_transport", "labor_51_air_transport", "labor_52_storage_and_transportation_support_activities", "labor_53_postal_and_delivery_services", "labor_55_accommodation", "labor_56_food_services", "labor_58_publishing_and_integrated_printing", "labor_59_film_and_tv_production", "labor_60_radio_and_television_activities", "labor_61_telecommunications", "labor_62_it_services", "labor_63_information_services", "labor_64_financial_services", "labor_65_insurance_and_pension_funding", "labor_66_financial_services_support_and_insurance", "labor_68_real_estate_activities", "labor_69_legal_accounting_and_auditing_services", "labor_70_business_management_and_consulting", "labor_71_architecture_engineering_and_testing_services", "labor_72_scientific_research_and_development", "labor_73_advertising_and_market_research", "labor_74_other_professional_scientific_and_technical_activities", "labor_75_veterinary_activities", "labor_77_non_real_estate_rentals_and_intangible_asset_management", "labor_78_staffing_and_labor_leasing", "labor_79_travel_agencies_and_tour_operators", "labor_80_security_and_investigation_activities", "labor_81_building_and_landscaping_services", "labor_82_office_and_administrative_support_services", "labor_84_public_administration_defense_and_social_security", "labor_85_education", "labor_86_human_healthcare_activities", "labor_87_healthcare_with_social_assistance", "labor_88_non_residential_social_assistance_services", "labor_90_artistic_and_creative_activities", "labor_91_cultural_and_environmental_heritage_activities", "labor_92_gambling_and_betting_activities", "labor_93_sports_recreation_and_leisure_activities", "labor_94_associative_organization_activities", "labor_95_equipment_and_personal_item_repair", "labor_96_other_personal_services", "labor_97_domestic_services", "labor_99_international_organizations_and_extraterritorial_institutions")) %>% 
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "X", min(as.numeric(.), na.rm = TRUE)))) %>% # THIS IS DATA SCIENCE YO!
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "-", "0"))) %>%
    dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
    dplyr::arrange(desc(labor_total)) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  df_laborwsalary <- readr::read_tsv("volume/data/clean_data/munic/tabela6450_cempre_laborwsalary_munic.tsv") %>%
    stats::setNames(c("cd_munic", "year", "laborwsalary_total", "laborwsalary_01_agriculture_and_livestock_services", "laborwsalary_02_forestry_production", "laborwsalary_03_fishing_and_aquaculture", "laborwsalary_05_coal_mining", "laborwsalary_07_metallic_mineral_extraction", "laborwsalary_08_non_metallic_mineral_extraction", 
                      "laborwsalary_09_mineral_extraction_support_activities", "laborwsalary_10_food_manufacturing", "laborwsalary_11_beverage_manufacturing", "laborwsalary_12_tobacco_manufacturing", "laborwsalary_13_textile_manufacturing", "laborwsalary_14_apparel_and_accessories_manufacturing", "laborwsalary_15_leather_goods_and_footwear_manufacturing", "laborwsalary_16_wood_product_manufacturing", "laborwsalary_17_paper_and_pulp_manufacturing", "laborwsalary_18_printing_and_recording_reproduction", "laborwsalary_19_petroleum_and_biofuels_manufacturing", "laborwsalary_20_chemical_manufacturing", "laborwsalary_21_pharmaceutical_manufacturing", "laborwsalary_22_rubber_and_plastics_manufacturing", "laborwsalary_23_non_metallic_minerals_manufacturing", "laborwsalary_24_metallurgy", "laborwsalary_25_metal_products_manufacturing_except_machinery", "laborwsalary_26_electronics_and_optical_equipment_manufacturing", "laborwsalary_27_electrical_equipment_manufacturing", "laborwsalary_28_machinery_and_equipment_manufacturing", "laborwsalary_29_automotive_vehicle_manufacturing", "laborwsalary_30_other_transport_equipment_manufacturing", "laborwsalary_31_furniture_manufacturing", "laborwsalary_32_miscellaneous_manufacturing", "laborwsalary_35_electricity_gas_and_other_utilities", "laborwsalary_36_water_supply_and_treatment", "laborwsalary_37_wastewater_treatment", "laborwsalary_38_waste_management_and_material_recovery", "laborwsalary_39_decontamination_and_waste_management_services", "laborwsalary_41_building_construction", "laborwsalary_42_infrastructure_construction", "laborwsalary_43_specialized_construction_services", "laborwsalary_45_vehicle_and_motorcycle_sales_and_repair", "laborwsalary_46_wholesale_trade_except_vehicles", "laborwsalary_47_retail_trade", "laborwsalary_49_land_transport", "laborwsalary_50_water_transport", "laborwsalary_51_air_transport", "laborwsalary_52_storage_and_transportation_support_activities", "laborwsalary_53_postal_and_delivery_services", "laborwsalary_55_accommodation", "laborwsalary_56_food_services", "laborwsalary_58_publishing_and_integrated_printing", "laborwsalary_59_film_and_tv_production", "laborwsalary_60_radio_and_television_activities", "laborwsalary_61_telecommunications", "laborwsalary_62_it_services", "laborwsalary_63_information_services", "laborwsalary_64_financial_services", "laborwsalary_65_insurance_and_pension_funding", "laborwsalary_66_financial_services_support_and_insurance", "laborwsalary_68_real_estate_activities", "laborwsalary_69_legal_accounting_and_auditing_services", "laborwsalary_70_business_management_and_consulting", "laborwsalary_71_architecture_engineering_and_testing_services", "laborwsalary_72_scientific_research_and_development", "laborwsalary_73_advertising_and_market_research", "laborwsalary_74_other_professional_scientific_and_technical_activities", "laborwsalary_75_veterinary_activities", "laborwsalary_77_non_real_estate_rentals_and_intangible_asset_management", "laborwsalary_78_staffing_and_labor_leasing", "laborwsalary_79_travel_agencies_and_tour_operators", "laborwsalary_80_security_and_investigation_activities", "laborwsalary_81_building_and_landscaping_services", "laborwsalary_82_office_and_administrative_support_services", "laborwsalary_84_public_administration_defense_and_social_security", "laborwsalary_85_education", "laborwsalary_86_human_healthcare_activities", "laborwsalary_87_healthcare_with_social_assistance", "laborwsalary_88_non_residential_social_assistance_services", "laborwsalary_90_artistic_and_creative_activities", "laborwsalary_91_cultural_and_environmental_heritage_activities", "laborwsalary_92_gambling_and_betting_activities", "laborwsalary_93_sports_recreation_and_leisure_activities", "laborwsalary_94_associative_organization_activities", "laborwsalary_95_equipment_and_personal_item_repair", "laborwsalary_96_other_personal_services", "laborwsalary_97_domestic_services", "laborwsalary_99_international_organizations_and_extraterritorial_institutions")) %>% 
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "X", min(as.numeric(.), na.rm = TRUE)))) %>% # THIS IS DATA SCIENCE YO!
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "-", "0"))) %>%
    dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
    dplyr::arrange(desc(laborwsalary_total)) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  df_salary <- readr::read_tsv("volume/data/clean_data/munic/tabela6450_cempre_salary_munic.tsv") %>%
    stats::setNames(c("cd_munic", "year", "salary_total", "salary_01_agriculture_and_livestock_services", "salary_02_forestry_production", "salary_03_fishing_and_aquaculture", "salary_05_coal_mining", "salary_07_metallic_mineral_extraction", "salary_08_non_metallic_mineral_extraction", "salary_09_mineral_extraction_support_activities", "salary_10_food_manufacturing", "salary_11_beverage_manufacturing", "salary_12_tobacco_manufacturing", "salary_13_textile_manufacturing", "salary_14_apparel_and_accessories_manufacturing", "salary_15_leather_goods_and_footwear_manufacturing", "salary_16_wood_product_manufacturing", "salary_17_paper_and_pulp_manufacturing", "salary_18_printing_and_recording_reproduction", "salary_19_petroleum_and_biofuels_manufacturing", "salary_20_chemical_manufacturing", "salary_21_pharmaceutical_manufacturing", "salary_22_rubber_and_plastics_manufacturing", "salary_23_non_metallic_minerals_manufacturing", "salary_24_metallurgy", "salary_25_metal_products_manufacturing_except_machinery", "salary_26_electronics_and_optical_equipment_manufacturing", "salary_27_electrical_equipment_manufacturing", "salary_28_machinery_and_equipment_manufacturing", "salary_29_automotive_vehicle_manufacturing", "salary_30_other_transport_equipment_manufacturing", "salary_31_furniture_manufacturing", "salary_32_miscellaneous_manufacturing", "salary_35_electricity_gas_and_other_utilities", "salary_36_water_supply_and_treatment", "salary_37_wastewater_treatment", "salary_38_waste_management_and_material_recovery", "salary_39_decontamination_and_waste_management_services", "salary_41_building_construction", "salary_42_infrastructure_construction", "salary_43_specialized_construction_services", "salary_45_vehicle_and_motorcycle_sales_and_repair", "salary_46_wholesale_trade_except_vehicles", "salary_47_retail_trade", "salary_49_land_transport", "salary_50_water_transport", "salary_51_air_transport", "salary_52_storage_and_transportation_support_activities", "salary_53_postal_and_delivery_services", "salary_55_accommodation", "salary_56_food_services", "salary_58_publishing_and_integrated_printing", "salary_59_film_and_tv_production", "salary_60_radio_and_television_activities", "salary_61_telecommunications", "salary_62_it_services", "salary_63_information_services", "salary_64_financial_services", "salary_65_insurance_and_pension_funding", "salary_66_financial_services_support_and_insurance", "salary_68_real_estate_activities", "salary_69_legal_accounting_and_auditing_services", "salary_70_business_management_and_consulting", "salary_71_architecture_engineering_and_testing_services", "salary_72_scientific_research_and_development", "salary_73_advertising_and_market_research", "salary_74_other_professional_scientific_and_technical_activities", "salary_75_veterinary_activities", "salary_77_non_real_estate_rentals_and_intangible_asset_management", "salary_78_staffing_and_labor_leasing", "salary_79_travel_agencies_and_tour_operators", "salary_80_security_and_investigation_activities", "salary_81_building_and_landscaping_services", "salary_82_office_and_administrative_support_services", "salary_84_public_administration_defense_and_social_security", "salary_85_education", "salary_86_human_healthcare_activities", "salary_87_healthcare_with_social_assistance", "salary_88_non_residential_social_assistance_services", "salary_90_artistic_and_creative_activities", "salary_91_cultural_and_environmental_heritage_activities", "salary_92_gambling_and_betting_activities", "salary_93_sports_recreation_and_leisure_activities", "salary_94_associative_organization_activities", "salary_95_equipment_and_personal_item_repair", "salary_96_other_personal_services", "salary_97_domestic_services", "salary_99_international_organizations_and_extraterritorial_institutions")) %>% 
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "X", min(as.numeric(.), na.rm = TRUE)))) %>% # Here, I rather estimate the salaries in "X"
    dplyr::mutate(dplyr::across(everything(), ~ replace(., . == "-", "0"))) %>%
    dplyr::mutate(dplyr::across(3:ncol(.), as.numeric)) %>%
    dplyr::arrange(desc(salary_total)) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  
  
  df_ratio <- dplyr::inner_join(df_laborwsalary, df_salary, by = c("cd_munic", "year")) %>% 
    dplyr::mutate(
      dplyr::across(
        starts_with("salary_"),
        ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
        .names = "mean_{.col}"
      )
    ) %>% 
    dplyr::select(cd_munic, year, starts_with("mean_"))
  
  # Oportunity:
  # mean size of firms in BR, as by deviding firms by labor
  df_cempre <- df_locations %>%
    dplyr::left_join(., df_firms) %>%
    dplyr::left_join(., df_labor) %>%
    dplyr::left_join(., df_laborwsalary) %>%
    dplyr::left_join(., df_salary) %>%
    # dplyr::left_join(., df_ratio) %>%
    dplyr::arrange(desc(firms_total))
  
  if (grouped_by == "munic") {
    df_cempre %>%
      dplyr::mutate( # Mean salary per labor
        dplyr::across(
          starts_with("salary_"),
          ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
          .names = "mean_{.col}"
        )
      ) %>% 
      dplyr::select(-cd_micro, -nm_micro, -cd_meso, -nm_meso, -cd_state, -sg_state, -nm_state, -cd_region, -sg_region, -nm_region, -cd_rgime, -nm_rgime, -cd_rgint, -nm_rgint) %>%
      rio::export(., "volume/data/curated_data/munic/df_cempre_munic.csv")
  } else if (grouped_by == "micro") {
    df_cempre %>%
      dplyr::group_by(cd_micro, year) %>%
      dplyr::summarise(
        across(
          .cols = matches("^firms|^labor|^salary"),
          .fns = \(x) sum(x, na.rm = TRUE) # Use an anonymous function for additional arguments
        ),
        .groups = "drop"
      ) %>% 
      dplyr::mutate( # Mean salary per labor
        dplyr::across(
          starts_with("salary_"),
          ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
          .names = "mean_{.col}"
        )
      ) %>%
      rio::export(., "volume/data/curated_data/micro/df_cempre_micro.csv")
  } else if (grouped_by == "meso") {
    exp_munic_plus %>%
      dplyr::group_by(cd_meso, year) %>%
      dplyr::summarise(
        across(
          .cols = matches("^firms|^labor|^salary"),
          .fns = \(x) sum(x, na.rm = TRUE) # Use an anonymous function for additional arguments
        ),
        .groups = "drop"
      ) %>% 
      dplyr::mutate( # Mean salary per labor
        dplyr::across(
          starts_with("salary_"),
          ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
          .names = "mean_{.col}"
        )
      ) %>%
      rio::export(., "volume/data/curated_data/meso/df_cempre_meso.csv")
  } else if (grouped_by == "rgime") {
    exp_munic_plus %>%
      dplyr::group_by(cd_rgime, year) %>%
      dplyr::summarise(
        across(
          .cols = matches("^firms|^labor|^salary"),
          .fns = \(x) sum(x, na.rm = TRUE) # Use an anonymous function for additional arguments
        ),
        .groups = "drop"
      ) %>% 
      dplyr::mutate( # Mean salary per labor
        dplyr::across(
          starts_with("salary_"),
          ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
          .names = "mean_{.col}"
        )
      ) %>%
      rio::export(., "volume/data/curated_data/rgime/df_cempre_rgime.csv")
  } else if (grouped_by == "rgint") {
    exp_munic_plus %>%
      dplyr::group_by(cd_rgint, year) %>%
      dplyr::summarise(
        across(
          .cols = matches("^firms|^labor|^salary"),
          .fns = \(x) sum(x, na.rm = TRUE) # Use an anonymous function for additional arguments
        ),
        .groups = "drop"
      ) %>% 
      dplyr::mutate( # Mean salary per labor
        dplyr::across(
          starts_with("salary_"),
          ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
          .names = "mean_{.col}"
        )
      ) %>%
      rio::export(., "volume/data/curated_data/rgint/df_cempre_rgint.csv")
  } else if (grouped_by == "state") {
    exp_munic_plus %>%
      dplyr::group_by(sg_state, year) %>%
      dplyr::summarise(
        across(
          .cols = matches("^firms|^labor|^salary"),
          .fns = \(x) sum(x, na.rm = TRUE) # Use an anonymous function for additional arguments
        ),
        .groups = "drop"
      ) %>% 
      dplyr::mutate( # Mean salary per labor
        dplyr::across(
          starts_with("salary_"),
          ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
          .names = "mean_{.col}"
        )
      ) %>%
      rio::export(., "volume/data/curated_data/state/df_cempre_state.csv")
  } else if (grouped_by == "macro") {
    exp_munic_plus %>%
      dplyr::group_by(sg_region, year) %>%
      dplyr::summarise(
        across(
          .cols = matches("^firms|^labor|^salary"),
          .fns = \(x) sum(x, na.rm = TRUE) # Use an anonymous function for additional arguments
        ),
        .groups = "drop"
      ) %>% 
      dplyr::mutate( # Mean salary per labor
        dplyr::across(
          starts_with("salary_"),
          ~ .x / get(gsub("salary_", "laborwsalary_", cur_column())),
          .names = "mean_{.col}"
        )
      ) %>%
      rio::export(., "volume/data/curated_data/macro/df_cempre_macro.csv")
  }

}


# data_labor <- data_labor_CEMPRE_IBGE()
