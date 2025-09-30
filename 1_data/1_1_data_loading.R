# Data loading and initial preprocessing functions

library(tidyverse)
library(metafor)

load_pair_data <- function() {
  # Load pair matching information
  pair_data <- read.csv("/path/to/your/data/lnRR_match_info_environment.csv") %>%
    select(PlastisphereID, PlastisphereBigGroup, PlastisphereGroup, 
           CKenvironmentID, CKenvironmentGroup, CKenvironmentBigGroup) %>%
    filter(!is.na(PlastisphereID) | !is.na(CKenvironmentID)) %>% 
    filter(PlastisphereID != "" | CKenvironmentID != "")
  
  return(pair_data)
}

load_measurement_data <- function() {
  # Load abundance measurement data from multiple files
  bigtable <- read.csv("/path/to/your/data/acc_metadata_union_abun_df.csv") %>% 
    select(-ARG_normalized_abundance)
  
  # Combine different abundance measurements
  measurements <- bigtable %>% 
    select(SampleID = SampleName) %>% 
    left_join(
      read.csv("/path/to/your/data/abundance/ARG_category/acc_abun_cate.csv", row.names = 1) %>% 
        t %>% rowSums %>% as.data.frame() %>% 
        tibble::rownames_to_column("SampleID") %>% 
        rename(ACC_abun = "."),
      by = "SampleID"
    ) %>% 
    left_join(
      read.csv("/path/to/your/data/abundance/ARG_category/amcc_abun_cate.csv", row.names = 1) %>% 
        t %>% rowSums %>% as.data.frame() %>% 
        tibble::rownames_to_column("SampleID") %>% 
        rename(AMCC_abun = "."),
      by = "SampleID"
    ) %>% 
    # Add similar joins for other measurement types...
    pivot_longer(cols = names(.)[-1], names_to = "Variable", values_to = "Value")
  
  return(measurements)
}

load_group_info <- function() {
  # Load and preprocess group information
  groups_list <- read.csv("/path/to/your/data/lnRR_groups_ckenvironment_reviewed_longer.csv")
  
  # Standardize plastic type categories
  groups_list <- groups_list %>%
    mutate(
      PlasticType = case_when(
        !is.na(PlasticType) & PlasticType == "PF" ~ NA_character_,
        !is.na(PlasticType) & PlasticType == "PLLA" ~ "PLA",
        !is.na(PlasticType) & PlasticType %in% c("PHB", "PHBH", "PHBV", "P3HB4HB") ~ "PHA",
        !is.na(PlasticType) & PlasticType %in% c("HDPE", "LDPE", "LLDPE", "OXO-LLDPE") ~ "PE",
        !is.na(PlasticType) & PlasticType == "PBSA" ~ "PBS",
        TRUE ~ PlasticType
      ),
      # Create categorical variables for environmental factors
      Water.Temp.Class = cut(WaterTemperature, breaks=c(-Inf, 15, 25, Inf), 
                            labels = c("≤15°C", "15-25°C", ">25°C"), right = TRUE),
      pH.Class = cut(pH, breaks=c(-Inf, 7, 8, Inf), 
                    labels = c("pH (≤7)", "7-8", ">8")),
      DO.Class = cut(DO_mgL, breaks=c(-Inf, 2, 6, Inf), 
                    labels = c("DO (≤2)", "2-6", ">6"), right = TRUE),
      Depth.Class = cut(SamplingDepth, breaks=c(-Inf, 2, Inf), 
                       labels = c("Sampling depth (≤2)", ">2"), right = TRUE)
    )
  
  return(groups_list)
}