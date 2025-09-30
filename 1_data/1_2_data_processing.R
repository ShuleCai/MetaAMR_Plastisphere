# Data processing and merging functions

library(tidyverse)
library(metafor)

create_group_mapping <- function(pair_data) {
  # Create mapping between plastisphere and control groups
  group_mapping <- pair_data %>%
    distinct(PlastisphereBigGroup, CKenvironmentBigGroup) %>%
    filter(CKenvironmentBigGroup != "" & PlastisphereBigGroup != "")
  
  return(group_mapping)
}

process_plastisphere_data <- function(pair_data, measurements) {
  # Process plastisphere sample data
  plastisphere <- pair_data %>%
    select(SampleID = PlastisphereID, Group = PlastisphereGroup) %>%
    filter(SampleID != "") %>%
    left_join(measurements, by = "SampleID") %>%
    group_by(Group, Variable) %>%
    summarise(
      Mean_t = mean(Value, na.rm = TRUE),
      SD_t = sd(Value, na.rm = TRUE),
      N_t = n(),
      .groups = "drop"
    ) %>% 
    rename(PlastisphereBigGroup = Group)
  
  return(plastisphere)
}

process_control_data <- function(pair_data, measurements) {
  # Process control environment sample data
  control <- pair_data %>%
    select(SampleID = CKenvironmentID, Group = CKenvironmentGroup) %>%
    filter(SampleID != "") %>%
    left_join(measurements, by = "SampleID") %>%
    group_by(Group, Variable) %>%
    summarise(
      Mean_c = mean(Value, na.rm = TRUE),
      SD_c = sd(Value, na.rm = TRUE),
      N_c = n(),
      .groups = "drop"
    ) %>% 
    rename(CKenvironmentBigGroup = Group)
  
  return(control)
}

merge_meta_data <- function(plastisphere, control, group_mapping) {
  # Merge plastisphere and control data for meta-analysis
  meta_data <- plastisphere %>%
    left_join(group_mapping, by = "PlastisphereBigGroup") %>%
    left_join(control, by = c("CKenvironmentBigGroup", "Variable")) %>%
    mutate(Study = str_extract(PlastisphereBigGroup, "^[A-Za-z]+[0-9]+"))
  
  return(meta_data)
}

calculate_effect_sizes <- function(meta_data) {
  # Calculate effect sizes (lnRR) and variances for meta-analysis
  processed_data <- meta_data %>%
    mutate(
      lnRR = log(Mean_t / Mean_c),
      v = (SD_t^2) / (N_t * Mean_t^2) + (SD_c^2) / (N_c * Mean_c^2)
    ) %>% 
    arrange(Variable, Study)
  
  return(processed_data)
}