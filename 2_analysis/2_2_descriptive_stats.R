# Descriptive statistics calculation functions

library(tidyverse)

calculate_descriptive_stats <- function(pair_data, measurements_longer, groups_list, group_vars, base_path, folder) {
  # Calculate descriptive statistics for all variables
  stats_all <- list()
  
  # Prepare combined dataset for statistics
  pair_data_combined <- prepare_combined_data(pair_data, measurements_longer, groups_list)
  
  for (var_name in unique(pair_data_combined$Variable)) {
    cat("Calculating stats for:", var_name, "\n")
    
    df_var <- pair_data_combined %>% filter(Variable == var_name)
    
    # Calculate overall statistics
    stats_df <- calculate_group_stats(df_var, "Total")
    
    # Calculate statistics for each subgroup
    for (group_var in group_vars) {
      if (group_var %in% names(df_var)) {
        stats_df <- calculate_subgroup_stats(df_var, group_var, stats_df)
      }
    }
    
    # Add spacers for visualization alignment
    stats_df <- add_stat_spacers(stats_df, group_vars)
    stats_all[[var_name]] <- stats_df
    
    # Save individual statistics
    write.csv(stats_all[[var_name]], 
              file = paste0(base_path, folder, "meta_stats_ckenvironment_", var_name, ".csv"), 
              row.names = FALSE)
  }
  
  return(stats_all)
}

prepare_combined_data <- function(pair_data, measurements, groups_list) {
  # Prepare combined dataset for statistical analysis
  plastisphere_data <- pair_data %>%
    select(SampleID = PlastisphereID, Group = PlastisphereGroup) %>%
    filter(SampleID != "") %>% 
    mutate(Treatment = "Plastisphere")
  
  control_data <- pair_data %>%
    select(SampleID = CKenvironmentID, Group = CKenvironmentGroup) %>%
    filter(SampleID != "") %>% 
    mutate(Treatment = "environment")
  
  # Combine and join with measurements and group info
  combined_data <- bind_rows(plastisphere_data, control_data) %>%
    left_join(measurements, by = "SampleID") %>%
    left_join(groups_list, by = c("Group" = "PlastisphereGroup")) 
  
  return(combined_data)
}

calculate_group_stats <- function(df, group_label) {
  # Calculate statistics for a group
  stats <- df %>% 
    group_by(Treatment) %>% 
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      n = unique(Group) %>% length,
      sd = sd(Value, na.rm = TRUE),
      se = sd / sqrt(n)
    ) %>% 
    mutate(var = group_label)
  
  return(stats)
}

calculate_subgroup_stats <- function(df_var, group_var, stats_df) {
  # Calculate statistics for subgroups
  group_levels <- get_group_levels(df_var, group_var)
  
  for (level in group_levels) {
    df_level <- df_var %>% filter(.data[[group_var]] == level)
    
    # Skip if insufficient groups
    if (length(unique(df_level$Group)) < 3) next
    
    tryCatch({
      level_stats <- calculate_group_stats(df_level, level)
      stats_df <- bind_rows(stats_df, level_stats)
    }, error = function(e) {
      message(paste("Error in", group_var, level, ":", e$message))
    })
  }
  
  return(stats_df)
}

get_group_levels <- function(df_var, group_var) {
  # Get valid group levels for a grouping variable
  if (grepl("Class$", group_var)) {
    group_levels <- levels(df_var[[group_var]]) %>% setdiff(c("Forbidden", "Lab"))
  } else {
    group_levels <- df_var %>% 
      filter(!is.na(.data[[group_var]])) %>% 
      distinct(.data[[group_var]]) %>% 
      pull() %>% setdiff(c("Forbidden", "Lab"))
  }
  
  return(group_levels)
}

add_stat_spacers <- function(stats_df, group_vars) {
  # Add spacer rows for visualization alignment
  spacers <- expand.grid(
    var = LETTERS[1:length(group_vars)], 
    Treatment = unique(stats_df$Treatment)
  ) %>% 
    mutate(Mean = 10, n = 0, sd = 0, se = 0)
  
  return(bind_rows(stats_df, spacers))
}