# Heterogeneity analysis functions

library(tidyverse)
library(metafor)

analyze_heterogeneity <- function(processed_data_with_groups, groups_list, group_vars, base_path, folder) {
  # Perform heterogeneity analysis for all variables
  for (var in unique(processed_data_with_groups$Variable)) {
    cat("Analyzing heterogeneity for:", var, "\n")
    
    heterogeneity <- data.frame()
    
    for (group in group_vars) {
      # Prepare data for current grouping variable
      df_var <- prepare_heterogeneity_data(processed_data_with_groups, groups_list, var, group)
      
      # Handle zero variances
      if (any(df_var$v == 0)) {
        df_var[which(df_var$v == 0), ]$v <- 5e-6
      }
      
      # Perform meta-regression to assess heterogeneity
      tryCatch({
        res_cat <- rma(yi = lnRR, vi = v, 
                       data = df_var,
                       mods = ~ Group - 1,
                       method = "DL")
        
        # Extract heterogeneity statistics
        heterogeneity <- rbind(heterogeneity, data.frame(
          Variable = var,
          group = group,
          QT = res_cat$QE,  # Total heterogeneity
          QW = res_cat$QE,  # Within-group heterogeneity
          QB = res_cat$QM,  # Between-group heterogeneity
          p_value = res_cat$QMp
        ))
      }, error = function(e) {
        message(paste("Error in heterogeneity analysis for", var, group, ":", e$message))
      })
    }
    
    # Save heterogeneity results
    write.csv(heterogeneity, 
              paste0(base_path, folder, "heterogeneity/", "ckenvironment_heterogeneity_", var, ".csv"), 
              row.names = FALSE)
  }
}

prepare_heterogeneity_data <- function(processed_data_with_groups, groups_list, var, group) {
  # Prepare data for heterogeneity analysis
  df_var <- processed_data_with_groups %>% 
    filter(Variable == var) %>%
    left_join(groups_list %>% 
                select(PlastisphereBigGroup = PlastisphereGroup, Group = !!group), 
              by = "PlastisphereBigGroup") %>% 
    filter(!is.na(Group))   
  return(df_var)
}