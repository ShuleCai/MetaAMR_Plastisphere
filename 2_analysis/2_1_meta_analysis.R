
# Main meta-analysis functions
library(tidyverse)
library(metafor)

perform_meta_analysis <- function(processed_data_with_groups, group_vars, base_path, folder) {
  # Perform meta-analysis for all variables and subgroups
  results_all <- list()
  
  for (var_name in unique(processed_data_with_groups$Variable)) {
    cat("Processing variable:", var_name, "\n")
    
    # Filter data for current variable
    df_var <- processed_data_with_groups %>%
      filter(Variable == var_name) 
    
    # Initialize results table
    diff_mean <- data.frame()
    seq_counter <- 1
    
    # Calculate overall effect
    overall_model <- rma(yi = lnRR, vi = v, data = df_var, method = "DL")
    diff_mean <- add_result_row(diff_mean, "Total", overall_model, seq_counter)
    seq_counter <- seq_counter + 1
    
    # Add spacer row
    diff_mean <- add_spacer_row(diff_mean, "A", seq_counter)
    seq_counter <- seq_counter + 1
    
    LETTER_ID <- 2
    # Perform subgroup analysis for each grouping variable
    for (group_var in group_vars) {
      if (group_var %in% names(df_var)) {
        diff_mean <- analyze_subgroups(df_var, group_var, diff_mean, seq_counter)
        seq_counter <- nrow(diff_mean) + 1
        
        # Add spacer between grouping variables (except the last one)
        if (group_var != group_vars[length(group_vars)]) {
          diff_mean <- add_spacer_row(diff_mean, LETTERS[LETTER_ID], seq_counter)
          seq_counter <- seq_counter + 1
          LETTER_ID <- LETTER_ID + 1
        }
      }
    }
    
    # Store results for current variable
    results_all[[var_name]] <- diff_mean %>% arrange(sequence)
    
    # Save individual results
    write.csv(results_all[[var_name]], 
              file = paste0(base_path, folder, "meta_effect_ckenvironment_", var_name, ".csv"), 
              row.names = FALSE)
  }
  
  return(results_all)
}

add_result_row <- function(results_df, level_name, model, sequence) {
  # Add a result row to the results data frame
  group_category <- ifelse(model$ci.lb > 0 & model$ci.ub > 0, "plastisphere", 
                          ifelse(model$ci.lb < 0 & model$ci.ub < 0, "ckenvironment", "none"))
  
  new_row <- data.frame(
    var = level_name,
    estimate = model$b[1],
    conf.low = model$ci.lb,
    conf.high = model$ci.ub,
    p.value = model$pval,
    sequence = sequence,
    Group = group_category
  )
  
  return(rbind(results_df, new_row))
}

add_spacer_row <- function(results_df, spacer_label, sequence) {
  # Add a spacer row for visualization purposes
  spacer_row <- data.frame(
    var = spacer_label,
    estimate = 0,
    conf.low = 0,
    conf.high = 0,
    p.value = 1,
    sequence = sequence,
    Group = "b"
  )
  
  return(rbind(results_df, spacer_row))
}

analyze_subgroups <- function(df_var, group_var, results_df, start_seq) {
  # Analyze subgroups for a given grouping variable
  seq_counter <- start_seq
  
  # Get group levels, excluding forbidden categories
  if (is.factor(df_var[[group_var]])) {
    group_levels <- levels(df_var[[group_var]]) %>% setdiff(c("Forbidden", "Lab"))
  } else {
    group_levels <- df_var %>% 
      filter(!is.na(.data[[group_var]])) %>% 
      distinct(.data[[group_var]]) %>% 
      pull() %>% setdiff(c("Forbidden", "Lab"))
  }
  
  # Analyze each subgroup level
  for (level in group_levels) {
    df_level <- df_var %>% filter(.data[[group_var]] == level)
    
    # Skip if insufficient data
    if (nrow(df_level) < 3) next
    
    tryCatch({
      level_model <- rma(yi = lnRR, vi = v, data = df_level, method = "DL")
      results_df <- add_result_row(results_df, level, level_model, seq_counter)
      seq_counter <- seq_counter + 1
    }, error = function(e) {
      message(paste("Error in", group_var, level, ":", e$message))
    })
  }
  
  return(results_df)
}