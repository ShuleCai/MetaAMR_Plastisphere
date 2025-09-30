# MAG (Metagenome-Assembled Genome) statistical analysis

library(dplyr)
library(tidyverse)

load_mag_stats <- function(stats_path) {
  # Load MAG phylogenetic tree and statistical data
  cat("Loading MAG statistics from:", stats_path, "\n")
  
  mag_stats <- read.csv(stats_path)
  
  # Print basic summary
  cat("MAG statistics dataset dimensions:", dim(mag_stats), "\n")
  cat("Number of significant MAGs:", sum(mag_stats$significance, na.rm = TRUE), "\n")
  
  return(mag_stats)
}

preprocess_mag_data <- function(mag_stats) {
  # Preprocess MAG data for analysis
  
  processed_data <- mag_stats %>%
    mutate(
      # Create grouping based on effect direction and significance
      group = case_when(
        effect > 0 & pvalue < 0.05 ~ "plas",
        effect < 0 & pvalue < 0.05 ~ "env",
        TRUE ~ "ns"
      ),
      # Create binary indicator for triple presence (ARG, MGE, VFG)
      triple_presence = arg_cnt > 0 & mge_cnt > 0 & vfg_cnt > 0,
      # Convert genome size to megabases
      Genome_Size = Genome_Size / 1e6
    )
  
  # Calculate and print summary statistics
  print_mag_summary_stats(processed_data)
  
  return(processed_data)
}

print_mag_summary_stats <- function(mag_data) {
  # Print summary statistics for MAG data
  
  plas_data <- mag_data %>% filter(group == "plas")
  env_data <- mag_data %>% filter(group == "env")
  
  cat("MAG Analysis Summary:\n")
  cat("Total MAGs:", nrow(mag_data), "\n")
  cat("Plastisphere-enriched MAGs:", nrow(plas_data), "\n")
  cat("Environment-enriched MAGs:", nrow(env_data), "\n")
  cat("Non-significant MAGs:", nrow(mag_data) - nrow(plas_data) - nrow(env_data), "\n")
  
  cat("\nTriple Presence Statistics:\n")
  cat("Plastisphere-enriched MAGs with ARG+MGE+VFG:", 
      sum(plas_data$triple_presence), "/", nrow(plas_data), 
      "(", round(mean(plas_data$triple_presence) * 100, 1), "%)\n")
  
  cat("Environment-enriched MAGs with ARG+MGE+VFG:", 
      sum(env_data$triple_presence), "/", nrow(env_data), 
      "(", round(mean(env_data$triple_presence) * 100, 1), "%)\n")
}

perform_mag_statistical_tests <- function(mag_data) {
  # Perform statistical tests on MAG characteristics
  
  cat("Performing statistical tests on MAG characteristics...\n")
  
  # Filter data for comparison
  comparison_data <- mag_data %>% filter(group %in% c("plas", "env"))
  
  # Define metrics to test
  metrics <- c("Genome_Size", "GC", "arg_cnt", "mge_cnt", "vfg_cnt")
  
  results <- list()
  
  for (metric in metrics) {
    # Perform t-test
    t_test <- t.test(comparison_data[[metric]] ~ comparison_data$group)
    
    # Calculate effect size (Cohen's d)
    plas_values <- comparison_data %>% filter(group == "plas") %>% pull(metric)
    env_values <- comparison_data %>% filter(group == "env") %>% pull(metric)
    
    cohens_d <- (mean(plas_values, na.rm = TRUE) - mean(env_values, na.rm = TRUE)) / 
      sqrt((sd(plas_values, na.rm = TRUE)^2 + sd(env_values, na.rm = TRUE)^2) / 2)
    
    results[[metric]] <- list(
      metric = metric,
      t_statistic = t_test$statistic,
      p_value = t_test$p.value,
      cohens_d = cohens_d,
      plas_mean = mean(plas_values, na.rm = TRUE),
      env_mean = mean(env_values, na.rm = TRUE),
      plas_sd = sd(plas_values, na.rm = TRUE),
      env_sd = sd(env_values, na.rm = TRUE)
    )
  }
  
  # Convert results to data frame
  results_df <- bind_rows(lapply(results, as.data.frame))
  
  cat("Statistical tests completed.\n")
  
  return(results_df)
}

perform_mag_analysis <- function(stats_path, output_path) {
  # Main function to perform complete MAG analysis
  
  cat("Starting MAG statistical analysis...\n")
  
  # Load and preprocess data
  mag_stats <- load_mag_stats(stats_path)
  mag_processed <- preprocess_mag_data(mag_stats)
  
  # Create output directory if it doesn't exist
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # Perform statistical tests
  statistical_results <- perform_mag_statistical_tests(mag_processed)
  
  # Save statistical results
  write.csv(statistical_results, 
            file = paste0(output_path, "MAG_statistical_tests.csv"), 
            row.names = FALSE)
  
  # Save processed data
  write.csv(mag_processed, 
            file = paste0(output_path, "MAG_processed_data.csv"), 
            row.names = FALSE)
  
  # Print final summary
  print_final_mag_summary(mag_processed, statistical_results)
  
  cat("MAG statistical analysis complete! Results saved in:", output_path, "\n")
  
  return(list(
    processed_data = mag_processed,
    statistical_results = statistical_results
  ))
}

print_final_mag_summary <- function(mag_data, statistical_results) {
  # Print final summary of MAG analysis
  
  cat("\n=== MAG Analysis Final Summary ===\n")
  
  group_counts <- table(mag_data$group)
  cat("Group distribution:\n")
  for (group_name in names(group_counts)) {
    cat("  ", group_name, ":", group_counts[group_name], "MAGs\n")
  }
  
  # Print statistical test results
  cat("\nStatistical Test Results:\n")
  for (i in 1:nrow(statistical_results)) {
    result <- statistical_results[i, ]
    cat("  ", result$metric, ": t =", round(result$t_statistic, 3), 
        ", p =", format.pval(result$p_value, digits = 3), 
        ", d =", round(result$cohens_d, 3), "\n")
  }
}