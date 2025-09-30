# ALDEx2 differential abundance analysis functions

library(ALDEx2)
library(tidyverse)

perform_aldex_analysis <- function(meta_data, pair_data, output_path) {
  # Perform ALDEx2 differential abundance analysis
  
  # Prepare sample lists for plastisphere and control groups
  sample_lists <- prepare_sample_lists(pair_data)
  
  # Prepare group mean matrices for ALDEx2
  group_matrices <- prepare_group_matrices(meta_data)
  
  # Check matrix consistency
  if (!identical(rownames(group_matrices$plas), rownames(group_matrices$control))) {
    stop("Row names of plastisphere and control matrices do not match")
  }
  
  # Merge matrices for ALDEx2 input
  merged_matrix <- cbind(group_matrices$plas, group_matrices$control)
  
  # Define conditions for ALDEx2
  conditions <- c(rep("plas", ncol(group_matrices$plas)), 
                  rep("ck", ncol(group_matrices$control)))
  
  # Perform ALDEx2 analysis
  aldex_results <- run_aldex_analysis(merged_matrix, conditions, output_path)
  
  return(aldex_results)
}

prepare_sample_lists <- function(pair_data) {
  # Prepare sample lists for plastisphere and control groups
  sample_list_plas <- pair_data$PlastisphereID %>% 
    unique() %>% 
    data.frame() %>% 
    rename(Sample = ".") 
  
  sample_list_control <- pair_data$CKenvironmentID %>% 
    unique() %>% 
    data.frame() %>% 
    rename(Sample = ".") 
  
  # Create group information data frame
  group_list <- data.frame(
    sample = c(sample_list_plas$Sample, sample_list_control$Sample),
    group = c(rep("plastisphere", nrow(sample_list_plas)), 
              rep("ckenvironment", nrow(sample_list_control)))
  )
  
  return(list(
    plas = sample_list_plas,
    control = sample_list_control,
    group_list = group_list
  ))
}

prepare_group_matrices <- function(meta_data) {
  # Prepare group mean matrices for ALDEx2 analysis
  
  # Create plastisphere matrix
  group_mean_plas <- meta_data %>% 
    mutate(id = paste0(Study, "P")) %>% 
    select(id, Variable, Mean_t) %>% 
    pivot_wider(id_cols = "Variable", names_from = "id", values_from = "Mean_t") %>% 
    tibble::column_to_rownames("Variable")
  
  # Create control environment matrix
  group_mean_control <- meta_data %>% 
    mutate(id = paste0(Study, "E")) %>% 
    select(id, Variable, Mean_c) %>% 
    pivot_wider(id_cols = "Variable", names_from = "id", values_from = "Mean_c") %>% 
    tibble::column_to_rownames("Variable")
  
  return(list(
    plas = group_mean_plas,
    control = group_mean_control
  ))
}

run_aldex_analysis <- function(merged_matrix, conditions, output_path) {
  # Run ALDEx2 analysis with specified parameters
  
  cat("Running ALDEx2 analysis...\n")
  cat("Matrix dimensions:", dim(merged_matrix), "\n")
  cat("Conditions:", table(conditions), "\n")
  
  # Perform ALDEx2 analysis for effect size testing
  aldex_results_effect <- ALDEx2::aldex(
    round(merged_matrix),           # Round counts for ALDEx2
    conditions = conditions,        # Group conditions
    mc.samples = 128,              # Number of Monte Carlo samples
    test = "t",                    # Statistical test
    denom = "all",                 # Denominator for CLR transformation
    effect = TRUE,                 # Calculate effect sizes
    paired.test = TRUE,            # Use paired test design
    include.sample.summary = FALSE # Exclude sample summary
  )
  
  # Perform ALDEx2 analysis to get CLR values
  aldex_results_clr <- ALDEx2::aldex(
    round(merged_matrix),           # Round counts for ALDEx2
    conditions = conditions,        # Group conditions
    mc.samples = 128,              # Number of Monte Carlo samples
    test = "t",                    # Statistical test
    denom = "all",                 # Denominator for CLR transformation
    effect = TRUE,                 # Calculate effect sizes
    paired.test = TRUE,            # Use paired test design
    include.sample.summary = TRUE  # Include sample summary for CLR values
  )
  
  # Extract and process CLR values
  clr_data <- process_clr_values(aldex_results_clr)
  
  # Save results
  write.csv(aldex_results_effect, 
            file = paste0(output_path, "aldex2_effect_test.csv"), 
            row.names = TRUE)
  
  write.csv(clr_data$clr_df, 
            file = paste0(output_path, "aldex2_clr_df.csv"), 
            row.names = TRUE)
  
  write.csv(clr_data$E_clr, 
            file = paste0(output_path, "aldex2_E_clr_df.csv"), 
            row.names = TRUE)
  
  # Print summary of significant results
  print_significant_results(aldex_results_effect)
  
  return(list(
    effect_results = aldex_results_effect,
    clr_data = clr_data
  ))
}

process_clr_values <- function(aldex_results) {
  # Process CLR values from ALDEx2 results
  
  # Extract CLR values (rab.sample columns)
  clr_columns <- grep("rab.sample", colnames(aldex_results), value = TRUE)
  clr_df <- aldex_results[, clr_columns] %>% t()
  
  # Clean row names
  rownames(clr_df) <- gsub("rab.sample.", "", rownames(clr_df))
  
  # Convert CLR to exponential scale and recalculate CLR
  exp_matrix <- apply(clr_df, 1, function(x) 2^x)
  E_clr <- t(apply(exp_matrix, 2, function(x) log2(x) - mean(log2(x)))) %>% 
    as.data.frame()
  
  return(list(
    clr_df = as.data.frame(clr_df),
    E_clr = E_clr
  ))
}

print_significant_results <- function(aldex_results) {
  # Print summary of significant differential abundance results
  
  significant_up <- aldex_results %>% 
    filter(wi.eBH < 0.05 & effect > 0)
  
  significant_down <- aldex_results %>% 
    filter(wi.eBH < 0.05 & effect < 0)
  
  cat("ALDEx2 Results Summary:\n")
  cat("Total features:", nrow(aldex_results), "\n")
  cat("Significantly increased in plastisphere:", nrow(significant_up), "\n")
  cat("Significantly decreased in plastisphere:", nrow(significant_down), "\n")
  cat("FDR threshold: 0.05\n")
  
  if (nrow(significant_up) > 0) {
    cat("\nTop increased features:\n")
    print(head(significant_up[order(-significant_up$effect), ], 10))
  }
  
  if (nrow(significant_down) > 0) {
    cat("\nTop decreased features:\n")
    print(head(significant_down[order(significant_down$effect), ], 10))
  }
}

load_mag_abundance_data <- function(abundance_path) {
  # Load MAG abundance data for ALDEx2 analysis
  
  cat("Loading MAG abundance data from:", abundance_path, "\n")
  
  abun_df <- read.csv(abundance_path) %>% 
    tibble::column_to_rownames("X")
  
  cat("MAG abundance matrix dimensions:", dim(abun_df), "\n")
  cat("Sample names:", names(abun_df)[1:5], "...\n") # Show first 5 samples
  
  return(abun_df)
}

validate_sample_alignment <- function(abun_df, group_list) {
  # Validate that sample names in abundance data match group list
  
  samples_in_abundance <- names(abun_df)
  samples_in_groups <- group_list$sample
  
  # Check for exact match
  exact_match <- identical(sort(samples_in_abundance), sort(samples_in_groups))
  
  if (exact_match) {
    cat("Sample names in abundance data and group list match exactly.\n")
    return(TRUE)
  } else {
    cat("Sample name mismatch detected:\n")
    cat("Samples in abundance data but not in group list:", 
        setdiff(samples_in_abundance, samples_in_groups), "\n")
    cat("Samples in group list but not in abundance data:", 
        setdiff(samples_in_groups, samples_in_abundance), "\n")
    return(FALSE)
  }
}