# Main execution script for meta-analysis project
# This script orchestrates the entire analysis pipeline

source("0_config/0_1_paths.R")
source("0_config/0_2_constants.R")
source("1_data/1_1_data_loading.R")
source("1_data/1_2_data_processing.R")
source("2_analysis/2_1_meta_analysis.R")
source("2_analysis/2_2_descriptive_stats.R")
source("2_analysis/2_3_heterogeneity.R")
source("2_analysis/2_4_aldex2_analysis.R")
source("2_analysis/2_5_MAG_statistics.R")       
source("3_visualization/3_1_forest_plots.R")
source("3_visualization/3_2_funnel_plots.R")
source("3_visualization/3_3_MAG_visualization.R") 

# Initialize paths and constants
paths <- export_paths()
constants <- export_constants()

# Load data
cat("Loading data...\n")
pair_data <- load_pair_data()
measurements <- load_measurement_data()
groups_list <- load_group_info()

# Process data
cat("Processing data...\n")
group_mapping <- create_group_mapping(pair_data)
plastisphere_data <- process_plastisphere_data(pair_data, measurements)
control_data <- process_control_data(pair_data, measurements)
meta_data <- merge_meta_data(plastisphere_data, control_data, group_mapping)
processed_data <- calculate_effect_sizes(meta_data)

# Merge with group information
cat("Merging with group information...\n")
processed_data_with_groups <- merge(processed_data, groups_list, 
                                   by.x = "PlastisphereBigGroup", 
                                   by.y = "PlastisphereGroup", 
                                   all.x = TRUE) %>% 
  filter(!grepl("^AZ", PlastisphereBigGroup))

# Save processed data
write.csv(processed_data_with_groups, 
          paste0(paths$base_path, paths$folder, "effect_groups_ckenvironment.csv"), 
          row.names = FALSE)

# Perform ALDEx2 analysis on group means
cat("Performing ALDEx2 analysis...\n")
aldex2_output_path <- paths$aldex2_dir
aldex_results <- perform_aldex_analysis(meta_data, pair_data, aldex2_output_path)

# Perform MAG statistical analysis
cat("Performing MAG statistical analysis...\n")
mag_stats_path <- "/path/to/your/MAG/phylogenetic_tree_data.csv"  # Replace with actual path
mag_analysis_results <- perform_mag_analysis(mag_stats_path, paths$figures_dir)

# Perform MAG visualization
cat("Creating MAG visualizations...\n")
mag_violin_plots <- create_mag_violin_plots(mag_analysis_results$processed_data, paths$figures_dir)
mag_pie_chart <- create_mag_triple_presence_pie_chart(mag_analysis_results$processed_data, paths$figures_dir)

# Perform meta-analysis
cat("Performing meta-analysis...\n")
meta_results <- perform_meta_analysis(processed_data_with_groups, constants$group_vars, 
                                     paths$base_path, paths$folder)

# Calculate descriptive statistics
cat("Calculating descriptive statistics...\n")
descriptive_stats <- calculate_descriptive_stats(pair_data, measurements, groups_list, 
                                                constants$group_vars, paths$base_path, paths$folder)

# Generate forest plots
cat("Generating forest plots...\n")
generate_forest_plots(meta_results, descriptive_stats, paths$base_path, paths$folder, constants$plot_colors)

# Analyze heterogeneity
cat("Analyzing heterogeneity...\n")
analyze_heterogeneity(processed_data_with_groups, groups_list, constants$group_vars, 
                     paths$base_path, paths$folder)

# Generate funnel plots
cat("Generating funnel plots...\n")
generate_funnel_plots(processed_data, paths$base_path, paths$folder)

cat("Analysis complete!\n")
cat("Results saved in:", paths$output_dir, "\n")
cat("ALDEx2 results saved in:", paths$aldex2_dir, "\n")
cat("MAG analysis results saved in:", paths$figures_dir, "\n")
