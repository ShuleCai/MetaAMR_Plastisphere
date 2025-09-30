# Configuration for file paths and directory creation
# Set base paths and create necessary directories

library(tidyverse)

# Set base directory (replace with your actual base path)
base_path <- "/path/to/your/project/"
folder <- "Total_Abun_var_env_ARG/"

# Create output directories
dir.create(paste0(base_path, folder), showWarnings = FALSE)
dir.create(paste0(base_path, folder, "heterogeneity"), showWarnings = FALSE)
dir.create(paste0(base_path, folder, "funnels"), showWarnings = FALSE)
dir.create(paste0(base_path, "MAG_90_5/"), showWarnings = FALSE)
dir.create(paste0(base_path, "figures/"), showWarnings = FALSE)  

# Export paths for use in other scripts
export_paths <- function() {
  list(
    base_path = base_path,
    folder = folder,
    output_dir = paste0(base_path, folder),
    heterogeneity_dir = paste0(base_path, folder, "heterogeneity"),
    funnel_dir = paste0(base_path, folder, "funnels"),
    aldex2_dir = paste0(base_path, "MAG_90_5/"),
    figures_dir = paste0(base_path, "figures/")  
  )
}