

# MetaAMR_Plastisphere

## Overview
MetaAMR_Plastisphere is an code repository for the analysis and visualization of antimicrobial resistance (AMR) in aquatic plastisphere. This repository provides scripts for data loading, processing, meta-analysis, statistical evaluation, and graphical representation.

## Directory Structure
```
MetaAMR_Plastisphere/
├── main.R                  # Main entry script
├── README.md               # Project documentation
├── 0_config/               # Configuration files
│   ├── 0_1_path.R          # Path settings
│   └── 0_2_constants.R     # Constants
├── 1_data/                 # Data processing
│   ├── 1_1_data_loading.R  # Data loading
│   └── 1_2_data_processing.R # Data preprocessing
├── 2_analysis/             # Data analysis
│   ├── 2_1_meta_analysis.R     # Meta-analysis
│   ├── 2_2_descriptive_stats.R # Descriptive statistics
│   ├── 2_3_heterogeneity.R     # Heterogeneity analysis
│   ├── 2_4_aldex2_analysis.R   # ALDEx2 differential analysis
│   └── 2_5_MAG_analysis.R      # MAG (Metagenome-Assembled Genomes) analysis
├── 3_visualization/        # Visualization
│   ├── 3_1_forest_plots.R      # Forest plots
│   ├── 3_2_funnel_plots.R      # Funnel plots
│   └── 3_3_MAG_voilin_plots.R  # MAG violin plots
```

## Features
- Data loading and preprocessing
- Meta-analysis and descriptive statistics
- Heterogeneity assessment
- ALDEx2 differential abundance analysis
- MAG (Metagenome-Assembled Genomes) analysis
- Multiple visualizations: forest plots, funnel plots, violin plots

## Requirements
- R (version 4.0 or higher recommended)
- R packages: see the header of each script for required packages (e.g., tidyverse, meta, ALDEx2, etc.)

## Usage
1. Clone the repository:
	```bash
	git clone https://github.com/ShuleCai/MetaAMR_Plastisphere.git
	```
2. Install R and required packages. Refer to each script for package installation instructions.
3. Run `main.R` or execute individual module scripts in order.
4. Output results and figures will be saved to designated folders.

## Citation
If you use this code for your research, please cite our paper:

> Shu-Le Li, et al. XXX

