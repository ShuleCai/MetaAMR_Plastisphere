# MetaAMR_Plastisphere

## Overview
This repository provides a reproducible workflow for visualizing antimicrobial resistance (AMR) gene in aquatic plastisphere. The resources include R scripts for generating figures, pre-processed input tables, and final graphical outputs. To reproduce a figure, simply run the `scripts/Fig*.R` script, which will process the `data/Fig*.csv` file and save the plot as `figures/Fig*.pdf`.

## Directory structure
```
MetaAMR_Plastisphere/
├── README.md            # Project document
├── data/                # pre-processed input tables
│   ├── Fig1a_effect.csv
│   ├── Fig1a_stats.csv
│   ├── Fig1b_effect.csv
│   ├── Fig1b_stats.csv
│   ├── Fig1c_effect.csv
│   ├── Fig1c_stats.csv
│   ├── Fig1d_effect.csv
│   ├── Fig1d_stats.csv
│   ├── ...
├── figures/             # graphical outputs
│   ├── Fig1a.pdf
│   ├── Fig1b.pdf
│   ├── Fig1c.pdf
│   ├── Fig1d.pdf
│   ├── ...
└── scripts/             # R scripts for generating figures
    ├── Fig1a.R
    ├── Fig1b.R
    ├── Fig1c.R
    ├── Fig1d.R
    ├── ...
```

## Requirements
* R (version 4.0 or higher recommended).
* Packages used across scripts: `ComplexHeatmap`, `RColorBrewer`, `circlize`, `dplyr`, `ggplot2`, `ggpubr`, `ggraph`, `grid`, `igraph`, `patchwork`, `tidygraph`, `tidyverse`.


## Instruction of scripts
### Figure 1
- `scripts/Fig1a.R` reads `data/Fig1a_effect.csv`, `data/Fig1a_stats.csv` → `figures/Fig1a.pdf`  
  [**Figure 1a**](./figures/Fig1a.pdf) shows the abundance of antibiotic resistance genes (ARGs) in the plastisphere versus surrounding water.
- `scripts/Fig1b.R` reads `data/Fig1b_effect.csv`, `data/Fig1b_stats.csv` → `figures/Fig1b.pdf`  
  [**Figure 1b**](./figures/Fig1b.pdf) shows the abundance of ARG-carrying pathogens (APs) in the plastisphere versus surrounding water.
- `scripts/Fig1c.R` reads `data/Fig1c_effect.csv`, `data/Fig1c_stats.csv` → `figures/Fig1c.pdf`  
  [**Figure 1c**](./figures/Fig1c.pdf) shows the abundance of ARGs in the plastisphere versus natural biofilms.
- `scripts/Fig1d.R` reads `data/Fig1d_effect.csv`, `data/Fig1d_stats.csv` → `figures/Fig1d.pdf`  
  [**Figure 1d**](./figures/Fig1d.pdf) shows the abundance of APs in the plastisphere versus natural biofilms.
### Figure 2
- `scripts/Fig2a.R` reads `data/Fig2a.csv` → `figures/Fig2a.pdf`  
  [**Figure 2a**](./figures/Fig2a.pdf) shows comparisons of the relative selection of ARG-carrying contigs (ACCs) and ARG-MGE-carrying contigs (AMCCs; MGE, mobile genetic elements) between the plastisphere and the surrounding environment across all samples, freshwater systems, and marine systems.
- `scripts/Fig2b.R` reads `data/Fig2b.csv` → `figures/Fig2b.pdf`  
  [**Figure 2b**](./figures/Fig2b.pdf) shows selection of MGE categories between plastisphere and surrounding environment across all samples, freshwater systems, and marine systems.
### Figure 3
- `scripts/Fig3a.R` reads `data/Fig3a.csv` → `figures/Fig3a.pdf`  
  [**Figure 3a**](./figures/Fig3a.pdf) shows effect sizes of AP selection in the plastisphere across all samples, and freshwater and marine systems only.
- `scripts/Fig3b.R` reads `data/Fig3b1.csv`, `data/Fig3b2.csv` → `figures/Fig3b.pdf`  
  [**Figure 3b**](./figures/Fig3b.pdf) shows relative abundance of major APs in marine system, and asterisks (*) denote significant selection in the plastisphere compared to the surrounding environment (P < 0.05).
- `scripts/Fig3c.R` reads `data/Fig3c1.csv`, `data/Fig3c2.csv` → `figures/Fig3c.pdf`  
  [**Figure 3c**](./figures/Fig3c.pdf) shows relative abundance of major APs in freshwater system, and asterisks (*) denote significant selection in the plastisphere compared to the surrounding environment (P < 0.05).
### Figure 4
- `scripts/Fig4.R` reads `data/Fig4_nodes.csv`, `data/Fig4_edges.csv` → `figures/Fig4.pdf`  
  [**Figure 4**](./figures/Fig4.pdf) shows the phylum-level horizontal gene transfer (HGT) network for different categories of ARGs among metagenome-assembled genomes (MAGs) in the plastisphere, natural biofilm, and surrounding water.
### Figure 5
> The metagenomic and metatranscriptomic data originate from two independent in-situ incubation experiments conducted in this study. The marine and freshwater experiments were performed in the subtropical coastal waters near Xiamen, Fujian (118°11'E, 24°57'N) and in the Dasha River, Shenzhen (113°53'E, 22°34'N), respectively.
- `scripts/Fig5a.R` reads `data/Fig5a1.csv`, `data/Fig5a2.csv` → `figures/Fig5a.pdf`  
  [**Figure 5a**](./figures/Fig5a.pdf) shows ARG abundance and transcriptional activity in freshwater and marine plastisphere samples, with bubble size representing abundance and color intensity indicating transcriptional activity.
- `scripts/Fig5b.R` reads `data/Fig5b.csv` → `figures/Fig5b.pdf`  
  [**Figure 5b**](./figures/Fig5b.pdf) shows the natural logarithm of the ratio (plastisphere/surrounding water) of the average metagenomic abundance for different categories of ARGs in both freshwater and marine systems.
- `scripts/Fig5c.R` reads `data/Fig5c.csv` → `figures/Fig5c.pdf`  
  [**Figure 5c**](./figures/Fig5c.pdf) shows the natural logarithm of the ratio (plastisphere/surrounding water) of the average metatranscriptomic abundance for different categories of ARGs in both freshwater and marine systems.
### Figure 6
- `scripts/Fig6b.R` reads `data/Fig6b.csv` → `figures/Fig6b.pdf`  
  [**Figure 6b**](./figures/Fig6b.pdf) shows the traits of MAGs regarding genome size for those enriched in the plastisphere (orange) versus those that are not (green).
- `scripts/Fig6c.R` reads `data/Fig6c.csv` → `figures/Fig6c.pdf`  
  [**Figure 6c**](./figures/Fig6c.pdf) shows the traits of MAGs regarding GC content for those enriched in the plastisphere (orange) versus those that are not (green).
- `scripts/Fig6d.R` uses inline data (no CSV input) → `figures/Fig6d.pdf`  
  [**Figure 6d**](./figures/Fig6d.pdf) shows the proportion of MAGs selected in the plastisphere compared to those that are not.
### Figure 7
- `scripts/Fig7a.R` reads `data/Fig7a.csv` → `figures/Fig7a.pdf`  
  [**Figure 7a**](./figures/Fig7a.pdf) shows shifts in bacterial life history traits in freshwater plastisphere.
- `scripts/Fig7b.R` reads `data/Fig7b.csv` → `figures/Fig7b.pdf`  
  [**Figure 7b**](./figures/Fig7b.pdf) shows shifts in bacterial life history traits in marine plastisphere.

## Usage
1. Clone the repository:
``` 
git clone https://github.com/ShuleCai/MetaAMR_Plastisphere.git
```
2. Run any script in `scripts/` to regenerate its figure; missing packages are installed automatically.
3. Outputs are written to `figures/` in PDF format.

## Citation
If you use this code for your research, please cite us:
> Shu-Le Li, et al. XXX
