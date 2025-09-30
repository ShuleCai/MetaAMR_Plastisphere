# Define constants and grouping variables for the analysis

# Grouping variables for subgroup analysis
group_vars <- c("Ecosystem", "ResearchMethod", 
                "PlasticType", "Degradability", "PlasticSize",
                "Water.Temp.Class", "pH.Class", "DO.Class", "Depth.Class")

# Color scheme for plots
plot_colors <- c(
  "Plastisphere" = "#FFCC00",
  "environment" = "#56B4E9",
  "plastisphere" = "#FFCC00",
  "ckenvironment" = "#56B4E9",
  "none" = "white"
)

# Export constants
export_constants <- function() {
  list(
    group_vars = group_vars,
    plot_colors = plot_colors
  )
}