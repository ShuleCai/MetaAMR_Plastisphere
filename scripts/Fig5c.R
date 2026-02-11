pkgs <- c("ComplexHeatmap", "circlize", "grid")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

df_metat <- read.csv("./data/Fig5c.csv", row.names = 1)

process_data <- function(df) {
  mat <- t(as.matrix(df))
  mat_log <- log10(mat)
  mat_log[is.infinite(mat_log)] <- NA
  mat_log
}

mat_metat <- process_data(df_metat)
max_val <- 2.005444 # max value of metag from Fig5b.R

puor_colors <- c("#B35806", "#E08214", "#FDB863", "#FEE0B6",
                 "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC",
                 "#542788")

col_fun <- circlize::colorRamp2(
  c(-max_val,  -max_val/4, 0, max_val/4, max_val), 
  c(puor_colors[1], puor_colors[2], puor_colors[5], puor_colors[8], puor_colors[9])
)

# Deep purple for Inf
inf_color <- "#4e2983"

# Heatmap 2: Metatranscriptome
ht2 <- ComplexHeatmap::Heatmap(mat_metat,
               name = "ln(Ratio)",
               col = col_fun,
               na_col = inf_color,
               
               # Layout settings
               cluster_rows = FALSE,
               cluster_columns = FALSE,
               rect_gp = grid::gpar(col = "black", lwd = 1), # Cell borders
               
               # Titles and labels
               row_title = "MetaT",
               row_names_side = "left",
               column_names_side = "top",
               
               column_names_rot = 45,
               show_column_names = TRUE
)

pdf("./figures/Fig5c.pdf", family = "ArialMT", height = 2.5, width = 6)
ht2
dev.off()
