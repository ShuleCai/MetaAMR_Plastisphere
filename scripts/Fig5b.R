pkgs <- c("ComplexHeatmap", "circlize", "grid")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

df_metag <- read.csv("./data/Fig5b.csv", row.names = 1)

process_data <- function(df) {
  mat <- t(as.matrix(df))
  mat_log <- log10(mat)
  mat_log[is.infinite(mat_log)] <- NA
  mat_log
}

mat_metag <- process_data(df_metag)
max_val <- max(mat_metag) # 2.005444

puor_colors <- c("#B35806", "#E08214", "#FDB863", "#FEE0B6",
                 "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC",
                 "#542788")

# Dark purple for Inf
inf_color <- "#4e2983"

# Heatmap 1: Metagenome
ht1 <- ComplexHeatmap::Heatmap(mat_metag,
               name = "ln(Ratio)",
               col = circlize::colorRamp2(
                 c(-max_val,  -max_val/4, 0, max_val/4, max_val), 
                 c(puor_colors[1], puor_colors[2], puor_colors[5], puor_colors[8], puor_colors[9])
               ),
               na_col = inf_color,
               
               # Layout settings
               cluster_rows = FALSE,
               cluster_columns = FALSE,
               rect_gp = grid::gpar(col = "black", lwd = 1), # Cell borders
               
               # Titles and labels
               row_title = "MetaG",
               row_names_side = "left",
               column_title = NULL,
               column_names_side = "top",
               
               column_names_rot = 45,
               show_column_names = TRUE
)

pdf("./figures/Fig5b.pdf", family = "ArialMT", height = 2.5, width = 6)
  ht1
dev.off()

