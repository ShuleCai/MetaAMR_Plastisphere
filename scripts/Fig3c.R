pkgs <- c("ComplexHeatmap", "RColorBrewer")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

freshwater_data <- read.csv("./data/Fig3c1.csv", row.names = 1)
AP_freshwater_df_s_sort <- read.csv("./data/Fig3c2.csv", row.names = 1)

# Freshwater heatmap
p <- ComplexHeatmap::pheatmap(
  mat = log10(t(freshwater_data)),
  name = "Average\nabundance (tpm)",
  scale = "none",
  na_col = "#dddddd",
  cluster_cols = F,
  cluster_rows = F,
  fontsize_col = 7.62,
  fontsize_row = 7.62,
  angle_col = "45",
  row_names_side = "left",
  column_names_side = "bottom",
  cellwidth = 12,
  cellheight = 12,
  color = colorRampPalette(brewer.pal(9, "Blues")[3:9], bias = 1)(256),
  display_numbers = F,
  number_format = "%.0f",
  border_color = "grey30",
  cell_fun = function(j, i, x, y, width, height, fill) {
    if (t(AP_freshwater_df_s_sort)[i, j] == 1)
      grid.text("*", x, y, gp = gpar(fontsize = 9 * 0.9))
  }
)

pdf("./figures/Fig3c.pdf", family = "ArialMT", width = 10)
p
dev.off()
