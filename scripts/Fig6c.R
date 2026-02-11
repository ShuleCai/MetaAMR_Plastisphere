pkgs <- c("ggplot2", "ggpubr", "dplyr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

data_df <- read.csv("./data/Fig6c.csv")
plot_data <- data_df %>% 
  mutate(group = factor(group, levels = c("plas", "env"))) 

label_y <- max(plot_data[["GC"]]) * 0.98

# Violin plot
ggplot(plot_data, aes(x = group, y = .data[["GC"]], fill = group)) +
  geom_violin(scale = "width", width = 0.7) +
  geom_boxplot(width = 0.1, fill = "white") +  # Boxplot inside the violin
  stat_compare_means(label = "p.signif", method = "t.test", 
                     label.x = 1.5, label.y = label_y,
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, Inf),
                                        symbols = c("***", "**", "*", "ns"))) +
  labs(title = NULL,
       x = "Group",
       y = "GC content (%)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set2", direction = -1)

ggsave("./figures/Fig6c.pdf", width = 6, height = 6)
