pkgs <- c("tidyverse", "patchwork")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

effect_df_1d <- read.csv("./data/Fig1d_effect.csv") %>%
  arrange(sequence)
stats_df_1d <- read.csv("./data/Fig1d_stats.csv") %>%
  mutate(var = factor(var, levels = rev(effect_df_1d$var)))

# Left panel: mean values with SE
bar_plot_1d <- ggplot(stats_df_1d, aes(var, Mean, fill = Treatment)) +
  annotate(
    "rect",
    xmin = seq(0.5, n_distinct(stats_df_1d$var) - 0.5, by = 2),
    xmax = seq(1.5, n_distinct(stats_df_1d$var) + 0.5, by = 2),
    ymin = -Inf, ymax = Inf,
    fill = "gray95", alpha = 0.5
  ) +
  geom_errorbar(
    aes(ymin = Mean - se, ymax = Mean + se),
    width = 0.5, position = position_dodge(0.8), linewidth = 0.5
  ) +
  geom_col(
    position = position_dodge(0.8),
    width = 0.8, colour = "black", linewidth = 0.4
  ) +
  scale_fill_manual(values = c("Plastisphere" = "#FFCC00", "Biofilm" = "#8bc483")) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0.4, "lines"),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, colour = "black", margin = margin(r = 20)),
    legend.position = c(-1, -0.1),
    legend.direction = "horizontal",
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  scale_y_log10()

# Right panel: effect estimates with CI (exclude baseline group)
effect_df_1d$var <- factor(effect_df_1d$var, levels = levels(stats_df_1d$var))
effect_df_1d$psig <- as.character(signif(effect_df_1d$p.value, 3))
non_baseline_effects_1d <- subset(effect_df_1d, Group != "b")

scatter_plot_1d <- ggplot(effect_df_1d, aes(var, estimate)) +
  annotate(
    "rect",
    xmin = seq(0.5, n_distinct(effect_df_1d$var) - 0.5, by = 2),
    xmax = seq(1.5, n_distinct(effect_df_1d$var) + 0.5, by = 2),
    ymin = -Inf, ymax = Inf,
    fill = "gray95", alpha = 0.5
  ) +
  {
    if (nrow(non_baseline_effects_1d) > 0) {
      list(
        geom_errorbar(
          data = non_baseline_effects_1d,
          aes(ymin = conf.low, ymax = conf.high),
          width = 0.5, size = 0.5, color = "black"
        ),
        geom_point(
          data = non_baseline_effects_1d,
          aes(fill = Group),
          shape = 21, size = 4, color = "black", stroke = 0.8
        )
      )
    }
  } +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("plastisphere" = "#FFCC00", "ckbiofilm" = "#8bc483", "none" = "white")) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0.4, "lines"),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 10),
    axis.text.y = element_blank(),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 15, colour = "black", hjust = 0.5)
  ) +
  scale_x_discrete(limits = levels(effect_df_1d$var))

# Middle panel: sample size labels
mid_counts_1d <- ggplot(stats_df_1d, aes(var, x = 1, label = n)) +
  geom_text(color = "black", size = 4) +
  theme_void()

# Combine panels and export
fig1d_plot <- bar_plot_1d + mid_counts_1d + scatter_plot_1d + plot_layout(widths = c(2, 0.2, 2))
fig1d_plot
ggsave(fig1d_plot, filename = "./figures/Fig1d.pdf",
       family = "ArialMT", height = 8, width = 8)
