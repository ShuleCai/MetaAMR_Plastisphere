pkgs <- c("dplyr", "ggplot2", "patchwork")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

sorted_names <- c("Total", "Quinolone", "Polymyxin", "Beta-lactam", "Bacitracin", "Aminoglycoside", "Glycopeptide", "Rifamycin", "Sulfonamide", "MLS", "Tetracenomycin_C", "Tetracycline")
freshwater_ta_f <- read.csv("./data/Fig5a1.csv") %>% mutate(ARG_category = factor(ARG_category, levels = sorted_names))
marine_ta_f <- read.csv("./data/Fig5a2.csv") %>% mutate(ARG_category = factor(ARG_category, levels = sorted_names))

scale_size_common <- function(limits) {
  scale_size_continuous(
    name = "log10(Abundance)",
    range = c(3, 12),
    trans = "exp",
    limits = limits,
    breaks = 0:4
  )
}

fill_scale_common <- scale_fill_gradientn(
  name = "ln(Transcriptional activity)",
  colours = c("#e0e5f9", "#0107ff"),
  limits = c(-3.570, 0.536),
  breaks = -4:1,
  labels = scales::number(-4:1, accuracy = 0.1),
  oob = scales::squish
)

theme_common <- theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 8),
    plot.margin  = margin(6, 18, 6, 6),
    panel.grid.major = element_line(linetype = "dashed", size = 0.5),
    panel.grid.minor = element_line(linetype = "dotted", size = 0.3),
  )

make_bubble_plot <- function(df, y_label, size_limits, x_text_element) {
  ggplot(df, aes(y = SampleName, x = ARG_category)) +
    labs(y = y_label) +
    geom_point(
      aes(
        size = abun_log10,
        fill = ta_ln
      ),
      shape = 21,
      color = "grey30",
      stroke = 0.3,
      alpha = 0.95
    ) +
    scale_size_common(size_limits) +
    fill_scale_common +
    scale_x_discrete(position = "top") +
    scale_y_discrete(expand = expansion(add = 0.35)) +
    coord_cartesian(clip = "off") +
    theme_common +
    theme(axis.text.x = x_text_element)
}

# Freshwater
p_fresh_bubble <- make_bubble_plot(
  freshwater_ta_f,
  y_label = "Freshwater plastisphere",
  size_limits = c(0.605, 3.922),
  x_text_element = element_text(angle = 45, hjust = 0)
)

# Marine
p_marine_bubble <- make_bubble_plot(
  marine_ta_f,
  y_label = "Marine plastisphere",
  size_limits = c(0.606, 3.922),
  x_text_element = element_blank()
)

# Merge
p_bubble_combined <- (p_fresh_bubble / p_marine_bubble) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("./figures/Fig5a.pdf", p_bubble_combined, width = 8, height = 5)
