pkgs <- c("dplyr", "igraph", "ggraph", "ggplot2", "tidygraph")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

node_df <- read.csv("./data/Fig4_nodes.csv")
edge_df <- read.csv("./data/Fig4_edges.csv")

g <- graph_from_data_frame(edge_df, vertices = node_df, directed = FALSE)

# node colors
node_colors <- c(
    "#E5C494", "#377EB8", "#4DAF4A", "#984EA3", 
    "#FF7F00", "#8DA0CB", "#d39f8d", "#A65628",
    "#F781BF", "#66C2A5", "#A6D854", "#FFD92F",
    "#6cbcfa"
)

# edge colors
edge_categories <- unique(edge_df$color)
edge_colors <- c(
    "#377EB8", "#984EA3", "#FF7F00", "#d39f8d",
    "#A65628", "#F781BF", "#999999", "#66C2A5",
    "#FC8D62", "#8DA0CB", "#E78AC3", "#FFD92F",
    "#E5C494", "#B3B3B3"
)
if (length(edge_colors) < length(edge_categories)) { 
    edge_colors <- rainbow(length(edge_categories))
}
names(edge_colors) <- edge_categories

set.seed(123)

# transfer igraph to tbl_graph
tg <- as_tbl_graph(g) %>%
    mutate(node_size = 2) %>%
    activate(nodes) %>%
    group_by(type) %>%
    mutate(type_cnt = n()) %>%
    arrange(desc(type_cnt)) %>%
    mutate(type = factor(type, levels = unique(type)))

# create layout
lay <- create_layout(tg,
    layout = "fr",
    niter = 9000,
    start.temp = 0.5,
    grid = "nogrid"
)

network_plot <- ggraph(lay, rescale = F) +
    geom_edge_fan(
        aes(color = color),
        strength = 5
    ) +
    scale_edge_color_manual(
        values = edge_colors,
        name = "ARG category",
        guide = guide_legend(
            order = 3,
            override.aes = list(edge_width = 2, alpha = 1)
        )
    ) +
    geom_node_point(
        aes(
            color = type,
            shape = shape,
            size = node_size
        ),
        alpha = 1,
        stroke = 0.4
    ) +
    scale_color_manual(
        values = node_colors,
        name = "Taxonomy (phylum)"
    ) +
    scale_shape_manual(
        values = c(
          "Natural biofilm" = 15,
          "Surrounding water" = 16, 
          "Plastisphere" = 17
        ),
        name = "Environment",
        na.value = 18
    ) +
    scale_size_continuous(
        range = c(4),
        guide = "none"
    ) +
    theme_void() +
    theme(
        plot.margin = margin(20, 20, 20, 20),
        legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.spacing.y = unit(0.5, "cm"),
        legend.key = element_rect(fill = NA, color = NA)
    ) +
    labs(
        title = NULL
    ) + guides(
        color = guide_legend(
            order = 1,
            title.position = "top",
            override.aes = list(size = 5, shape = 16)
        ),
        shape = guide_legend(
            order = 2,
            title.position = "top",
            override.aes = list(size = 5)
        ),
        edge_color = guide_legend(
            order = 3,
            title.position = "top",
            override.aes = list(edge_width = 2)
        )
    )
ggsave("./figures/Fig4.pdf", network_plot, width = 10, height = 8)

