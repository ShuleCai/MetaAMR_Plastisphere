pkgs <- c("dplyr", "ggplot2", "patchwork")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

data_df <- read.csv("./data/Fig7b.csv")
traits_vector <- c(
  "Amino acid, fatty acid and nucleotide synthesis",
  "Bacterial biomass degradation enzyme",
  "Oligosaccharides degradation enzymes",
  "Primary metabolism",
  "Carbohydrate central metabolism",
  "Osmolytes",
  "Quorum sensing",
  "Membrane synthesis and repair",
  "Biofilm formation and adhesion",
  "Siderophores",
  "Uptake system",
  "Chemotaxis",
  "Sporulation",
  "Chaperons",
  "Sigma factor",
  "ROS generation",
  "SOS response",
  "Antibiotic",
  "Motility",
  "Pathogenic interactions with human",
  "rRNA gene copies",
  "GC (%)",
  "Genome size"
) 
plot_func <- function(split_name) {
  effect_df <- data_df %>% filter(split == split_name) %>% mutate(x = factor(var, levels = traits_vector))
  
  p <- ggplot(effect_df, aes(x, estimate)) +
    annotate('rect',
             xmin = seq(0.5, n_distinct(effect_df$x)-0.5, by = 2),
             xmax = seq(1.5, n_distinct(effect_df$x)+0.5, by = 2),
             ymin = -Inf, ymax = Inf, fill = 'gray95', alpha = 0.5) +
    {
      if(any(effect_df$Group != "b")) {
        list(
          geom_errorbar(
            data = subset(effect_df, Group != "b"),
            aes(ymin = conf.low, ymax = conf.high),
            width = 0.5, size = 0.5, color = "black"
          ),
          geom_point(
            data = subset(effect_df, Group != "b"),
            aes(fill = Group),
            shape = 21, size = 4, color = "black", stroke=0.8
          )
        )
      }
    } +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_discrete() +
    scale_fill_manual(values = c("plastisphere" = "#FFCC00", "ckenvironment" = "#56B4E9", "none" = "white")) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(),
          axis.ticks.length = unit(0.4,"lines"),
          axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(colour='black', size=12),
          axis.text=element_text(colour='black',size=10),
          axis.text.y = element_text(colour='black', size=12),
          legend.position = "none",
          axis.line.y = element_blank(),
          plot.title = element_text(size = 15,colour = "black",hjust = 0.5)) +
    scale_x_discrete(limits = levels(effect_df$x))
  return(p)
}
p <- plot_func("Marine") 

ggsave(p, filename = "./figures/Fig7b.pdf", width = 6, height = 6)
