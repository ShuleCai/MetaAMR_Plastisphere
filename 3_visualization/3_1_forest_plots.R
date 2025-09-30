# Forest plot generation functions

library(ggplot2)
library(patchwork)

generate_forest_plots <- function(results_all, stats_all, base_path, folder, plot_colors) {
  # Generate forest plots for all variables
  for (var_name in names(results_all)) {
    cat("Generating forest plot for:", var_name, "\n")
    
    # Prepare data for plotting
    plot_data <- prepare_forest_plot_data(results_all[[var_name]], stats_all[[var_name]])
    
    # Generate individual plot components
    bar_plot <- create_bar_plot(plot_data$stats_df, plot_colors)
    scatter_plot <- create_scatter_plot(plot_data$effect_df, plot_colors)
    mid_plot <- create_mid_plot(plot_data$stats_df)
    
    # Combine plots
    combined_plot <- bar_plot + mid_plot + scatter_plot + 
      plot_layout(widths = c(2, 0.2, 2))
    
    # Save plot
    ggsave(combined_plot, 
           filename = paste0(base_path, folder, "forest_ckenvironment_", var_name, ".pdf"), 
           family = "ArialMT", height = 8, width = 8)
  }
}

prepare_forest_plot_data <- function(effect_df, stats_df) {
  # Prepare data for forest plot generation
  effect_df <- effect_df %>% arrange(sequence)
  stats_df <- stats_df %>% mutate(var = factor(var, levels = rev(effect_df$var)))
  
  # Ensure factor levels match between effect and stats data
  effect_df$var <- factor(effect_df$var, levels = levels(stats_df$var))
  effect_df$psig <- as.character(signif(effect_df$p.value, 3))
  
  return(list(effect_df = effect_df, stats_df = stats_df))
}

create_bar_plot <- function(stats_df, plot_colors) {
  # Create bar plot for descriptive statistics
  ggplot(stats_df, aes(var, Mean, fill = Treatment)) +
    # Add alternating background
    annotate('rect', 
             xmin = seq(0.5, n_distinct(stats_df$var) - 0.5, by = 2), 
             xmax = seq(1.5, n_distinct(stats_df$var) + 0.5, by = 2), 
             ymin = -Inf, ymax = Inf, fill = 'gray95', alpha = 0.5) +
    geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se),
                  width = 0.5, position = position_dodge(0.8), linewidth = 0.5) +
    geom_col(position = position_dodge(0.8), width = 0.8, 
             colour = "black", linewidth = 0.4) +
    scale_fill_manual(values = plot_colors) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = 'transparent'),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0.4, "lines"), 
      axis.ticks = element_line(color = 'black'),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(colour = 'black', size = 12),
      axis.text = element_text(colour = 'black', size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 12, colour = "black",
                                 margin = margin(r = 20)),
      legend.position = c(-1, -0.1),
      legend.direction = "horizontal",
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.5, "cm")
    ) +
    scale_y_log10()
}

create_scatter_plot <- function(effect_df, plot_colors) {
  # Create scatter plot for effect sizes
  ggplot(effect_df, aes(var, estimate)) +
    # Add alternating background
    annotate('rect', 
             xmin = seq(0.5, n_distinct(effect_df$var) - 0.5, by = 2), 
             xmax = seq(1.5, n_distinct(effect_df$var) + 0.5, by = 2), 
             ymin = -Inf, ymax = Inf, fill = 'gray95', alpha = 0.5) +
    # Add error bars and points for significant effects
    {
      if (any(effect_df$Group != "b")) {
        list(
          geom_errorbar(
            data = subset(effect_df, Group != "b"),
            aes(ymin = conf.low, ymax = conf.high), 
            width = 0.5, size = 0.5, color = "black"
          ),
          geom_point(
            data = subset(effect_df, Group != "b"),
            aes(fill = Group), 
            shape = 21, size = 4, color = "black", stroke = 0.8
          )
        )
      }
    } +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(values = plot_colors) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = 'transparent'),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0.4, "lines"), 
      axis.ticks = element_line(color = 'black'),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(colour = 'black', size = 12),
      axis.text = element_text(colour = 'black', size = 10),
      axis.text.y = element_blank(),
      legend.position = "none",
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(size = 15, colour = "black", hjust = 0.5)
    ) +
    scale_x_discrete(limits = levels(effect_df$var))
}

create_mid_plot <- function(stats_df) {
  # Create middle plot with sample sizes
  ggplot(stats_df, aes(var, x = 1, label = n)) +
    geom_text(color = "black", size = 4) +
    theme_void()
}