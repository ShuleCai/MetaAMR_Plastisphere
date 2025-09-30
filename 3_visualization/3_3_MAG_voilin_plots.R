# Visualization functions for MAG (Metagenome-Assembled Genome) analysis results

library(ggplot2)
library(ggpubr)
library(dplyr)

create_mag_violin_plots <- function(mag_data, output_path) {
  # Create violin plots for multiple MAG characteristics
  
  # Define metrics to plot
  metrics <- list(
    list(metric = "Genome_Size", title = NULL, y_label = "Genome size (M)"),
    list(metric = "GC", title = NULL, y_label = "GC content (%)"),
    list(metric = "arg_cnt", title = NULL, y_label = "ARG count"),
    list(metric = "mge_cnt", title = NULL, y_label = "MGE count"),
    list(metric = "vfg_cnt", title = NULL, y_label = "VFG count")
  )
  
  # Generate individual plots
  plots <- list()
  for (i in seq_along(metrics)) {
    plots[[i]] <- create_single_mag_violin_plot(
      data = mag_data,
      metric = metrics[[i]]$metric,
      title = metrics[[i]]$title,
      y_label = metrics[[i]]$y_label
    )
  }
  
  # Combine plots
  combined_plot <- ggarrange(
    plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
    ncol = 3, nrow = 2, 
    common.legend = TRUE, 
    legend = "bottom"
  )
  
  # Save combined plot
  ggsave(
    combined_plot,
    filename = paste0(output_path, "MAG_violin_plots.pdf"),
    width = 10,
    height = 8
  )
  
  cat("MAG violin plots saved to:", paste0(output_path, "MAG_violin_plots.pdf"), "\n")
  
  return(combined_plot)
}

create_single_mag_violin_plot <- function(data, metric, title, y_label) {
  # Create a single violin plot for MAG data
  
  # Filter data to include only plas and env groups
  plot_data <- data %>% 
    filter(group %in% c("plas", "env")) %>% 
    mutate(group = factor(group, levels = c("plas", "env")))
  
  # Check if we have enough data for comparison
  if (n_distinct(plot_data$group) < 2) {
    warning(paste("Insufficient groups for", metric, "comparison"))
    return(ggplot() + labs(title = "Insufficient Data"))
  }
  
  # Perform t-test
  stats_test <- t.test(plot_data[[metric]] ~ plot_data$group)
  p_value <- stats_test$p.value
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = group, y = .data[[metric]], fill = group)) +
    geom_violin(scale = "width", width = 0.7, alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.7) +
    stat_compare_means(
      label = "p.signif", 
      method = "t.test", 
      label.x = 1.5, 
      label.y = max(plot_data[[metric]], na.rm = TRUE) * 0.95,
      symnum.args = list(
        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
        symbols = c("***", "**", "*", "ns")
      )
    ) +
    labs(
      title = title,
      x = "Group",
      y = y_label
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      panel.grid.minor = element_blank()
    ) +
    scale_fill_manual(
      values = c("plas" = "#FFCC00", "env" = "#56B4E9"),
      labels = c("plas" = "Plastisphere", "env" = "Environment")
    )
  
  return(p)
}

create_mag_triple_presence_pie_chart <- function(mag_data, output_path) {
  # Create pie chart for triple presence statistics
  
  # Calculate counts for plas group
  plas_data <- mag_data %>% filter(group == "plas")
  plas_triple <- sum(plas_data$triple_presence)
  plas_non_triple <- nrow(plas_data) - plas_triple
  
  # Prepare data for pie chart
  pie_data <- data.frame(
    category = c("Triple Present", "Not Triple Present"),
    value = c(plas_triple, plas_non_triple)
  )
  
  # Calculate percentages
  pie_data$percentage <- paste0(
    round(pie_data$value / sum(pie_data$value) * 100, 1), 
    "%"
  )
  
  # Create pie chart (donut chart)
  pie_plot <- ggplot(pie_data, aes(x = 2, y = value, fill = category)) +
    geom_col(width = 1, color = "white") +
    coord_polar("y", start = 0) +
    geom_text(
      aes(label = percentage),
      position = position_stack(vjust = 0.5),
      color = "white", 
      fontface = "bold",
      size = 4
    ) +
    scale_fill_manual(
      values = c("Triple Present" = "#8dbea7", "Not Triple Present" = "#e09772")
    ) +
    theme_void() +
    xlim(0.5, 2.5) +  # Creates donut hole
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    ) +
    labs(
      title = "Plastisphere-enriched MAGs\nwith ARG+MGE+VFG"
    )
  
  # Save pie chart
  ggsave(
    pie_plot,
    filename = paste0(output_path, "MAG_triple_presence_pie.pdf"),
    width = 6,
    height = 6
  )
  
  cat("MAG pie chart saved to:", paste0(output_path, "MAG_triple_presence_pie.pdf"), "\n")
  
  return(pie_plot)
}