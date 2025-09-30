# Funnel plot generation and publication bias assessment

library(metafor)

generate_funnel_plots <- function(processed_data, base_path, folder) {
  # Generate funnel plots and assess publication bias for all variables
  for (var in unique(processed_data$Variable)) {
    cat("Generating funnel plot for:", var, "\n")
    
    # Filter data for current variable
    df_var <- processed_data %>% 
      filter(Variable == var)     
    # Perform meta-analysis
    res <- rma(yi = lnRR, vi = v, data = df_var, method = "DL")
    
    # Assess publication bias using Egger's test
    egger_results <- perform_egger_test(res)
    
    # Generate funnel plot
    generate_single_funnel_plot(res, var, egger_results, base_path, folder)
  }
}

perform_egger_test <- function(meta_model) {
  # Perform Egger's test for publication bias
  egger_test <- regtest(meta_model)
  
  # If significant, use trim-and-fill method
  if (egger_test$pval <= 0.05) {
    trimmed_model <- regtest(trimfill(meta_model))
    return(list(
      pval = trimmed_model$pval,
      zval = trimmed_model$zval,
      method = "trimfill"
    ))
  } else {
    return(list(
      pval = egger_test$pval,
      zval = egger_test$zval,
      method = "standard"
    ))
  }
}

generate_single_funnel_plot <- function(meta_model, var_name, egger_results, base_path, folder) {
  # Generate a single funnel plot with publication bias statistics
  pdf(paste0(base_path, folder, "funnels/funnel_plot_", var_name, ".pdf"), 
      family = "ArialMT", width = 6, height = 6)
  
  # Create funnel plot
  funnel(meta_model, main = var_name, xlab = "lnRR", ylab = "Standard error")
  
  # Add publication bias statistics to plot
  add_bias_statistics(egger_results)
  
  dev.off()
}

add_bias_statistics <- function(egger_results) {
  # Add publication bias statistics to the current plot
  usr <- par("usr")  # Get plot boundaries
  x_min <- usr[1]    
  y_max <- usr[4]    
  
  # Add statistics text in top-left corner
  text(
    x = x_min + 0.02 * (usr[2] - x_min),
    y = y_max - 0.02 * (y_max - usr[3]),
    labels = paste0("z = ", round(egger_results$zval, digits = 3),
                    "\np = ", format.pval(egger_results$pval, digits = 3)),
    adj = c(0, 1),
    cex = 0.9,
    font = 1
  )
}