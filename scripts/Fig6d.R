if(!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# data_mags <- read.csv("./data/Fig6b.csv")
# table(data_mags$group) # plas:327, env:475

# Create the data frame
data <- data.frame(
  category = c("328", "474"),
  value = c(328, 474)
)

# Calculate percentages for labels
data$percentage <- paste0(round(data$value / sum(data$value) * 100, 1), "%")

# Hollow pie (donut) chart
ggplot(data, aes(x = 2, y = value, fill = category)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = percentage),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#ef885d", "#6db79c")) +
  theme_void() +
  xlim(0.8, 2.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    legend.title = element_blank()
  )

ggsave("./figures/Fig6d.pdf", width = 4, height = 4)
