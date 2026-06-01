# Task 1: Scree Plot
# Self-contained analysis script for HEAVY_METAL_DATA.xlsx

install_and_load <- function(packages) {
  for (package_name in packages) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      install.packages(package_name, dependencies = TRUE)
    }
    suppressPackageStartupMessages(
      library(package_name, character.only = TRUE)
    )
  }
}

required_packages <- c("readxl", "ggplot2")
install_and_load(required_packages)

# Load workbook data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)

# Prepare numeric matrix for PCA
metal_columns <- setdiff(names(data), "Number")
numeric_data <- data[, metal_columns]
numeric_data[] <- lapply(numeric_data, as.numeric)

# PCA on standardized variables
pca_result <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
explained_variance <- (pca_result$sdev ^ 2) / sum(pca_result$sdev ^ 2)
variance_percent <- explained_variance * 100
cumulative_percent <- cumsum(variance_percent)

scree_df <- data.frame(
  PC = seq_along(variance_percent),
  Variance = variance_percent,
  Cumulative = cumulative_percent
)

# Print exact PCA summary values
cat(sprintf("PCA RESULTS for %s\n\n", data_file))
cat(sprintf("PC1 variance: %.2f%%\n", variance_percent[1]))
cat(sprintf("PC2 variance: %.2f%%\n", variance_percent[2]))
cat(sprintf("PC3 variance: %.2f%%\n", variance_percent[3]))
cat(sprintf("Cumulative variance (first 3 PCs): %.2f%%\n\n", cumulative_percent[3]))
cat("Scree data:\n")
print(scree_df)

# Save summary table
write.csv(scree_df, "16_scree_plot_pca_variance.csv", row.names = FALSE)

# Plot scree plot
scree_plot <- ggplot(scree_df, aes(x = factor(PC), y = Variance)) +
  geom_col(fill = "#4c78a8", width = 0.75) +
  geom_line(aes(group = 1), color = "#1f3c88", linewidth = 1) +
  geom_point(color = "#1f3c88", size = 2.5) +
  geom_text(aes(label = sprintf("%.1f%%", Variance)), vjust = -0.6, size = 3.5) +
  labs(
    title = "Scree Plot for Heavy Metal PCA",
    x = "Principal Component",
    y = "Variance Explained (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

plot_file <- "16_scree_plot.png"
ggsave(plot_file, scree_plot, width = 10, height = 7, dpi = 300)
print(scree_plot)
cat(sprintf("\nScree plot saved as: %s\n", plot_file))
