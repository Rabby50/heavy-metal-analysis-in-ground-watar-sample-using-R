# Task 2: PCA Biplot (PC1 vs PC2)
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
metal_columns <- setdiff(names(data), "Number")
numeric_data <- data[, metal_columns]
numeric_data[] <- lapply(numeric_data, as.numeric)

# PCA on standardized variables
pca_result <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
variance_percent <- (pca_result$sdev ^ 2) / sum(pca_result$sdev ^ 2) * 100
cumulative_percent <- cumsum(variance_percent)

scores <- as.data.frame(pca_result$x[, 1:2])
scores$Sample <- data$Number
loadings <- as.data.frame(pca_result$rotation[, 1:2])
loadings$Metal <- rownames(loadings)

# Scale loadings to score space for visualization
score_range_x <- diff(range(scores$PC1))
score_range_y <- diff(range(scores$PC2))
loading_range_x <- diff(range(loadings$PC1))
loading_range_y <- diff(range(loadings$PC2))
scale_factor <- 0.75 * min(score_range_x / loading_range_x, score_range_y / loading_range_y)
loadings$PC1_scaled <- loadings$PC1 * scale_factor
loadings$PC2_scaled <- loadings$PC2 * scale_factor

# Exact PCA results for this project
pc1_top <- loadings[order(abs(loadings$PC1), decreasing = TRUE), c("Metal", "PC1")][1:3, ]

cat("PCA RESULTS for HEAVY_METAL_DATA.xlsx\n\n")
cat(sprintf("PC1 variance: %.2f%%\n", variance_percent[1]))
cat(sprintf("PC2 variance: %.2f%%\n", variance_percent[2]))
cat(sprintf("Cumulative variance (first 3 PCs): %.2f%%\n\n", cumulative_percent[3]))
cat("Top 3 loadings for PC1:\n")
print(pc1_top, row.names = FALSE)

# Save summary files
write.csv(scores, "17_pca_biplot_scores.csv", row.names = FALSE)
write.csv(loadings, "17_pca_biplot_loadings.csv", row.names = FALSE)

# Build biplot
biplot_plot <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.5) +
  geom_point(size = 3, color = "#2a6fdb", alpha = 0.85) +
  geom_text(aes(label = Sample), vjust = -0.8, size = 3) +
  geom_segment(
    data = loadings,
    aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
    arrow = grid::arrow(length = grid::unit(0.22, "cm")),
    color = "#d1495b",
    linewidth = 0.8
  ) +
  geom_text(
    data = loadings,
    aes(x = PC1_scaled, y = PC2_scaled, label = Metal),
    color = "#8c1d40",
    size = 3.5,
    fontface = "bold",
    vjust = -0.6
  ) +
  labs(
    title = "PCA Biplot (PC1 vs PC2) for Heavy Metals",
    x = sprintf("PC1 (%.2f%%)", variance_percent[1]),
    y = sprintf("PC2 (%.2f%%)", variance_percent[2])
  ) +
  coord_equal() +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

plot_file <- "17_pca_biplot_pc1_pc2.png"
ggsave(plot_file, biplot_plot, width = 10, height = 8, dpi = 300)
print(biplot_plot)
cat(sprintf("\nPCA biplot saved as: %s\n", plot_file))
