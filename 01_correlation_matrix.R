# Heavy Metal Correlation Matrix with Hierarchical Clustering
# Self-contained analysis script - no external sourcing required

# Install and load packages
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

required_packages <- c("readxl", "dplyr", "ggplot2", "pheatmap")
install_and_load(required_packages)

# Load data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)

# Extract metal columns (exclude 'Number')
metal_columns <- setdiff(names(data), "Number")

# Convert to numeric
numeric_data <- data[, metal_columns]
numeric_data[] <- lapply(numeric_data, as.numeric)

# Print data overview
cat("DATA OVERVIEW (n=", nrow(data), ")\n\n", sep = "")
print(summary(numeric_data))
cat("\nBASIC STATISTICS (Standard Deviation)\n\n")
sd_values <- round(sapply(numeric_data, sd, na.rm = TRUE), 4)
print(sd_values)
cat("\n")

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")

# Create heatmap with hierarchical clustering
output_file <- "heavy_metal_correlation_matrix.png"
png(output_file, width = 1800, height = 1600, res = 220)
pheatmap::pheatmap(
  correlation_matrix,
  color = colorRampPalette(c("#2c7bb6", "white", "#d7191c"))(100),
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  border_color = "white",
  display_numbers = TRUE,
  number_format = "%.2f",
  fontsize_number = 8,
  main = "Heavy Metal Correlation Matrix with Hierarchical Clustering"
)
dev.off()

cat("Correlation matrix plot saved as:", output_file, "\n")

# Save correlation matrix as CSV
write.csv(correlation_matrix, "heavy_metal_correlation_matrix.csv", row.names = TRUE)
cat("Correlation matrix CSV saved as: heavy_metal_correlation_matrix.csv\n")
