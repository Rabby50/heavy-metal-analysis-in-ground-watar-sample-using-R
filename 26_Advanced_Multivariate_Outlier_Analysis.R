# Task 26: Advanced Multivariate Outlier Analysis
# (a) Multivariate outlier detection
# (b) PCA with outlier identification
# Self-contained script for HEAVY_METAL_DATA.xlsx

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

required_packages <- c("readxl", "ggplot2", "patchwork")
install_and_load(required_packages)

# Load and prepare data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)
metal_columns <- setdiff(names(data), "Number")
metal_data <- data[, metal_columns]
metal_data[] <- lapply(metal_data, as.numeric)

X <- scale(metal_data)
n <- nrow(X)
p <- ncol(X)

# Mahalanobis distance with chi-square threshold
center_vec <- colMeans(X, na.rm = TRUE)
cov_mat <- stats::cov(X, use = "pairwise.complete.obs")
md <- stats::mahalanobis(X, center = center_vec, cov = cov_mat)
threshold_95 <- stats::qchisq(0.95, df = p)
is_outlier <- md > threshold_95

sample_id <- if ("Number" %in% names(data)) data$Number else seq_len(n)

outlier_df <- data.frame(
  Sample = sample_id,
  SampleIndex = seq_len(n),
  Mahalanobis = md,
  Outlier = is_outlier,
  stringsAsFactors = FALSE
)

# PCA for outlier display
pca <- stats::prcomp(X, center = TRUE, scale. = FALSE)
var_pct <- (pca$sdev ^ 2) / sum(pca$sdev ^ 2) * 100
pca_df <- data.frame(
  Sample = sample_id,
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  Outlier = is_outlier
)

left_plot <- ggplot(outlier_df, aes(x = SampleIndex, y = Mahalanobis)) +
  geom_col(fill = "#6c8ebf", color = "grey35", width = 0.75, alpha = 0.9) +
  geom_hline(yintercept = threshold_95, color = "red", linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = max(outlier_df$SampleIndex) * 0.78, y = threshold_95 + 0.8,
           label = sprintf("95%% threshold (%.1f)", threshold_95), color = "grey20", size = 3.2) +
  labs(
    title = "Multivariate Outlier Detection",
    x = "Sample Index",
    y = "Mahalanobis Distance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

right_plot <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(
    aes(shape = Outlier, color = Outlier),
    size = 3,
    alpha = 0.8
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), labels = c(`FALSE` = "Normal", `TRUE` = "Outlier")) +
  scale_color_manual(values = c(`FALSE` = "#4f81bd", `TRUE` = "#d62728"), labels = c(`FALSE` = "Normal", `TRUE` = "Outlier")) +
  labs(
    title = "PCA with Outlier Identification",
    x = sprintf("PC1 (%.1f%%)", var_pct[1]),
    y = sprintf("PC2 (%.1f%%)", var_pct[2]),
    shape = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

combined_plot <- left_plot + right_plot +
  patchwork::plot_annotation(title = "Advanced Multivariate Outlier Analysis")

plot_file <- "26_advanced_multivariate_outlier_analysis.png"
ggsave(plot_file, combined_plot, width = 12, height = 6, dpi = 300)
print(combined_plot)

write.csv(outlier_df, "26_multivariate_outlier_results.csv", row.names = FALSE)
write.csv(pca_df, "26_multivariate_outlier_pca_scores.csv", row.names = FALSE)

cat("\nInterpretation note:\n")
cat("- Samples above the chi-square threshold are multivariate anomalies relative to the joint metal profile.\n")
cat("- PCA view confirms whether outliers separate from the main cloud in reduced dimensions.\n")
cat(sprintf("- Outliers detected: %d of %d samples.\n", sum(is_outlier), n))
cat(sprintf("- Figure saved as: %s\n", plot_file))
