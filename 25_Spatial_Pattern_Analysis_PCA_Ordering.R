# Task 25: Spatial Pattern Analysis (Using PCA-based ordering)
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

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2")
install_and_load(required_packages)

# Load data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)
metal_columns <- setdiff(names(data), "Number")
metal_data <- data[, metal_columns]
metal_data[] <- lapply(metal_data, as.numeric)

clean_metal_symbol <- function(x) {
  gsub("[^A-Za-z].*$", "", x)
}

metal_symbols <- clean_metal_symbol(metal_columns)

# PCA-based ordering by PC1 score
pca_result <- stats::prcomp(metal_data, center = TRUE, scale. = TRUE)
pc_scores <- as.data.frame(pca_result$x)
order_idx <- order(pc_scores$PC1)

# Keep metal set in demo order when available
preferred_order <- c("As", "B", "Ba", "Cr", "Cu", "Fe", "Mn", "Pb", "Sn", "Sr", "Ti", "Zn")
available_idx <- which(metal_symbols %in% preferred_order)

if (length(available_idx) < 6) {
  var_rank <- order(apply(metal_data, 2, stats::var, na.rm = TRUE), decreasing = TRUE)
  selected_idx <- var_rank[1:min(12, ncol(metal_data))]
} else {
  selected_symbols <- preferred_order[preferred_order %in% metal_symbols]
  selected_idx <- unlist(lapply(selected_symbols, function(sym) which(metal_symbols == sym)[1]))
}

selected_data <- metal_data[order_idx, selected_idx, drop = FALSE]
selected_symbols <- clean_metal_symbol(colnames(selected_data))

spatial_df <- as.data.frame(selected_data)
spatial_df$Order <- seq_len(nrow(spatial_df))

spatial_long <- spatial_df |>
  tidyr::pivot_longer(cols = -Order, names_to = "MetalColumn", values_to = "Concentration")

spatial_long$Metal <- factor(clean_metal_symbol(spatial_long$MetalColumn), levels = selected_symbols)

plot_spatial <- ggplot(spatial_long, aes(x = Order, y = Concentration, group = Metal)) +
  geom_line(color = "#377eb8", linewidth = 0.7, alpha = 0.8) +
  geom_point(color = "#377eb8", size = 1.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~Metal, scales = "free_y", ncol = 4) +
  labs(
    title = "Spatial Pattern Analysis by PCA-based Sample Ordering",
    x = "Sample Order (PC1)",
    y = "Concentration"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

plot_file <- "25_spatial_pattern_analysis_pca_ordering.png"
ggsave(plot_file, plot_spatial, width = 14, height = 10, dpi = 300)
print(plot_spatial)

# Save ordered concentration table
ordered_output <- data.frame(Sample_Order_PC1 = seq_len(nrow(selected_data)), selected_data, check.names = FALSE)
write.csv(ordered_output, "25_spatial_pattern_ordered_data.csv", row.names = FALSE)

cat("\nInterpretation note:\n")
cat("- Samples are ordered by PC1 score to expose dominant multivariate gradient structure.\n")
cat("- Upward dashed trends indicate increasing concentration along the dominant gradient, while flat trends indicate weak ordering effects.\n")
cat("- Metals with similar trend directions are likely co-varying under common controls.\n")
cat(sprintf("- Figure saved as: %s\n", plot_file))
