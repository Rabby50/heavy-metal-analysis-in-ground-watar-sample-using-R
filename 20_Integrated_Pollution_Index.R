# Task 20: Contamination Indices and Risk Assessment
# Integrated Pollution Index (Nemerow Pollution Index)
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

required_packages <- c("readxl", "ggplot2")
install_and_load(required_packages)

# Load data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)
metal_columns <- setdiff(names(data), "Number")
concentrations <- data[, metal_columns]
concentrations[] <- lapply(concentrations, as.numeric)

# Reference guideline values used for the pollution index
reference_limits <- c(
  As_75 = 10,
  Se_82 = 40,
  Pb_208 = 10,
  Cd_111 = 3,
  Cr_52 = 50,
  Ni_60 = 70,
  Cu_63 = 2000,
  Zn_66 = 3000,
  Hg_202 = 6,
  Co_59 = 50,
  Be_9 = 4,
  V_51 = 100,
  Fe_57 = 300,
  Mn_55 = 400
)

reference_limits <- reference_limits[metal_columns]

# Pollution index for each metal and sample
pi_matrix <- sweep(as.matrix(concentrations), 2, reference_limits, "/")
mean_pi <- rowMeans(pi_matrix, na.rm = TRUE)
max_pi <- apply(pi_matrix, 1, max, na.rm = TRUE)
nemerow_pi <- sqrt((mean_pi^2 + max_pi^2) / 2)

# Classification
category <- cut(
  nemerow_pi,
  breaks = c(-Inf, 1, 2, 3, Inf),
  labels = c("Clean", "Low pollution", "Moderate pollution", "Heavy pollution"),
  right = TRUE
)

summary_table <- data.frame(
  Sample = data$Number,
  NIPI = nemerow_pi,
  Category = as.character(category),
  stringsAsFactors = FALSE
)

category_counts <- as.data.frame(table(summary_table$Category))
names(category_counts) <- c("Category", "Count")

cat("INTEGRATED POLLUTION INDEX (NEMEROW PI)\n\n")
cat("Reference limits used (same order as workbook columns):\n")
print(reference_limits)
cat("\nSummary statistics:\n")
print(summary(nemerow_pi))
cat("\nCategory counts:\n")
print(category_counts, row.names = FALSE)
cat("\nTop samples by NIPI:\n")
print(summary_table[order(summary_table$NIPI, decreasing = TRUE), ][1:5, ], row.names = FALSE)
cat("\nInterpretation:\n")
cat("- NIPI values below 1 indicate clean conditions, 1-2 low pollution, 2-3 moderate pollution, and above 3 heavy pollution.\n")
cat(sprintf("- In this dataset, %d of %d samples fall in the heavy pollution class.\n", sum(summary_table$Category == "Heavy pollution", na.rm = TRUE), nrow(summary_table)))
cat("- Higher values are driven by metals with larger exceedances relative to their reference limits.\n")

# Save statistics
write.csv(summary_table, "20_integrated_pollution_index_values.csv", row.names = FALSE)
write.csv(category_counts, "20_integrated_pollution_index_category_counts.csv", row.names = FALSE)
writeLines(capture.output(print(summary_table[order(summary_table$NIPI, decreasing = TRUE), ], row.names = FALSE)), "20_integrated_pollution_index_ranked.txt")

# Plot
plot_df <- summary_table
plot_df$Sample <- factor(plot_df$Sample, levels = plot_df$Sample[order(plot_df$NIPI)])

ipi_plot <- ggplot(plot_df, aes(x = Sample, y = NIPI, fill = NIPI)) +
  geom_col(color = "grey25", width = 0.8) +
  geom_hline(yintercept = 1, color = "blue", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 2, color = "orange", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 3, color = "red", linetype = "dashed", linewidth = 0.9) +
  scale_fill_gradientn(colors = c("#2ca25f", "#fee08b", "#fdae61", "#d7301f")) +
  labs(
    title = "Contamination Indices and Risk Assessment",
    subtitle = "Integrated Pollution Index",
    x = "Sample",
    y = "Nemerow Index (NIPI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold")
  )

plot_file <- "20_integrated_pollution_index.png"
ggsave(plot_file, ipi_plot, width = 12, height = 7, dpi = 300)
print(ipi_plot)
cat(sprintf("\nPlot saved as: %s\n", plot_file))
