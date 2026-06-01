# Task 3: Quartile distribution summary for each detected metal
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

required_packages <- c("readxl", "ggplot2", "tidyr", "dplyr")
install_and_load(required_packages)

# Load workbook data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)
metal_columns <- setdiff(names(data), "Number")

# Long format for quartile boxplot
long_data <- data |>
  dplyr::select(Number, dplyr::all_of(metal_columns)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(metal_columns),
    names_to = "Metal",
    values_to = "Concentration"
  )

# Statistics summary for each metal
summary_table <- long_data |>
  dplyr::group_by(Metal) |>
  dplyr::summarise(
    N = dplyr::n(),
    Mean = mean(Concentration, na.rm = TRUE),
    SD = stats::sd(Concentration, na.rm = TRUE),
    Median = stats::median(Concentration, na.rm = TRUE),
    Q1 = stats::quantile(Concentration, 0.25, na.rm = TRUE),
    Q3 = stats::quantile(Concentration, 0.75, na.rm = TRUE),
    IQR = stats::IQR(Concentration, na.rm = TRUE),
    CV_Percent = (stats::sd(Concentration, na.rm = TRUE) / mean(Concentration, na.rm = TRUE)) * 100,
    .groups = "drop"
  )

# Print exact statistical summaries
for (i in seq_len(nrow(summary_table))) {
  row <- summary_table[i, ]
  cat(sprintf("STATISTICAL SUMMARY %s:\n", row$Metal))
  cat(sprintf("  Mean ± SD: %.2f ± %.2f\n", row$Mean, row$SD))
  cat(sprintf("  Median (IQR): %.2f (%.2f - %.2f)\n", row$Median, row$Q1, row$Q3))
  cat(sprintf("  CV: %.1f%%\n\n", row$CV_Percent))
}

# Save summary tables
write.csv(summary_table, "18_quartile_summary_statistics.csv", row.names = FALSE)
writeLines(capture.output(print(summary_table)), "18_quartile_summary_statistics.txt")

# Combined boxplot for quartile distribution
boxplot_plot <- ggplot(long_data, aes(x = Metal, y = Concentration, fill = Metal)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#b23a48", outlier.size = 1.8) +
  scale_y_log10() +
  labs(
    title = "Quartile Distribution of Heavy Metal Concentrations",
    x = "Metal",
    y = "Concentration (log10 scale)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

plot_file <- "18_quartile_distribution_boxplot.png"
ggsave(plot_file, boxplot_plot, width = 12, height = 7, dpi = 300)
print(boxplot_plot)
cat(sprintf("\nQuartile distribution plot saved as: %s\n", plot_file))
