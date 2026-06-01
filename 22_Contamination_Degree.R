# Task 22: Contamination Indices and Risk Assessment
# Contamination Degree (Cd)
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

# Reference limits
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

# Contamination factor and degree
cf_matrix <- sweep(as.matrix(concentrations), 2, reference_limits, "/")
cd_values <- rowSums(cf_matrix, na.rm = TRUE)
mean_cf <- rowMeans(cf_matrix, na.rm = TRUE)

category <- cut(
  cd_values,
  breaks = c(-Inf, 1, 3, 6, Inf),
  labels = c("Low contamination", "Moderate contamination", "Considerable contamination", "Very high contamination"),
  right = FALSE
)

summary_table <- data.frame(
  Sample = data$Number,
  Cd = cd_values,
  Mean_CF = mean_cf,
  Category = as.character(category),
  stringsAsFactors = FALSE
)

category_counts <- as.data.frame(table(summary_table$Category))
names(category_counts) <- c("Category", "Count")

cat("CONTAMINATION DEGREE (Cd)\n\n")
cat("Reference limits used:\n")
print(reference_limits)
cat("\nSummary statistics:\n")
print(summary(cd_values))
cat("\nCategory counts:\n")
print(category_counts, row.names = FALSE)
cat("\nTop samples by Cd:\n")
print(summary_table[order(summary_table$Cd, decreasing = TRUE), ][1:5, ], row.names = FALSE)
cat("\nInterpretation:\n")
cat("- Cd below 1 indicates low contamination; 1-3 moderate; 3-6 considerable; 6 or above very high contamination.\n")
cat(sprintf("- In this dataset, %d of %d samples are in the very high contamination class.\n",
            sum(summary_table$Category == "Very high contamination", na.rm = TRUE),
            nrow(summary_table)))
cat("- Samples with the largest Cd values are the main contributors to overall contamination burden.\n")

# Save results
write.csv(summary_table, "22_contamination_degree_values.csv", row.names = FALSE)
write.csv(category_counts, "22_contamination_degree_category_counts.csv", row.names = FALSE)

# Plot
plot_df <- summary_table
plot_df$Sample <- factor(plot_df$Sample, levels = plot_df$Sample[order(plot_df$Cd)])

cd_plot <- ggplot(plot_df, aes(x = Sample, y = Cd, fill = Cd)) +
  geom_col(color = "grey25", width = 0.8) +
  geom_hline(yintercept = 1, color = "blue", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 3, color = "orange", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 6, color = "red", linetype = "dashed", linewidth = 0.9) +
  scale_fill_gradientn(colors = c("#3f007d", "#54278f", "#fdae61", "#d7301f")) +
  labs(
    title = "Contamination Indices and Risk Assessment",
    subtitle = "Contamination Degree",
    x = "Sample",
    y = "Contamination Degree (Cd)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold")
  )

plot_file <- "22_contamination_degree.png"
ggsave(plot_file, cd_plot, width = 12, height = 7, dpi = 300)
print(cd_plot)
cat(sprintf("\nPlot saved as: %s\n", plot_file))
