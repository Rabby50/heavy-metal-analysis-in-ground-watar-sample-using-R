# Task 21: Contamination Indices and Risk Assessment
# HPI Values (Heavy Metal Pollution Index)
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

# Reference limits and weights
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
weights <- 1 / reference_limits

# HPI calculation
qi_matrix <- sweep(as.matrix(concentrations), 2, reference_limits, "/") * 100
hpi_values <- as.numeric(qi_matrix %*% weights / sum(weights))

# Classification
category <- cut(
  hpi_values,
  breaks = c(-Inf, 100, 200, Inf),
  labels = c("Low risk", "Moderate risk", "High risk"),
  right = FALSE
)

summary_table <- data.frame(
  Sample = data$Number,
  HPI = hpi_values,
  Category = as.character(category),
  stringsAsFactors = FALSE
)

category_counts <- as.data.frame(table(summary_table$Category))
names(category_counts) <- c("Category", "Count")

cat("HEAVY METAL POLLUTION INDEX (HPI)\n\n")
cat("Reference limits used:\n")
print(reference_limits)
cat("\nSummary statistics:\n")
print(summary(hpi_values))
cat("\nCategory counts:\n")
print(category_counts, row.names = FALSE)
cat("\nTop samples by HPI:\n")
print(summary_table[order(summary_table$HPI, decreasing = TRUE), ][1:5, ], row.names = FALSE)
cat("\nInterpretation:\n")
cat("- HPI below 100 indicates low risk, 100-200 indicates moderate risk, and 200 or more indicates high risk.\n")
cat(sprintf("- In this dataset, %d of %d samples fall in the high-risk class, while %d remain below 100.\n",
            sum(summary_table$Category == "High risk", na.rm = TRUE),
            nrow(summary_table),
            sum(summary_table$Category == "Low risk", na.rm = TRUE)))
cat("- The highest HPI sample should be treated as the most impacted location for management priority.\n")

# Save results
write.csv(summary_table, "21_hpi_values.csv", row.names = FALSE)
write.csv(category_counts, "21_hpi_category_counts.csv", row.names = FALSE)

# Plot
plot_df <- summary_table
plot_df$Sample <- factor(plot_df$Sample, levels = plot_df$Sample[order(plot_df$HPI)])

hpi_plot <- ggplot(plot_df, aes(x = Sample, y = HPI, fill = HPI)) +
  geom_col(color = "grey25", width = 0.8) +
  geom_hline(yintercept = 100, color = "orange", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 200, color = "red", linetype = "dashed", linewidth = 0.9) +
  scale_fill_gradientn(colors = c("#542788", "#8073ac", "#fdb863", "#e66101")) +
  labs(
    title = "Contamination Indices and Risk Assessment",
    subtitle = "HPI Values",
    x = "Sample",
    y = "Heavy Metal Pollution Index (HPI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold")
  )

plot_file <- "21_hpi_values.png"
ggsave(plot_file, hpi_plot, width = 12, height = 7, dpi = 300)
print(hpi_plot)
cat(sprintf("\nPlot saved as: %s\n", plot_file))
