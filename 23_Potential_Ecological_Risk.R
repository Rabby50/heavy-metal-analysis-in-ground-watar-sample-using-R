# Task 23: Contamination Indices and Risk Assessment
# Potential Ecological Risk (RI)
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

# Reference limits and toxic response factors
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

toxic_response <- c(
  As_75 = 10,
  Se_82 = 3,
  Pb_208 = 5,
  Cd_111 = 30,
  Cr_52 = 2,
  Ni_60 = 5,
  Cu_63 = 5,
  Zn_66 = 1,
  Hg_202 = 40,
  Co_59 = 5,
  Be_9 = 2,
  V_51 = 2,
  Fe_57 = 1,
  Mn_55 = 1
)
toxic_response <- toxic_response[metal_columns]

# Ecological risk calculation
cf_matrix <- sweep(as.matrix(concentrations), 2, reference_limits, "/")
eri_matrix <- sweep(cf_matrix, 2, toxic_response, "*")
ri_values <- rowSums(eri_matrix, na.rm = TRUE)
mean_eri <- rowMeans(eri_matrix, na.rm = TRUE)

category <- cut(
  ri_values,
  breaks = c(-Inf, 150, 300, 600, Inf),
  labels = c("Low risk", "Moderate risk", "Considerable risk", "Very high risk"),
  right = FALSE
)

summary_table <- data.frame(
  Sample = data$Number,
  RI = ri_values,
  Mean_ERI = mean_eri,
  Category = as.character(category),
  stringsAsFactors = FALSE
)

category_counts <- as.data.frame(table(summary_table$Category))
names(category_counts) <- c("Category", "Count")

cat("POTENTIAL ECOLOGICAL RISK (RI)\n\n")
cat("Reference limits used:\n")
print(reference_limits)
cat("\nToxic response factors used:\n")
print(toxic_response)
cat("\nSummary statistics:\n")
print(summary(ri_values))
cat("\nCategory counts:\n")
print(category_counts, row.names = FALSE)
cat("\nTop samples by RI:\n")
print(summary_table[order(summary_table$RI, decreasing = TRUE), ][1:5, ], row.names = FALSE)
cat("\nInterpretation:\n")
cat("- RI below 150 indicates low ecological risk; 150-300 moderate; 300-600 considerable; 600 or above very high risk.\n")
cat(sprintf("- In this dataset, %d of %d samples fall in the low-risk class, and %d reach the considerable-risk class.\n",
            sum(summary_table$Category == "Low risk", na.rm = TRUE),
            nrow(summary_table),
            sum(summary_table$Category == "Considerable risk", na.rm = TRUE)))
cat("- Cadmium, mercury, and arsenic are the main contributors because of their higher toxic response factors.\n")

# Save results
write.csv(summary_table, "23_potential_ecological_risk_values.csv", row.names = FALSE)
write.csv(category_counts, "23_potential_ecological_risk_category_counts.csv", row.names = FALSE)

# Plot
plot_df <- summary_table
plot_df$Sample <- factor(plot_df$Sample, levels = plot_df$Sample[order(plot_df$RI)])

ri_plot <- ggplot(plot_df, aes(x = Sample, y = RI, fill = RI)) +
  geom_col(color = "grey25", width = 0.8) +
  geom_hline(yintercept = 150, color = "orange", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 300, color = "red", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 600, color = "darkred", linetype = "dashed", linewidth = 0.9) +
  scale_fill_gradientn(colors = c("#f7f7f7", "#fcbba1", "#fb6a4a", "#cb181d")) +
  labs(
    title = "Contamination Indices and Risk Assessment",
    subtitle = "Potential Ecological Risk",
    x = "Sample",
    y = "Ecological Risk Index (RI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold")
  )

plot_file <- "23_potential_ecological_risk.png"
ggsave(plot_file, ri_plot, width = 12, height = 7, dpi = 300)
print(ri_plot)
cat(sprintf("\nPlot saved as: %s\n", plot_file))
