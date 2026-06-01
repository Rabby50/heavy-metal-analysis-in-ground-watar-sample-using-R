# Distribution Analysis: Vanadium (V_51)
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

required_packages <- c("readxl", "ggplot2")
install_and_load(required_packages)

# Load data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)

# Extract V values
metal_name <- "V_51"
metal_short <- "V"
values <- as.numeric(data[[metal_name]])

# WHO guideline for V (NA - not specified)
guideline <- NA_real_

# Calculate statistics
mean_val <- mean(values, na.rm = TRUE)
median_val <- stats::median(values, na.rm = TRUE)
sd_val <- stats::sd(values, na.rm = TRUE)
min_val <- min(values, na.rm = TRUE)
max_val <- max(values, na.rm = TRUE)
exceed_count <- if (is.na(guideline)) NA_integer_ else sum(values > guideline, na.rm = TRUE)

# Print statistics
cat("STATISTICS FOR", metal_short, "(n=", length(values), ")\n\n", sep = "")
stats_table <- data.frame(
  Parameter = c(
    paste0("Mean (", metal_short, ") (ppb)"),
    paste0("Median (", metal_short, ") (ppb)"),
    paste0("Standard Deviation (", metal_short, ") (ppb)"),
    paste0("Minimum (", metal_short, ") (ppb)"),
    paste0("Maximum (", metal_short, ") (ppb)"),
    "Exceeding WHO guideline"
  ),
  Value = c(
    round(mean_val, 4),
    round(median_val, 4),
    round(sd_val, 4),
    round(min_val, 4),
    round(max_val, 4),
    "Not available (no WHO guideline)"
  ),
  check.names = FALSE
)
print(stats_table, row.names = FALSE)
cat("\n")

# Create histogram
plot_data <- data.frame(Value = values)

histogram <- ggplot(plot_data, aes(x = Value)) +
  geom_histogram(bins = 10, fill = "#e57373", color = "#7f2d2d", alpha = 0.85) +
  geom_vline(xintercept = mean_val, color = "blue", linewidth = 1, label = paste0("Mean: ", round(mean_val, 2))) +
  geom_vline(xintercept = median_val, color = "black", linetype = "dotted", linewidth = 1, label = paste0("Median: ", round(median_val, 2))) +
  scale_x_log10() +
  labs(
    title = paste0("Distribution of ", metal_short, " in Ground Water Samples (n=", nrow(data), ")"),
    x = "V Concentration (ppb)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# Save plot
plot_file <- paste0(metal_short, "_distribution.png")
ggsave(plot_file, histogram, width = 10, height = 7, dpi = 300)
print(histogram)

cat("\nPlot saved as:", plot_file, "\n")
