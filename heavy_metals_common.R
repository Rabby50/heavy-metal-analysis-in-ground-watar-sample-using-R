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

data_file <- "HEAVY_METAL_DATA.xlsx"

read_heavy_metals_data <- function() {
  data <- readxl::read_excel(data_file, sheet = 1)
  data
}

heavy_metal_columns <- function(data) {
  setdiff(names(data), "Number")
}

metal_guidelines <- c(
  As = 10,
  Se = 40,
  Pb = 10,
  Cd = 3,
  Cr = 50,
  Ni = 70,
  Cu = 2000,
  Zn = 3000,
  Hg = 6,
  Co = NA_real_,
  Be = 4,
  V = NA_real_,
  Fe = 300,
  Mn = 400
)

format_count <- function(value, total) {
  if (is.na(value)) {
    return("Not available")
  }
  sprintf("%d / %d (%.1f%%)", value, total, 100 * value / total)
}

print_metal_statistics <- function(data, metal_name) {
  values <- as.numeric(data[[metal_name]])
  guideline <- metal_guidelines[[metal_name]]
  exceed_count <- if (!is.na(guideline)) sum(values > guideline, na.rm = TRUE) else NA_integer_

  stats_table <- data.frame(
    Parameter = c(
      paste0("Mean (", metal_name, ")"),
      paste0("Median (", metal_name, ")"),
      paste0("Standard Deviation (", metal_name, ")"),
      paste0("Minimum (", metal_name, ")"),
      paste0("Maximum (", metal_name, ")"),
      "Exceeding WHO guideline"
    ),
    Value = c(
      round(mean(values, na.rm = TRUE), 4),
      round(stats::median(values, na.rm = TRUE), 4),
      round(stats::sd(values, na.rm = TRUE), 4),
      round(min(values, na.rm = TRUE), 4),
      round(max(values, na.rm = TRUE), 4),
      format_count(exceed_count, length(values))
    ),
    check.names = FALSE
  )

  cat(sprintf("STATISTICS FOR %s (n=%d)\n\n", metal_name, length(values)))
  print(stats_table, row.names = FALSE)
  cat("\n")

  output_file <- paste0(metal_name, "_statistics.txt")
  writeLines(capture.output(print(stats_table, row.names = FALSE)), output_file)

  invisible(stats_table)
}

plot_metal_distribution <- function(data, metal_name) {
  values <- as.numeric(data[[metal_name]])
  guideline <- metal_guidelines[[metal_name]]
  stats_table <- print_metal_statistics(data, metal_name)

  plot_title <- sprintf("Distribution of %s in Ground Water Samples (n=%d)", metal_name, nrow(data))
  plot_file <- paste0(metal_name, "_distribution.png")

  plot_data <- data.frame(Value = values)

  histogram <- ggplot(plot_data, aes(x = Value)) +
    geom_histogram(bins = 10, fill = "#e57373", color = "#7f2d2d", alpha = 0.85) +
    geom_vline(xintercept = mean(values, na.rm = TRUE), color = "blue", linewidth = 1) +
    geom_vline(xintercept = stats::median(values, na.rm = TRUE), color = "black", linetype = "dotted", linewidth = 1) +
    { if (!is.na(guideline)) geom_vline(xintercept = guideline, color = "red", linetype = "dashed", linewidth = 1) } +
    scale_x_log10() +
    labs(
      title = plot_title,
      x = sprintf("%s Concentration", metal_name),
      y = "Frequency"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold")
    )

  ggsave(plot_file, histogram, width = 10, height = 7, dpi = 300)
  print(histogram)

  invisible(list(stats = stats_table, plot_file = plot_file))
}

plot_correlation_matrix <- function(data) {
  metal_columns <- heavy_metal_columns(data)
  numeric_data <- data[, metal_columns]
  numeric_data[] <- lapply(numeric_data, as.numeric)

  cat(sprintf("DATA OVERVIEW (n=%d)\n\n", nrow(data)))
  print(summary(numeric_data))
  cat("\nBASIC STATISTICS\n\n")
  print(round(sapply(numeric_data, sd, na.rm = TRUE), 4))
  cat("\n")

  correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")
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

  write.csv(correlation_matrix, "heavy_metal_correlation_matrix.csv", row.names = TRUE)
  invisible(correlation_matrix)
}
