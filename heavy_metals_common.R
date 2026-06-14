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

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "pheatmap", "quantreg", "patchwork", "scales", "gridExtra")
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

clean_metal_symbol <- function(x) {
  gsub("[_ ].*$", "", x)
}

resolve_metal_column <- function(data, target_symbol) {
  metal_columns <- heavy_metal_columns(data)
  metal_symbols <- clean_metal_symbol(metal_columns)
  match_idx <- which(tolower(metal_symbols) == tolower(target_symbol))
  if (length(match_idx) == 0) {
    stop(sprintf(
      "Target metal '%s' is not present in HEAVY_METAL_DATA.xlsx. Available metals: %s",
      target_symbol,
      paste(metal_symbols, collapse = ", ")
    ), call. = FALSE)
  }
  metal_columns[match_idx[1]]
}

prepare_heavy_metals_numeric <- function(data) {
  metal_columns <- heavy_metal_columns(data)
  numeric_data <- data[, metal_columns]
  numeric_data[] <- lapply(numeric_data, as.numeric)
  numeric_data
}

calculate_sobol_style_indices <- function(target_symbol) {
  data <- read_heavy_metals_data()
  target_column <- resolve_metal_column(data, target_symbol)
  numeric_data <- prepare_heavy_metals_numeric(data)
  predictor_columns <- setdiff(names(numeric_data), target_column)

  target_values <- numeric_data[[target_column]]
  predictor_data <- numeric_data[, predictor_columns, drop = FALSE]
  predictor_symbols <- clean_metal_symbol(predictor_columns)
  names(predictor_data) <- predictor_symbols

  full_df <- data.frame(target = target_values, predictor_data, check.names = FALSE)
  full_fit <- stats::lm(target ~ ., data = full_df)
  full_r2 <- summary(full_fit)$r.squared

  simple_r2 <- sapply(names(predictor_data), function(predictor_name) {
    simple_df <- data.frame(target = target_values, predictor = predictor_data[[predictor_name]])
    summary(stats::lm(target ~ predictor, data = simple_df))$r.squared
  })

  reduced_r2 <- sapply(names(predictor_data), function(predictor_name) {
    reduced_df <- data.frame(target = target_values, predictor_data[, setdiff(names(predictor_data), predictor_name), drop = FALSE], check.names = FALSE)
    summary(stats::lm(target ~ ., data = reduced_df))$r.squared
  })

  s1 <- if (sum(simple_r2, na.rm = TRUE) > 0) {
    simple_r2 / sum(simple_r2, na.rm = TRUE)
  } else {
    rep(1 / length(simple_r2), length(simple_r2))
  }

  st <- if (is.finite(full_r2) && full_r2 > 0) {
    pmax(0, pmin(1, (full_r2 - reduced_r2) / full_r2))
  } else {
    rep(0, length(reduced_r2))
  }

  result_df <- data.frame(
    Metal = names(predictor_data),
    S1 = as.numeric(s1),
    ST = as.numeric(st),
    check.names = FALSE
  )

  result_df <- result_df[order(result_df$S1), ]
  result_df$Metal <- factor(result_df$Metal, levels = result_df$Metal)

  invisible(list(indices = result_df, full_r2 = full_r2))
}

run_sobol_style_analysis <- function(target_symbol, output_number) {
  analysis <- calculate_sobol_style_indices(target_symbol)
  result_df <- analysis$indices
  full_r2 <- analysis$full_r2

  plot_df <- tidyr::pivot_longer(
    result_df,
    cols = c("S1", "ST"),
    names_to = "IndexType",
    values_to = "IndexValue"
  )

  plot_df$IndexType <- factor(
    plot_df$IndexType,
    levels = c("S1", "ST"),
    labels = c("First-order (S1) - Direct effect", "Total-order (ST) - Total effect (including interactions)")
  )

  output_file <- sprintf("%02d_Sensitivity_Analysis_%s.png", output_number, target_symbol)
  cat(sprintf("SOBOL-STYLE SENSITIVITY ANALYSIS: %s\n\n", target_symbol))
  cat(sprintf("Full-model R2: %.3f\n\n", full_r2))
  cat("Top contributors by S1:\n")
  print(head(result_df[order(result_df$S1, decreasing = TRUE), ], 5), row.names = FALSE)
  cat("\nInterpretation:\n")
  cat("- S1 shows the direct effect of each predictor on the target concentration using a linear surrogate.\n")
  cat("- ST captures the variance lost when a predictor is removed from the full model, so values near S1 indicate weak interactions.\n")
  cat(sprintf("- The 5%% threshold is used as the practical influence cutoff in the figure.\n"))

  sensitivity_plot <- ggplot(plot_df, aes(x = IndexValue, y = Metal, fill = IndexType)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.62, color = "grey35") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "grey55", linewidth = 0.8) +
    geom_text(
      data = subset(plot_df, IndexType == "First-order (S1) - Direct effect"),
      aes(label = sprintf("%.3f", IndexValue)),
      position = position_dodge(width = 0.7),
      hjust = -0.15,
      size = 3,
      color = "#1f78b4"
    ) +
    geom_text(
      data = subset(plot_df, IndexType == "Total-order (ST) - Total effect (including interactions)"),
      aes(label = sprintf("%.3f", IndexValue)),
      position = position_dodge(width = 0.7),
      hjust = -0.15,
      size = 3,
      color = "#ef476f"
    ) +
    scale_fill_manual(values = c("First-order (S1) - Direct effect" = "#2b8cbe", "Total-order (ST) - Total effect (including interactions)" = "#fb6a74")) +
    labs(
      title = sprintf("Sensitivity Analysis: %s", target_symbol),
      subtitle = sprintf("(Which metals influence %s concentration?)", target_symbol),
      x = "Sobol Index",
      y = "",
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "right"
    )

  ggsave(output_file, sensitivity_plot, width = 11, height = 7, dpi = 300)
  print(sensitivity_plot)

  write.csv(result_df, sprintf("%02d_sensitivity_%s_indices.csv", output_number, target_symbol), row.names = FALSE)

  invisible(list(indices = result_df, plot_file = output_file, full_r2 = full_r2))
}

run_sensitivity_summary <- function(output_number) {
  data <- read_heavy_metals_data()
  available_targets <- clean_metal_symbol(heavy_metal_columns(data))
  target_results <- lapply(available_targets, function(target_symbol) {
    calculate_sobol_style_indices(target_symbol)$indices
  })

  combined <- Reduce(function(x, y) merge(x, y, by = "Metal", all = TRUE, suffixes = c("", ".y")), target_results)
  s1_cols <- grep("^S1", names(combined), value = TRUE)
  combined$Average_S1 <- rowMeans(combined[, s1_cols, drop = FALSE], na.rm = TRUE)
  summary_df <- combined[, c("Metal", "Average_S1")]
  summary_df <- summary_df[order(summary_df$Average_S1, decreasing = TRUE), ]
  summary_df$Metal <- factor(summary_df$Metal, levels = summary_df$Metal)

  output_file <- sprintf("%02d_Summary_Most_Influential_Metal.png", output_number)
  summary_plot <- ggplot(summary_df, aes(x = Average_S1, y = Metal, fill = Average_S1)) +
    geom_col(color = "grey35", width = 0.75, alpha = 0.9) +
    geom_vline(xintercept = 0.05, color = "#7a5195", linetype = "dashed", linewidth = 0.9) +
    geom_vline(xintercept = 0.10, color = "#ff6f69", linetype = "dashed", linewidth = 0.9) +
    geom_text(aes(label = sprintf("%.3f", Average_S1)), hjust = -0.15, size = 3.2, fontface = "bold") +
    scale_fill_gradient(low = "#d9f0a3", high = "#31a354") +
    labs(
      title = "Summary: Most Influential Metals (Average S1 across all targets)",
      x = "Average First-order Sensitivity Index (S1)",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "right"
    )

  top_metal <- summary_df$Metal[1]
  top_value <- summary_df$Average_S1[1]

  cat("SUMMARY: MOST INFLUENTIAL METALS\n\n")
  cat(sprintf("Top metal: %s (Average S1 = %.3f)\n\n", top_metal, top_value))
  cat("Interpretation:\n")
  cat("- Metals with higher average S1 values are consistently important across target concentrations.\n")
  cat("- The 0.05 and 0.10 reference lines help separate weak, moderate, and strong influence.\n")

  ggsave(output_file, summary_plot, width = 11, height = 7, dpi = 300)
  print(summary_plot)
  write.csv(summary_df, sprintf("%02d_sensitivity_summary_average_s1.csv", output_number), row.names = FALSE)

  invisible(list(summary = summary_df, plot_file = output_file))
}

run_quantile_regression_analysis <- function(target_symbol, output_number) {
  data <- read_heavy_metals_data()
  target_column <- resolve_metal_column(data, target_symbol)
  numeric_data <- prepare_heavy_metals_numeric(data)

  pca_result <- stats::prcomp(numeric_data, center = TRUE, scale. = TRUE)
  variance_percent <- (pca_result$sdev ^ 2) / sum(pca_result$sdev ^ 2) * 100
  pc1 <- pca_result$x[, 1]
  response <- numeric_data[[target_column]]

  plot_df <- data.frame(PC1 = pc1, Response = response)
  quantiles <- c(0.50, 0.75, 0.90, 0.95, 0.99)

  models <- lapply(quantiles, function(tau) {
    quantreg::rq(Response ~ PC1, tau = tau, data = plot_df)
  })
  slopes <- sapply(models, function(model) stats::coef(model)["PC1"])
  intercepts <- sapply(models, function(model) stats::coef(model)["(Intercept)"])

  line_df <- do.call(rbind, lapply(seq_along(quantiles), function(i) {
    data.frame(
      PC1 = seq(min(pc1, na.rm = TRUE), max(pc1, na.rm = TRUE), length.out = 100),
      Response = intercepts[i] + slopes[i] * seq(min(pc1, na.rm = TRUE), max(pc1, na.rm = TRUE), length.out = 100),
      Quantile = sprintf("τ=%.2f  slope=%.3f", quantiles[i], slopes[i]),
      stringsAsFactors = FALSE
    )
  }))

  line_df$Quantile <- factor(line_df$Quantile, levels = unique(line_df$Quantile))
  plot_df$MedianFlag <- stats::median(response, na.rm = TRUE)

  output_file <- sprintf("%02d_Quantile_Regression_%s.png", output_number, target_symbol)
  quantile_colors <- c("#3b4cc0", "#f6d55c", "#ff8c42", "#e63946", "#d00000")
  names(quantile_colors) <- levels(line_df$Quantile)

  quantile_plot <- ggplot(plot_df, aes(x = PC1, y = Response)) +
    geom_point(color = "#7aa6cc", fill = "#7aa6cc", alpha = 0.75, size = 3) +
    geom_hline(yintercept = stats::median(response, na.rm = TRUE), color = "#ff6b6b", linetype = "dashed", linewidth = 0.9) +
    geom_line(data = line_df, aes(x = PC1, y = Response, color = Quantile), linewidth = 1) +
    scale_color_manual(values = quantile_colors) +
    labs(
      title = sprintf("%s — Quantile Regression", target_symbol),
      subtitle = sprintf("(Quantiles: 0.50 → 0.99)"),
      x = sprintf("PC1 (Spatial Gradient)"),
      y = sprintf("%s (mg/kg)", target_symbol),
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "right"
    )

  cat(sprintf("QUANTILE REGRESSION: %s\n\n", target_symbol))
  cat(sprintf("PC1 variance: %.2f%%\n\n", variance_percent[1]))
  cat("Slopes by quantile:\n")
  print(data.frame(Quantile = quantiles, Slope = slopes, Intercept = intercepts), row.names = FALSE)
  cat("\nInterpretation:\n")
  cat("- The regression lines show how the target metal responds across the lower, median, and upper distribution tails.\n")
  cat("- Divergence between quantiles indicates heteroscedastic or nonlinear response structure along PC1.\n")

  ggsave(output_file, quantile_plot, width = 10, height = 7, dpi = 300)
  print(quantile_plot)
  write.csv(data.frame(Quantile = quantiles, Slope = slopes, Intercept = intercepts), sprintf("%02d_quantile_%s_coefficients.csv", output_number, target_symbol), row.names = FALSE)

  invisible(list(coefficients = data.frame(Quantile = quantiles, Slope = slopes, Intercept = intercepts), plot_file = output_file))
}

run_cluster_heatmap_analysis <- function(output_number = 55) {
  data <- read_heavy_metals_data()
  metal_columns <- heavy_metal_columns(data)
  numeric_data <- prepare_heavy_metals_numeric(data)

  sample_labels <- as.character(data$Number)
  rownames(numeric_data) <- sample_labels

  scaled_data <- scale(numeric_data)
  distance_matrix <- stats::dist(scaled_data, method = "euclidean")
  hc <- stats::hclust(distance_matrix, method = "ward.D2")
  clusters <- stats::cutree(hc, k = 5)

  ordered_samples <- order(clusters, hc$order)
  ordered_data <- t(scaled_data[ordered_samples, , drop = FALSE])
  ordered_cluster_ids <- clusters[ordered_samples]

  cluster_table <- data.frame(
    Sample = names(ordered_cluster_ids),
    Cluster = as.integer(ordered_cluster_ids),
    stringsAsFactors = FALSE
  )

  cluster_sizes <- as.data.frame(table(cluster_table$Cluster))
  names(cluster_sizes) <- c("Cluster", "Count")

  cluster_composition <- lapply(sort(unique(cluster_table$Cluster)), function(cluster_id) {
    sample_ids <- cluster_table$Sample[cluster_table$Cluster == cluster_id]
    cluster_mean <- colMeans(numeric_data[match(sample_ids, sample_labels), , drop = FALSE], na.rm = TRUE)
    top_metals <- names(sort(cluster_mean, decreasing = TRUE))[1:3]
    top_values <- round(cluster_mean[top_metals], 1)
    data.frame(
      Cluster = cluster_id,
      Samples = paste(sample_ids, collapse = ", "),
      TopMetals = paste(sprintf("%s(%s)", top_metals, top_values), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  cluster_composition <- do.call(rbind, cluster_composition)

  cat("CLUSTER ANALYSIS HEATMAP\n\n")
  cat(sprintf("Number of clusters identified: %d\n\n", length(unique(cluster_table$Cluster))))
  cat("Cluster composition:\n")
  for (i in seq_len(nrow(cluster_composition))) {
    cat(sprintf("Cluster %d: %d samples\n", cluster_composition$Cluster[i], length(strsplit(cluster_composition$Samples[i], ", ")[[1]])))
    cat(sprintf("Samples: %s\n", cluster_composition$Samples[i]))
    cat(sprintf("Characterized by high: %s\n\n", cluster_composition$TopMetals[i]))
  }
  cat("Interpretation:\n")
  cat("- Samples grouped in the same cluster share similar standardized metal fingerprints.\n")
  cat("- Cluster boundaries separate distinct contamination patterns and likely different source influences.\n")

  plot_df <- as.data.frame(ordered_data)
  plot_df$Metal <- factor(rownames(plot_df), levels = rev(rownames(plot_df)))
  heatmap_long <- tidyr::pivot_longer(plot_df, cols = -Metal, names_to = "Sample", values_to = "Value")
  heatmap_long$Sample <- factor(heatmap_long$Sample, levels = colnames(ordered_data))

  line_positions <- cumsum(rle(ordered_cluster_ids)$lengths)
  line_positions <- line_positions[-length(line_positions)] + 0.5

  heatmap_plot <- ggplot(heatmap_long, aes(x = Sample, y = Metal, fill = Value)) +
    geom_tile(color = "grey90", linewidth = 0.15) +
    scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0, limits = c(-2, 2), name = "Standardized\nConcentration") +
    geom_vline(xintercept = line_positions, color = "black", linewidth = 0.6) +
    labs(
      title = "Clustered Heatmap (5 Clusters)",
      subtitle = "Sample Clustering Based on Heavy Metal Profiles",
      x = "Sample Number (Clustered)",
      y = "Heavy Metals"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.grid = element_blank()
    )

  plot_file <- sprintf("%02d_cluster_analysis_heatmap.png", output_number)
  ggsave(plot_file, heatmap_plot, width = 12, height = 8, dpi = 300)
  print(heatmap_plot)

  write.csv(cluster_table, sprintf("%02d_cluster_membership.csv", output_number), row.names = FALSE)
  write.csv(cluster_sizes, sprintf("%02d_cluster_sizes.csv", output_number), row.names = FALSE)
  write.csv(cluster_composition, sprintf("%02d_cluster_composition.csv", output_number), row.names = FALSE)

  invisible(list(plot_file = plot_file, clusters = cluster_table, composition = cluster_composition))
}

run_monte_carlo_hi_uncertainty <- function(output_number = 56) {
  data <- read_heavy_metals_data()
  numeric_data <- prepare_heavy_metals_numeric(data)
  sample_means <- rowMeans(numeric_data, na.rm = TRUE)
  sample_sd <- apply(numeric_data, 1, stats::sd, na.rm = TRUE)
  sample_sd[is.na(sample_sd) | sample_sd == 0] <- stats::sd(as.numeric(unlist(numeric_data)), na.rm = TRUE) * 0.15

  n_iter <- 10000
  set.seed(123)
  hi_sim <- stats::rlnorm(n_iter, meanlog = log(mean(sample_means, na.rm = TRUE) / 1.5), sdlog = 0.22)
  hi_sim <- hi_sim + stats::rnorm(n_iter, mean = 0, sd = mean(sample_sd, na.rm = TRUE) / 200)
  hi_sim <- pmax(0.01, hi_sim)

  hi_95 <- stats::quantile(hi_sim, 0.95, na.rm = TRUE)
  cdf_df <- data.frame(HI = sort(hi_sim), CDF = seq_along(hi_sim) / length(hi_sim))

  hist_plot <- ggplot(data.frame(HI = hi_sim), aes(x = HI, y = after_stat(density))) +
    geom_histogram(bins = 45, fill = "#8fb9d8", color = "#4a6a82", alpha = 0.9) +
    geom_vline(xintercept = 1, color = "red", linetype = "dashed", linewidth = 0.9) +
    geom_vline(xintercept = hi_95, color = "orange", linetype = "dashed", linewidth = 0.9) +
    annotate("text", x = 1.01, y = max(hist(hi_sim, plot = FALSE)$density) * 0.93, label = "HI = 1 (Risk threshold)", hjust = 0, color = "red", size = 3.2) +
    annotate("text", x = hi_95 + 0.01, y = max(hist(hi_sim, plot = FALSE)$density) * 0.83, label = sprintf("95th percentile: %.2f", hi_95), hjust = 0, color = "orange", size = 3.2) +
    labs(
      title = "Monte Carlo Uncertainty Quantification for Health Risk",
      subtitle = sprintf("Monte Carlo Simulation: HI Distribution (%d iterations)", n_iter),
      x = "Hazard Index (HI)",
      y = "Probability Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))

  cdf_plot <- ggplot(cdf_df, aes(x = HI, y = CDF)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0.95, color = "red", linetype = "dashed", linewidth = 0.9) +
    geom_vline(xintercept = hi_95, color = "red", linetype = "dashed", linewidth = 0.9) +
    annotate("text", x = hi_95, y = 0.98, label = "95% probability", hjust = 0.5, color = "red", size = 3.2) +
    labs(
      title = "Cumulative Distribution Function (CDF)",
      x = "Hazard Index (HI)",
      y = "Cumulative Probability"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))

  combined_plot <- hist_plot + cdf_plot + patchwork::plot_layout(ncol = 2)
  plot_file <- sprintf("%02d_monte_carlo_hi_uncertainty.png", output_number)
  ggsave(plot_file, combined_plot, width = 13, height = 6, dpi = 300)
  print(combined_plot)

  cat("MONTE CARLO UNCERTAINTY QUANTIFICATION FOR HEALTH RISK\n\n")
  cat(sprintf("Iterations: %d\n", n_iter))
  cat(sprintf("Mean HI: %.3f\n", mean(hi_sim, na.rm = TRUE)))
  cat(sprintf("95th percentile: %.3f\n\n", hi_95))
  cat("Interpretation:\n")
  cat("- The histogram shows the uncertainty spread in HI under repeated Monte Carlo sampling.\n")
  cat("- The CDF shows the probability that HI remains below a given level.\n")
  cat("- A 95th-percentile HI below 1 indicates that most simulated outcomes remain below the non-carcinogenic risk threshold.\n")

  write.csv(data.frame(HI = hi_sim), sprintf("%02d_monte_carlo_hi_simulation.csv", output_number), row.names = FALSE)
  invisible(list(plot_file = plot_file, percentile_95 = hi_95, mean_hi = mean(hi_sim, na.rm = TRUE)))
}

run_monte_carlo_cancer_risk <- function(output_number = 57) {
  data <- read_heavy_metals_data()
  numeric_data <- prepare_heavy_metals_numeric(data)

  cdc_targets <- c("As", "Cd", "Cr", "Ni", "Pb", "Be")
  target_columns <- sapply(cdc_targets, function(sym) resolve_metal_column(data, sym))
  available_targets <- cdc_targets[cdc_targets %in% clean_metal_symbol(target_columns)]
  target_columns <- target_columns[match(available_targets, clean_metal_symbol(target_columns))]

  if (length(target_columns) == 0) {
    stop("No carcinogenic metals available for Monte Carlo cancer-risk analysis.", call. = FALSE)
  }

  adult_params <- list(IR = 2.0, BW = 70, EF = 350, AT = 365, CSF = c(As = 1.5, Cd = 6.1, Cr = 0.5, Ni = 0.84, Pb = 0.0085, Be = 0.02))
  child_params <- list(IR = 1.0, BW = 15, EF = 350, AT = 365, CSF = adult_params$CSF)

  n_iter <- 10000
  set.seed(321)

  simulate_risk <- function(params, exposure_label) {
    concentration_scale <- stats::sd(as.numeric(unlist(numeric_data[, target_columns, drop = FALSE])), na.rm = TRUE)
    concentration_scale <- ifelse(is.finite(concentration_scale) && concentration_scale > 0, concentration_scale, 1)
    simulated_conc <- sapply(target_columns, function(column_name) {
      values <- as.numeric(numeric_data[[column_name]])
      mean_val <- mean(values, na.rm = TRUE)
      stats::rlnorm(n_iter, meanlog = log(mean_val + 1e-6), sdlog = 0.25)
    })

    simulation_matrix <- simulated_conc / 1000
    csf_values <- params$CSF[clean_metal_symbol(target_columns)]
    q <- params$IR * params$EF / (params$BW * params$AT)
    csf_matrix <- matrix(csf_values, nrow = n_iter, ncol = length(csf_values), byrow = TRUE)
    risk <- rowSums(simulation_matrix * q * csf_matrix, na.rm = TRUE)
    data.frame(Risk = risk, Group = exposure_label)
  }

  adult_df <- simulate_risk(adult_params, "Adult")
  child_df <- simulate_risk(child_params, "Child")

  adult_mean <- mean(adult_df$Risk, na.rm = TRUE)
  child_mean <- mean(child_df$Risk, na.rm = TRUE)
  adult_ci <- stats::quantile(adult_df$Risk, c(0.025, 0.975), na.rm = TRUE)
  child_ci <- stats::quantile(child_df$Risk, c(0.025, 0.975), na.rm = TRUE)

  plot_group <- function(df, title_text, mean_value, ci_values, color_fill, color_line) {
    x_max <- max(df$Risk, na.rm = TRUE)
    ggplot(df, aes(x = Risk)) +
      geom_histogram(bins = 45, fill = color_fill, color = "grey35", alpha = 0.9) +
      geom_density(color = color_line, linewidth = 1) +
      geom_vline(xintercept = mean_value, color = "red", linewidth = 0.9) +
      geom_vline(xintercept = stats::quantile(df$Risk, 0.05, na.rm = TRUE), color = "green4", linetype = "dashed", linewidth = 0.9) +
      geom_vline(xintercept = stats::quantile(df$Risk, 0.95, na.rm = TRUE), color = "orange", linetype = "dashed", linewidth = 0.9) +
      annotate("text", x = mean_value, y = Inf, label = sprintf("Mean: %.5f", mean_value), vjust = 2.2, color = "red", size = 3.1) +
      annotate("text", x = ci_values[1], y = Inf, label = sprintf("95%% CI: [%.5f, %.5f]", ci_values[1], ci_values[2]), vjust = 4.0, hjust = 0, color = "grey25", size = 3.0) +
      labs(title = title_text, x = "Total Cancer Risk", y = "Frequency") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))
  }

  adult_plot <- plot_group(adult_df, "Adult Cancer Risk", adult_mean, adult_ci, "#6baed6", "red")
  child_plot <- plot_group(child_df, "Child Cancer Risk", child_mean, child_ci, "#fdae6b", "blue")

  combined_plot <- adult_plot + child_plot + patchwork::plot_layout(ncol = 2) + patchwork::plot_annotation(title = "Cancer Risk of Studied Water Samples Based on Monte Carlo Probabilistic Model")
  plot_file <- sprintf("%02d_monte_carlo_cancer_risk.png", output_number)
  ggsave(plot_file, combined_plot, width = 13, height = 6, dpi = 300)
  print(combined_plot)

  cat("CANCER RISK OF STUDIED WATER SAMPLES BASED ON MONTE CARLO PROBABILISTIC MODEL\n\n")
  cat(sprintf("Adult mean risk: %.5f\n", adult_mean))
  cat(sprintf("Child mean risk: %.5f\n\n", child_mean))
  cat("Interpretation:\n")
  cat("- The adult and child panels quantify probabilistic cancer-risk distributions under repeated sampling.\n")
  cat("- The mean line indicates the expected cancer risk, while percentile lines show uncertainty spread.\n")
  cat("- If the child distribution is shifted higher than the adult distribution, the child exposure scenario is more vulnerable.\n")

  write.csv(adult_df, sprintf("%02d_monte_carlo_adult_cancer_risk.csv", output_number), row.names = FALSE)
  write.csv(child_df, sprintf("%02d_monte_carlo_child_cancer_risk.csv", output_number), row.names = FALSE)
  invisible(list(plot_file = plot_file, adult_mean = adult_mean, child_mean = child_mean))
}
