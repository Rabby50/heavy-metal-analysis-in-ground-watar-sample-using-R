# Task 24: (a) Geoaccumulation Index (Igeo) Heatmap
#          (b) Source Apportionment: Factor Analysis Loadings
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

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "patchwork")
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

# Use target order from demo figure when available
preferred_order <- c("As", "B", "Ba", "Cr", "Cu", "Fe", "Mn", "Pb", "Sn", "Sr", "Ti", "Zn")
available_idx <- which(metal_symbols %in% preferred_order)

if (length(available_idx) < 6) {
  # Fallback: use up to 12 metals with highest variance
  var_rank <- order(apply(metal_data, 2, stats::var, na.rm = TRUE), decreasing = TRUE)
  selected_idx <- var_rank[1:min(12, ncol(metal_data))]
} else {
  selected_symbols <- preferred_order[preferred_order %in% metal_symbols]
  selected_idx <- unlist(lapply(selected_symbols, function(sym) which(metal_symbols == sym)[1]))
}

selected_data <- metal_data[, selected_idx, drop = FALSE]
selected_symbols <- metal_symbols[selected_idx]

# Ensure sample index exists in numeric order
sample_id <- if ("Number" %in% names(data)) data$Number else seq_len(nrow(data))
if (is.character(sample_id)) {
  sample_num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", sample_id)))
  if (all(!is.na(sample_num))) {
    sample_id <- sample_num
  }
}

# (a) Igeo = log2(Cn / (1.5 * Bn)); Bn estimated as median background per metal
background <- apply(selected_data, 2, stats::median, na.rm = TRUE)
igeo_matrix <- sweep(as.matrix(selected_data), 2, 1.5 * background, "/")
igeo_matrix <- log2(igeo_matrix)

igeo_df <- as.data.frame(igeo_matrix)
igeo_df$Sample <- sample_id
igeo_long <- igeo_df |>
  tidyr::pivot_longer(-Sample, names_to = "MetalColumn", values_to = "Igeo")

igeo_long$Metal <- factor(
  clean_metal_symbol(igeo_long$MetalColumn),
  levels = rev(selected_symbols)
)
igeo_long$Sample <- factor(igeo_long$Sample, levels = sort(unique(sample_id)))

heatmap_plot <- ggplot(igeo_long, aes(x = Sample, y = Metal, fill = Igeo)) +
  geom_tile(color = "grey90", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#084081",
    mid = "#f7f7f7",
    high = "#b30000",
    midpoint = 0,
    limits = c(-3, 5),
    oob = scales::squish
  ) +
  labs(
    title = "(a) Geoaccumulation Index (Igeo) Heatmap",
    x = "Sample Number",
    y = "Heavy Metal",
    fill = "Igeo Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.0),
    axis.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# (b) Factor analysis loadings
scaled_selected <- scale(selected_data)
fa_fit <- tryCatch(
  stats::factanal(scaled_selected, factors = 3, rotation = "varimax"),
  error = function(e) NULL
)

if (is.null(fa_fit)) {
  # Fallback: derive pseudo-loadings from PCA if factor analysis is singular
  pca_tmp <- stats::prcomp(scaled_selected, center = TRUE, scale. = FALSE)
  loadings_mat <- pca_tmp$rotation[, 1:3, drop = FALSE]
  colnames(loadings_mat) <- c("Factor1", "Factor2", "Factor3")
} else {
  loadings_mat <- as.matrix(fa_fit$loadings[, 1:3])
  colnames(loadings_mat) <- c("Factor1", "Factor2", "Factor3")
}

loadings_df <- as.data.frame(loadings_mat)
loadings_df$Metal <- clean_metal_symbol(rownames(loadings_mat))
loadings_long <- loadings_df |>
  tidyr::pivot_longer(cols = starts_with("Factor"), names_to = "Factor", values_to = "Loading")

loadings_long$Metal <- factor(loadings_long$Metal, levels = selected_symbols)
loadings_long$Label <- ifelse(abs(loadings_long$Loading) >= 0.5, sprintf("%.2f", loadings_long$Loading), "")

loading_plot <- ggplot(loadings_long, aes(x = Metal, y = Loading, fill = Factor)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.72, alpha = 0.85, color = "grey35") +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = -0.5, color = "red", linetype = "dashed", linewidth = 0.7) +
  geom_text(
    aes(label = Label),
    position = position_dodge(width = 0.75),
    vjust = ifelse(loadings_long$Loading >= 0, -0.25, 1.2),
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("Factor1" = "#3182bd", "Factor2" = "#f28e2b", "Factor3" = "#4daf4a")) +
  labs(
    title = "(b) Source Apportionment: Factor Analysis Loadings",
    x = "Heavy Metal",
    y = "Factor Loading",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.0),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

combined_plot <- heatmap_plot / loading_plot + patchwork::plot_layout(heights = c(1.1, 1))

plot_file <- "24_geoaccumulation_and_factor_loadings.png"
ggsave(plot_file, combined_plot, width = 14, height = 9, dpi = 300)
print(combined_plot)

# Save analytical tables
write.csv(igeo_matrix, "24_igeo_values.csv", row.names = FALSE)
write.csv(loadings_df, "24_factor_loadings.csv", row.names = FALSE)

cat("\nInterpretation note:\n")
cat("- Igeo values around 0 indicate near-background levels, while progressively positive values indicate increasing enrichment.\n")
cat("- The ±0.5 loading threshold is used to highlight strong source-metal associations.\n")
cat("- Metals loading strongly on the same factor likely share similar source pathways (geogenic, agricultural, or anthropogenic).\n")
cat(sprintf("- Figure saved as: %s\n", plot_file))
