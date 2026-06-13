# Task 30: Random Forest Feature Importance for Source Identification
# with PC1, PC2, and PC3 variance (%)
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

required_packages <- c("readxl", "randomForest", "dplyr", "ggplot2", "patchwork")
install_and_load(required_packages)

# Load and prepare data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)
metal_columns <- setdiff(names(data), "Number")
X <- data[, metal_columns]
X[] <- lapply(X, as.numeric)

clean_metal_symbol <- function(x) {
  gsub("[^A-Za-z].*$", "", x)
}

colnames(X) <- clean_metal_symbol(colnames(X))
X <- X[, !duplicated(names(X)), drop = FALSE]

# PCA for source axes
pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
var_pct <- (pca$sdev ^ 2) / sum(pca$sdev ^ 2) * 100
scores <- as.data.frame(pca$x[, 1:3])

set.seed(123)

rf_importance_for_pc <- function(y, X, pc_name, var_value) {
  model <- randomForest::randomForest(x = X, y = y, ntree = 1000, importance = TRUE)
  imp <- randomForest::importance(model, type = 1)

  if (is.matrix(imp)) {
    imp_values <- imp[, 1]
  } else {
    imp_values <- imp
  }

  imp_values[imp_values < 0] <- 0
  if (sum(imp_values) == 0) {
    imp_values <- rep(1 / length(imp_values), length(imp_values))
  } else {
    imp_values <- imp_values / sum(imp_values)
  }

  out <- data.frame(
    Metal = names(imp_values),
    Importance = as.numeric(imp_values),
    PC = sprintf("%s (Variance: %.1f%%)", pc_name, var_value),
    stringsAsFactors = FALSE
  )

  out <- out |>
    dplyr::arrange(dplyr::desc(Importance))
  out
}

imp_pc1 <- rf_importance_for_pc(scores$PC1, X, "PC1", var_pct[1])
imp_pc2 <- rf_importance_for_pc(scores$PC2, X, "PC2", var_pct[2])
imp_pc3 <- rf_importance_for_pc(scores$PC3, X, "PC3", var_pct[3])

plot_one_pc <- function(df, fill_high = "#f46d43") {
  df$Metal <- factor(df$Metal, levels = rev(df$Metal))

  ggplot(df, aes(x = Importance, y = Metal, fill = Importance)) +
    geom_col(color = "grey30", width = 0.72) +
    scale_fill_gradient(low = "#fee08b", high = fill_high) +
    labs(
      title = unique(df$PC),
      x = "Feature Importance",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    )
}

p1 <- plot_one_pc(imp_pc1, "#f46d43")
p2 <- plot_one_pc(imp_pc2, "#fdae61")
p3 <- plot_one_pc(imp_pc3, "#f46d43")

combined_plot <- (p1 | p2 | p3) +
  patchwork::plot_annotation(title = "Random Forest Feature Importance for Source Identification")

plot_file <- "30_random_forest_feature_importance_source_identification.png"
ggsave(plot_file, combined_plot, width = 14, height = 6, dpi = 300)
print(combined_plot)

importance_all <- dplyr::bind_rows(imp_pc1, imp_pc2, imp_pc3)
write.csv(importance_all, "30_random_forest_feature_importance_pc1_pc2_pc3.csv", row.names = FALSE)

cat("\nInterpretation note:\n")
cat("- Important metals for each PC represent key predictors of latent source gradients.\n")
cat("- Comparing PC1-PC3 reveals whether dominant and secondary source patterns are controlled by different metals.\n")
cat("- Variance percentages in subplot titles indicate how much of the total structure each component explains.\n")
cat(sprintf("- Figure saved as: %s\n", plot_file))
