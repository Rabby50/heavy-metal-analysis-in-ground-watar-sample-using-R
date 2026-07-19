## ==============================================================================
## Script 4: Heavy Metal Correlation Matrix with Hierarchical Clustering
## Mahananda Floodplain Groundwater Arsenic Study
## Reads heavy metal concentration data, prints a data overview (sample count,
## metal count, missing values, summary statistics), computes a Pearson
## correlation matrix, reorders it via hierarchical clustering, and plots it
## as a red-white-blue heatmap with significance markers (p < 0.05).
## ==============================================================================

# ---- 1. Load required packages ----
# install.packages(c("readxl", "dplyr", "ggplot2", "reshape2", "Hmisc", "RColorBrewer"))
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(RColorBrewer)

# ---- 2. Read data from Excel ----
df <- read_excel("HEAVY_METAL_DATA.xlsx")

# Keep only numeric metal-concentration columns (drops any ID/Sample-number column)
metal_df <- df %>% select(where(is.numeric))

# ---- 3. DATA OVERVIEW (matches console summary style) ----
cat("================================================================\n")
cat("DATA OVERVIEW\n")
cat("================================================================\n\n")
cat("Number of samples:", nrow(metal_df), "\n")
cat("Number of metals:", ncol(metal_df), "\n")
cat("Missing values:", sum(is.na(metal_df)), "\n\n")

cat("Basic Statistics:\n")

# Build a describe()-style table: rows = stats, columns = metals
stat_names <- c("count", "mean", "std", "min", "25%", "50%", "75%", "max")
stats_matrix <- sapply(metal_df, function(x) {
  x <- x[!is.na(x)]
  c(
    count = length(x),
    mean  = mean(x),
    std   = sd(x),
    min   = min(x),
    `25%` = as.numeric(quantile(x, 0.25)),
    `50%` = as.numeric(quantile(x, 0.50)),
    `75%` = as.numeric(quantile(x, 0.75)),
    max   = max(x)
  )
})
rownames(stats_matrix) <- stat_names

# Print with fixed-width, right-aligned columns (like the reference console output)
col_width <- 9
header <- sprintf("%-8s", "")
for (cn in colnames(stats_matrix)) header <- paste0(header, sprintf(paste0("%", col_width, "s"), cn))
cat(header, "\n")
for (i in seq_along(stat_names)) {
  row_label <- sprintf("%-8s", stat_names[i])
  row_vals  <- sapply(stats_matrix[i, ], function(v) {
    if (stat_names[i] == "count") sprintf(paste0("%", col_width, ".2f"), v)
    else sprintf(paste0("%", col_width, ".2f"), v)
  })
  cat(row_label, paste(row_vals, collapse = ""), "\n")
}
cat("\n")

# ---- 4. Set dummy output path (EDIT THIS TO MATCH YOUR SYSTEM) ----
output_png <- "C:/Users/YourName/Documents/Heavy_Metal_Correlation_Matrix.png"

# ---- 5. Compute Pearson correlation matrix and significance (p-values) ----
corr_result <- rcorr(as.matrix(metal_df), type = "pearson")
cor_matrix  <- corr_result$r
p_matrix    <- corr_result$P
diag(p_matrix) <- NA   # no significance marker needed on the diagonal

# ---- 6. Hierarchical clustering to reorder metals ----
dist_matrix <- as.dist(1 - cor_matrix)
hc          <- hclust(dist_matrix, method = "complete")
ordered_metals <- rownames(cor_matrix)[hc$order]

cor_ordered <- cor_matrix[ordered_metals, ordered_metals]
p_ordered   <- p_matrix[ordered_metals, ordered_metals]

# ---- 7. Reshape into long format for ggplot ----
cor_long <- melt(cor_ordered, varnames = c("Var1", "Var2"), value.name = "Correlation")
p_long   <- melt(p_ordered,   varnames = c("Var1", "Var2"), value.name = "p_value")

plot_df <- left_join(cor_long, p_long, by = c("Var1", "Var2")) %>%
  mutate(
    Var1 = factor(Var1, levels = ordered_metals),
    Var2 = factor(Var2, levels = ordered_metals),
    label = sprintf("%.2f", Correlation),
    significant = !is.na(p_value) & p_value < 0.05 & Var1 != Var2
  )

# ---- 8. Build the correlation heatmap ----
p <- ggplot(plot_df, aes(x = Var2, y = Var1, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label), color = "black", size = 3.3) +
  geom_text(data = filter(plot_df, significant),
            aes(label = "*"), color = "red", size = 5,
            nudge_x = 0.28, nudge_y = 0.22, fontface = "bold") +
  scale_fill_gradientn(
    colours = rev(brewer.pal(11, "RdBu")),
    limits = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  scale_x_discrete(position = "bottom") +
  scale_y_discrete(limits = rev(ordered_metals)) +
  coord_fixed() +
  labs(title = "Heavy Metal Correlation Matrix with Hierarchical Clustering",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank()
  )

# ---- 9. Save as PNG ----
ggsave(filename = output_png, plot = p, width = 9.5, height = 8, dpi = 300, bg = "white")

cat("Plot saved to:", output_png, "\n")
