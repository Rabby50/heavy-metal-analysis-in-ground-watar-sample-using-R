# Task 19: Hierarchical Clustering Dendrogram
# Sample clustering based on heavy metal profiles
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

required_packages <- c("readxl", "ggplot2", "dendextend")
install_and_load(required_packages)

# Load data
data_file <- "HEAVY_METAL_DATA.xlsx"
data <- readxl::read_excel(data_file, sheet = 1)
metal_columns <- setdiff(names(data), "Number")
metal_data <- data[, metal_columns]
metal_data[] <- lapply(metal_data, as.numeric)

sample_labels <- as.character(data$Number)
rownames(metal_data) <- sample_labels

# Standardize and cluster
scaled_data <- scale(metal_data)
distance_matrix <- dist(scaled_data, method = "euclidean")
hc <- hclust(distance_matrix, method = "ward.D2")
threshold <- 8
clusters <- cutree(hc, h = threshold)

# Cluster statistics
cluster_table <- data.frame(
  Sample = names(clusters),
  Cluster = clusters,
  row.names = NULL
)
cluster_sizes <- as.data.frame(table(clusters))
names(cluster_sizes) <- c("Cluster", "Count")

cat("HIERARCHICAL CLUSTERING DENDROGRAM\n")
cat(sprintf("Samples clustered: %d\n", nrow(data)))
cat(sprintf("Metals used: %d\n\n", length(metal_columns)))
cat("Cluster membership:\n")
print(cluster_table, row.names = FALSE)
cat("\nCluster sizes:\n")
print(cluster_sizes, row.names = FALSE)
cat("\nInterpretation:\n")
cat("- Samples grouped in the same branch have similar heavy metal fingerprints.\n")
cat("- Clusters separated by larger linkage distances indicate distinct geochemical or contamination patterns.\n")
cat(sprintf("- The dashed cut line at Euclidean distance %.1f defines the reported sample clusters.\n", threshold))

# Save outputs
write.csv(cluster_table, "19_hierarchical_clustering_membership.csv", row.names = FALSE)
write.csv(cluster_sizes, "19_hierarchical_clustering_sizes.csv", row.names = FALSE)

# Convert to dendrogram and color branches
set.seed(1)
dend <- as.dendrogram(hc)
if (length(unique(clusters)) > 1) {
  dend <- dendextend::color_branches(dend, k = length(unique(clusters)))
}

plot_file <- "19_hierarchical_clustering_dendrogram.png"
png(plot_file, width = 1800, height = 1300, res = 220)
plot(
  dend,
  main = "Hierarchical Clustering Dendrogram\nSample Clustering Based on Heavy Metal Profiles",
  ylab = "Euclidean Distance",
  xlab = "Sample Number",
  cex = 0.85,
  las = 1
)
abline(h = threshold, col = "red", lty = 2, lwd = 1.2)
legend("topright", legend = "Cluster threshold", col = "red", lty = 2, bty = "n")
dev.off()

cat(sprintf("\nDendrogram saved as: %s\n", plot_file))
