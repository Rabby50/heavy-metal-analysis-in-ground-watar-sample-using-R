# Task 27: Health Risk Decomposition
# (a) Exposure pathway analysis (Top 5 risk samples)
# (b) Metal-specific risk contribution
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

# Convert concentration from ppb to mg/L
conc_mgL <- as.data.frame(metal_data / 1000)
colnames(conc_mgL) <- metal_symbols

# Keep one column per symbol if duplicates exist
conc_mgL <- conc_mgL[, !duplicated(names(conc_mgL)), drop = FALSE]
metal_symbols <- names(conc_mgL)

# Reference doses (mg/kg/day); conservative defaults for missing metals
rfd <- c(
  As = 0.0003, Se = 0.005, Pb = 0.0035, Cd = 0.001,
  Cr = 0.003, Ni = 0.02, Cu = 0.04, Zn = 0.3,
  Hg = 0.0003, Co = 0.02, Be = 0.002, V = 0.007,
  Fe = 0.7, Mn = 0.14, B = 0.2, Ba = 0.2,
  Sn = 0.3, Sr = 0.6, Ti = 0.3
)

rfd_vec <- rfd[metal_symbols]
rfd_vec[is.na(rfd_vec)] <- 0.1

# Exposure factors
IR <- 2.0      # ingestion rate (L/day)
BW <- 70.0     # body weight (kg)
EF <- 350.0
AT <- 365.0

# Pathway dose approximations (mg/kg/day)
cdi_ing <- as.matrix(conc_mgL) * IR * EF / (BW * AT)
cdi_inh <- cdi_ing * 0.08
cdi_der <- cdi_ing * 0.12

hq_ing <- sweep(cdi_ing, 2, rfd_vec, "/")
hq_inh <- sweep(cdi_inh, 2, rfd_vec, "/")
hq_der <- sweep(cdi_der, 2, rfd_vec, "/")

hi_total <- rowSums(hq_ing + hq_inh + hq_der, na.rm = TRUE)

sample_id <- if ("Number" %in% names(data)) data$Number else seq_len(nrow(data))

risk_summary <- data.frame(
  Sample = sample_id,
  HI = hi_total,
  Ingestion = rowSums(hq_ing, na.rm = TRUE),
  Inhalation = rowSums(hq_inh, na.rm = TRUE),
  Dermal = rowSums(hq_der, na.rm = TRUE),
  stringsAsFactors = FALSE
)

# Top 5 risk samples
top5 <- risk_summary |>
  dplyr::arrange(dplyr::desc(HI)) |>
  dplyr::slice(1:5)

top5_long <- top5 |>
  tidyr::pivot_longer(cols = c("Ingestion", "Inhalation", "Dermal"),
                      names_to = "Pathway", values_to = "Contribution")

top5$Sample <- factor(top5$Sample, levels = top5$Sample)
top5_long$Sample <- factor(top5_long$Sample, levels = top5$Sample)

left_plot <- ggplot(top5_long, aes(x = Sample, y = Contribution, fill = Pathway)) +
  geom_col(color = "grey30", width = 0.7, alpha = 0.9) +
  geom_text(
    data = top5,
    aes(x = Sample, y = HI, label = sprintf("HI=%.1f", HI)),
    inherit.aes = FALSE,
    vjust = -0.35,
    size = 3.2,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("Ingestion" = "#1f77b4", "Inhalation" = "#ff7f0e", "Dermal" = "#2ca02c")) +
  labs(
    title = "(c) Exposure Pathway Analysis (Top 5 Risk Samples)",
    x = "Sample Number",
    y = "Hazard Index Contribution",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# Metal-specific contribution summed over all samples and pathways
metal_total_contrib <- colSums(hq_ing + hq_inh + hq_der, na.rm = TRUE)
metal_df <- data.frame(
  Metal = names(metal_total_contrib),
  HQ_Total = as.numeric(metal_total_contrib),
  stringsAsFactors = FALSE
) |>
  dplyr::arrange(HQ_Total)

metal_df$Metal <- factor(metal_df$Metal, levels = metal_df$Metal)

right_plot <- ggplot(metal_df, aes(x = HQ_Total, y = Metal, fill = HQ_Total)) +
  geom_col(color = "grey30", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", HQ_Total)), hjust = -0.15, size = 3) +
  scale_fill_gradient(low = "#d9f0d3", high = "#31a354") +
  labs(
    title = "(d) Metal-Specific Risk Contribution",
    x = "Total Hazard Quotient (Sum over all samples)",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(0, max(metal_df$HQ_Total) * 1.12))

combined_plot <- left_plot + right_plot

plot_file <- "27_exposure_pathway_and_metal_risk_contribution.png"
ggsave(plot_file, combined_plot, width = 13, height = 6, dpi = 300)
print(combined_plot)

write.csv(risk_summary, "27_pathway_risk_summary.csv", row.names = FALSE)
write.csv(metal_df, "27_metal_specific_risk_contribution.csv", row.names = FALSE)

cat("\nInterpretation note:\n")
cat("- Top-5 samples represent the highest integrated non-carcinogenic burden (HI).\n")
cat("- Pathway stacks reveal whether ingestion, inhalation, or dermal contact dominates risk at hotspot samples.\n")
cat("- Metal totals indicate priority contaminants for control and mitigation.\n")
cat(sprintf("- Figure saved as: %s\n", plot_file))
