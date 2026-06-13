# Task 29: Carcinogenic Health Risk Assessment
# x-axis: sample number
# y-axis: Cancer Risk (x 10^-6)
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
metal_data <- data[, metal_columns]
metal_data[] <- lapply(metal_data, as.numeric)

clean_metal_symbol <- function(x) {
  gsub("[^A-Za-z].*$", "", x)
}

metal_symbols <- clean_metal_symbol(metal_columns)

# Convert concentration from ppb to mg/L
conc_mgL <- as.data.frame(metal_data / 1000)
colnames(conc_mgL) <- metal_symbols
conc_mgL <- conc_mgL[, !duplicated(names(conc_mgL)), drop = FALSE]
metal_symbols <- names(conc_mgL)

# Cancer slope factors ((mg/kg/day)^-1)
csf <- c(
  As = 1.5,
  Cd = 6.1,
  Cr = 0.5,
  Ni = 0.84,
  Pb = 0.0085,
  Be = 0.02
)

csf_vec <- csf[metal_symbols]
carc_idx <- which(!is.na(csf_vec))

if (length(carc_idx) == 0) {
  stop("No carcinogenic metals with slope factors found in the dataset.")
}

# Exposure model
IR <- 2.0
BW <- 70.0
EF <- 350.0
AT <- 365.0

cdi_ing <- as.matrix(conc_mgL[, carc_idx, drop = FALSE]) * IR * EF / (BW * AT)

# Total cancer risk by sample
cancer_risk <- rowSums(sweep(cdi_ing, 2, csf_vec[carc_idx], "*"), na.rm = TRUE)

# Convert to x10^-6 scale
cancer_risk_u <- cancer_risk * 1e6

sample_id <- if ("Number" %in% names(data)) data$Number else seq_len(nrow(data))

risk_df <- data.frame(
  Sample = sample_id,
  CancerRisk_u = cancer_risk_u,
  stringsAsFactors = FALSE
)

# Risk classes:
# High risk > 1e-4, acceptable <= 1e-6
risk_df$Class <- ifelse(
  cancer_risk > 1e-4,
  "High",
  ifelse(cancer_risk > 1e-6, "Moderate", "Acceptable")
)

risk_df$Sample <- factor(risk_df$Sample, levels = risk_df$Sample)

plot_carc <- ggplot(risk_df, aes(x = Sample, y = CancerRisk_u, fill = Class)) +
  geom_col(color = "grey30", width = 0.8, alpha = 0.9) +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 1, color = "orange", linetype = "dashed", linewidth = 0.9) +
  scale_fill_manual(values = c("Acceptable" = "#5ab4ac", "Moderate" = "#f2c14e", "High" = "#ff4d4d")) +
  labs(
    title = "Carcinogenic Health Risk Assessment",
    x = "Sample Number",
    y = "Cancer Risk (x 10^-6)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

plot_file <- "29_carcinogenic_health_risk_assessment.png"
ggsave(plot_file, plot_carc, width = 12, height = 7, dpi = 300)
print(plot_carc)

write.csv(risk_df, "29_carcinogenic_risk_values_x10minus6.csv", row.names = FALSE)

cat("\nInterpretation note:\n")
cat("- Dashed red line indicates the high-risk benchmark (1e-4).\n")
cat("- Dashed orange line indicates the acceptable benchmark (1e-6).\n")
cat(sprintf("- Samples above high-risk benchmark: %d of %d.\n", sum(cancer_risk > 1e-4), nrow(risk_df)))
cat(sprintf("- Figure saved as: %s\n", plot_file))
