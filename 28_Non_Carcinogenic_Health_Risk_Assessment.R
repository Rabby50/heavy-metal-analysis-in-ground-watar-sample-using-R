# Task 28: Non-Carcinogenic Health Risk Assessment
# x-axis: sample number
# y-axis: Hazard Index (HI)
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

# Reference doses (mg/kg/day)
rfd <- c(
  As = 0.0003, Se = 0.005, Pb = 0.0035, Cd = 0.001,
  Cr = 0.003, Ni = 0.02, Cu = 0.04, Zn = 0.3,
  Hg = 0.0003, Co = 0.02, Be = 0.002, V = 0.007,
  Fe = 0.7, Mn = 0.14, B = 0.2, Ba = 0.2,
  Sn = 0.3, Sr = 0.6, Ti = 0.3
)

rfd_vec <- rfd[metal_symbols]
rfd_vec[is.na(rfd_vec)] <- 0.1

# Exposure model
IR <- 2.0
BW <- 70.0
EF <- 350.0
AT <- 365.0

cdi_ing <- as.matrix(conc_mgL) * IR * EF / (BW * AT)
cdi_inh <- cdi_ing * 0.08
cdi_der <- cdi_ing * 0.12

hq_ing <- sweep(cdi_ing, 2, rfd_vec, "/")
hq_inh <- sweep(cdi_inh, 2, rfd_vec, "/")
hq_der <- sweep(cdi_der, 2, rfd_vec, "/")

hi_total <- rowSums(hq_ing + hq_inh + hq_der, na.rm = TRUE)

sample_id <- if ("Number" %in% names(data)) data$Number else seq_len(nrow(data))

risk_df <- data.frame(
  Sample = sample_id,
  HI = hi_total,
  stringsAsFactors = FALSE
)

risk_df$Sample <- factor(risk_df$Sample, levels = risk_df$Sample)
risk_df$RiskClass <- ifelse(risk_df$HI >= 1, "High", ifelse(risk_df$HI >= 0.1, "Action", "Low"))

plot_non_carc <- ggplot(risk_df, aes(x = Sample, y = HI, fill = RiskClass)) +
  geom_col(color = "grey30", width = 0.8, alpha = 0.9) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = 0.1, color = "orange", linetype = "dashed", linewidth = 0.9) +
  scale_fill_manual(values = c("Low" = "#5ab4ac", "Action" = "#f2c14e", "High" = "#ff4d4d")) +
  labs(
    title = "Non-Carcinogenic Health Risk Assessment",
    x = "Sample Number",
    y = "Hazard Index (HI)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

plot_file <- "28_non_carcinogenic_health_risk_assessment.png"
ggsave(plot_file, plot_non_carc, width = 12, height = 7, dpi = 300)
print(plot_non_carc)

write.csv(risk_df, "28_non_carcinogenic_hi_values.csv", row.names = FALSE)

cat("\nInterpretation note:\n")
cat("- HI > 1 indicates potential non-carcinogenic concern for cumulative exposure.\n")
cat("- HI between 0.1 and 1 suggests a precautionary action zone.\n")
cat(sprintf("- High-risk samples (HI >= 1): %d of %d.\n", sum(risk_df$HI >= 1), nrow(risk_df)))
cat(sprintf("- Figure saved as: %s\n", plot_file))
