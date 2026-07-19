## ==============================================================================
## Script 1: Redox Zone Distribution
## Mahananda Floodplain Groundwater Arsenic Study
## Generates a horizontal bar chart showing the number of samples in each
## redox zone, classified using the Fe/Mn ratio.
## ==============================================================================

# ---- 1. Load required packages ----
# install.packages(c("dplyr", "ggplot2"))
#library(openxlsx)
library(dplyr)
library(ggplot2)
library(readxl)
#HEAVY_METAL_DATA <- read_excel("HEAVY_METAL_DATA.xlsx")
#View(HEAVY_METAL_DATA)

# ---- 2. Set dummy input/output paths (EDIT THESE TO MATCH YOUR SYSTEM) ----
input_file  <- "HEAVY_METAL_DATA.xlsx"
sheet_name  <- "Sheet1"
output_png  <- "C:/Users/Wadud/OneDrive/Documents/Redox_Zone_Distribution.png"

# ---- 3. Read data from Excel ----
df <- read_excel("HEAVY_METAL_DATA.xlsx")
View(HEAVY_METAL_DATA)

# Expected columns: Number, As_75, Se_82, Pb_208, Cd_111, Cr_52, Ni_60, Cu_63,
# Zn_66, Hg_202, Co_59, Be_9, V_51, Fe_57, Mn_55

# ---- 4. Compute Fe/Mn ratio and classify redox zone ----
df <- df %>%
  mutate(
    Fe_Mn_ratio = Fe_57 / Mn_55,
    Redox_Zone = case_when(
      Fe_Mn_ratio > 5              ~ "Highly Reducing (Fe-dominated)",
      Fe_Mn_ratio > 2              ~ "Moderately Reducing",
      Fe_Mn_ratio >= 0.5           ~ "Transition/Suboxic",
      Fe_Mn_ratio >= 0.2           ~ "Moderately Oxidizing (Mn-dominated)",
      TRUE                         ~ "Highly Oxidizing"
    )
  )

# ---- 5. Order zones from most oxidizing (bottom) to most reducing (top) ----
zone_levels <- c("Highly Oxidizing",
                  "Moderately Oxidizing (Mn-dominated)",
                  "Moderately Reducing",
                  "Transition/Suboxic",
                  "Highly Reducing (Fe-dominated)")

zone_colors <- c("Highly Oxidizing"                     = "#00008B",
                  "Moderately Oxidizing (Mn-dominated)"  = "#ADD8E6",
                  "Moderately Reducing"                  = "#FF0000",
                  "Transition/Suboxic"                   = "#FFA500",
                  "Highly Reducing (Fe-dominated)"       = "#8B0000")

summary_df <- df %>%
  mutate(Redox_Zone = factor(Redox_Zone, levels = zone_levels)) %>%
  count(Redox_Zone, .drop = FALSE)

# ---- 6. Print geochemical redox indicators (mean, median, range) ----
cat("\n================================================================\n")
cat("GEOCHEMICAL REDOX INDICATORS\n")
cat("================================================================\n")
indicators <- df %>%
  mutate(
    As_Fe_ratio = (As_75 / Fe_57) * 1000,
    As_Mn_ratio = As_75 / Mn_55,
    Fe_Mn_As    = (Fe_57 + Mn_55) / As_75
  )

cat(sprintf("%-15s %-10s %-10s %-15s\n", "Ratio", "Mean", "Median", "Range"))
cat(sprintf("%-15s %-10.2f %-10.2f %.2f - %.2f\n", "Fe/Mn ratio",
            mean(indicators$Fe_Mn_ratio), median(indicators$Fe_Mn_ratio),
            min(indicators$Fe_Mn_ratio), max(indicators$Fe_Mn_ratio)))
cat(sprintf("%-15s %-10.2f %-10.2f %.2f - %.2f\n", "As/Fe ratio(%o)",
            mean(indicators$As_Fe_ratio), median(indicators$As_Fe_ratio),
            min(indicators$As_Fe_ratio), max(indicators$As_Fe_ratio)))
cat(sprintf("%-15s %-10.2f %-10.2f %.2f - %.2f\n", "As/Mn ratio",
            mean(indicators$As_Mn_ratio), median(indicators$As_Mn_ratio),
            min(indicators$As_Mn_ratio), max(indicators$As_Mn_ratio)))
cat(sprintf("%-15s %-10.0f %-10.0f %.0f - %.0f\n", "(Fe+Mn)/As",
            mean(indicators$Fe_Mn_As), median(indicators$Fe_Mn_As),
            min(indicators$Fe_Mn_As), max(indicators$Fe_Mn_As)))

# ---- 7. Build the horizontal bar chart ----
p <- ggplot(summary_df, aes(x = Redox_Zone, y = n, fill = Redox_Zone)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.2) +
  geom_text(aes(label = n), hjust = -0.3, size = 5) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = zone_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Redox Zone Distribution",
       x = "Redox Zone",
       y = "Number of Samples") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank())

# ---- 8. Save as PNG ----
ggsave(filename = output_png, plot = p, width = 9, height = 5.5, dpi = 300, bg = "white")

cat("\nPlot saved to:", output_png, "\n")
