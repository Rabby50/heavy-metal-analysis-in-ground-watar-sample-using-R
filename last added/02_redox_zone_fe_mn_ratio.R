## ==============================================================================
## Script 2: Redox Zone Distribution Based on Fe/Mn Ratio
## Mahananda Floodplain Groundwater Arsenic Study
## Generates a log-scale bar chart of samples sorted by Fe/Mn ratio, colored
## by redox zone, with dashed threshold lines marking zone boundaries.
## ==============================================================================

# ---- 1. Load required packages ----
# install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# ---- 2. Set dummy input/output paths (EDIT THESE TO MATCH YOUR SYSTEM) ----
input_file  <- "C:/Users/YourName/Documents/Mahananda_Groundwater_Data.xlsx"
sheet_name  <- "Sheet1"
output_png  <- "C:/Users/Wadud/OneDrive/Documents/Redox_Zone_FeMn_Ratio.png"

# ---- 3. Read data from Excel ----
df <- read_excel("HEAVY_METAL_DATA.xlsx")
#View(HEAVY_METAL_DATA)
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

# ---- 5. Sort samples by Fe/Mn ratio (ascending) ----
df <- df %>%
  arrange(Fe_Mn_ratio) %>%
  mutate(sample_order = row_number())

zone_levels <- c("Highly Oxidizing",
                  "Moderately Oxidizing (Mn-dominated)",
                  "Transition/Suboxic",
                  "Moderately Reducing",
                  "Highly Reducing (Fe-dominated)")

zone_colors <- c("Highly Oxidizing"                     = "#00008B",
                  "Moderately Oxidizing (Mn-dominated)"  = "#ADD8E6",
                  "Transition/Suboxic"                   = "#FFA500",
                  "Moderately Reducing"                  = "#FF0000",
                  "Highly Reducing (Fe-dominated)"       = "#8B0000")

df$Redox_Zone <- factor(df$Redox_Zone, levels = zone_levels)

# ---- 6. Threshold lines and labels ----
thresholds <- data.frame(
  yint  = c(5, 2, 0.5, 0.2),
  label = c("Highly Reducing (>5)", "Moderately Reducing (>2)",
            "Transition (0.5-2)", "Oxidizing (<0.5)"),
  color = c("#8B0000", "#FF0000", "#FFA500", "#00008B")
)

# ---- 7. Build sorted, log-scale bar chart ----
p <- ggplot(df, aes(x = sample_order, y = Fe_Mn_ratio, fill = Redox_Zone)) +
  geom_col(width = 0.8, color = NA) +
  geom_hline(data = thresholds, aes(yintercept = yint, color = label),
             linetype = "dashed", linewidth = 0.6) +
  scale_fill_manual(name = "Redox Zone", values = zone_colors) +
  scale_color_manual(name = "Threshold", values = setNames(thresholds$color, thresholds$label)) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Redox Zone Distribution Based on Fe/Mn Ratio",
       x = "Samples (sorted by Fe/Mn ratio)",
       y = "Fe/Mn Ratio") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 8)) +
  guides(fill = "none")

# ---- 8. Save as PNG ----
ggsave(filename = output_png, plot = p, width = 10, height = 6, dpi = 300, bg = "white")

cat("Plot saved to:", output_png, "\n")
