## ==============================================================================
## Script 3: Arsenic Distribution by Redox Zone
## Mahananda Floodplain Groundwater Arsenic Study
## Generates a log-scale boxplot of arsenic concentration across redox zones,
## with WHO/guideline reference lines and mean/median markers.
## ==============================================================================

# ---- 1. Load required packages ----
# install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# ---- 2. Set dummy input/output paths (EDIT THESE TO MATCH YOUR SYSTEM) ----

output_png  <- "C:/Users/Wadud/OneDrive/Documents/Arsenic_Distribution_by_Redox_Zone.png"

# ---- 3. Read data from Excel ----
df <- read_excel("HEAVY_METAL_DATA.xlsx")

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

# ---- 5. Order zones from most reducing (left) to most oxidizing (right) ----
zone_levels <- c("Highly Reducing (Fe-dominated)",
                  "Moderately Reducing",
                  "Transition/Suboxic",
                  "Moderately Oxidizing (Mn-dominated)",
                  "Highly Oxidizing")

zone_colors <- c("Highly Reducing (Fe-dominated)"       = "#8B0000",
                  "Moderately Reducing"                  = "#FF0000",
                  "Transition/Suboxic"                   = "#FFD700",
                  "Moderately Oxidizing (Mn-dominated)"  = "#ADD8E6",
                  "Highly Oxidizing"                     = "#00008B")

df$Redox_Zone <- factor(df$Redox_Zone, levels = zone_levels)

# ---- 6. Reference lines (adjust to your regulatory guideline values) ----
guideline_val  <- 20   # e.g. Bangladesh/India drinking water standard (mg/kg or ug/L)
highrisk_val   <- 50   # WHO provisional/high-risk threshold

# ---- 6b. Precompute per-zone mean for a dashed mean marker on each box ----
zone_means <- df %>%
  group_by(Redox_Zone) %>%
  summarise(mean_As = mean(As_75, na.rm = TRUE), .groups = "drop") %>%
  mutate(xmin = as.numeric(Redox_Zone) - 0.25,
         xmax = as.numeric(Redox_Zone) + 0.25)

# ---- 7. Build log-scale boxplot with mean & median markers ----
p <- ggplot(df, aes(x = Redox_Zone, y = As_75, fill = Redox_Zone)) +
  geom_boxplot(width = 0.5, color = "black", linewidth = 0.4, outlier.shape = 1) +
  geom_segment(data = zone_means,
               aes(x = xmin, xend = xmax, y = mean_As, yend = mean_As),
               inherit.aes = FALSE, color = "red", linetype = "dashed", linewidth = 0.6) +
  geom_hline(aes(yintercept = guideline_val, linetype = "Guideline (20 mg/kg)"),
             color = "red", linewidth = 0.6) +
  geom_hline(aes(yintercept = highrisk_val, linetype = "High risk (50 mg/kg)"),
             color = "black", linewidth = 0.6) +
  scale_linetype_manual(name = NULL,
                         values = c("Guideline (20 mg/kg)" = "dashed",
                                    "High risk (50 mg/kg)" = "dashed")) +
  scale_fill_manual(values = zone_colors) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Arsenic Distribution by Redox Zone",
       x = "Redox Zone",
       y = "Arsenic (mg/kg)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# ---- 8. Save as PNG ----
ggsave(filename = output_png, plot = p, width = 9, height = 6, dpi = 300, bg = "white")

cat("Plot saved to:", output_png, "\n")
