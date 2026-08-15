# Script name: LumenVol_vs_Sphericity_Scatter.R
# Creates a scatter graph relating the individual volume of lumens  to their sphericity
  # A full data set of at least three triplicates is recommended to conclude trends from this graph
  # PERMANOVA statistical testing is done comparing all data points above 1000 microns^3
  # An ellipse encompassing 60% of lumens above 1000 microns^3 in volume for each condition is included on the graph
# This code can be modified in the indicated areas for graphing different variables
# Author: Dr. Madeline Lovejoy
# Date: 2026-08-14
# R version 4.1.2 (2021-11-01)

# readxl version 1.4.2
# dplyr version 1.1.2
# tidyr version 1.3.0
# ggplot2 version 3.4.3
# vegan version 2.6.4

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)

# Load data
# ***BEFORE RUNNING THIS LINE, MANUALLY SET THE WORKING DIRECTORY TO YOUR QUANTIFICATION FOLDER BY GOING TO SESSION -> SET WORKING DIRECTORY -> CHOOSE DIRECTORY***
dir <- getwd()
# ***CAN BE ANY VARIABLES WITH A SIMILAR FORMAT***
df <- read_excel("Lumen vol vs. sphericity combined data.xlsx")

# Defines column names as conditions
long_df <- df %>%
  pivot_longer(
    # ***CHANGE THESE NAMES TO YOUR CONDITIONS***
    cols = c(CTRL, `SGEF KD`, 'SGEF KD + iCRT3'),
    names_to = "Condition",
    # Defines the values in these columns as individual lumen volume measurements
    values_to = "Lumen_vol"
  ) %>%
  # Eliminates NA values in CSV data
  filter(!is.na(Lumen_vol), !is.na(Sphericity))

# Sets up which values will be on the graph
plot_df <- long_df
plot_df$Condition <- factor(
  plot_df$Condition,
  # ***CHANGE THESE NAMES TO YOUR CONDITIONS***
  levels = c("CTRL", "SGEF KD", "SGEF KD + iCRT3")
)

# Filter out small lumens from statistical analysis (will still appear on the graph)
analysis_df <- long_df %>%
  filter(Lumen_vol >= 1000)

# PERMANOVA statistical analysis between values with lumen volumes above 1000 microns^3
pairwise_permanova <- function(data, group_col, vars, method = "euclidean") {
  groups <- unique(data[[group_col]])
  combs <- combn(groups, 2, simplify = FALSE)
  results <- lapply(combs, function(pair) {
    sub <- data %>% dplyr::filter(.data[[group_col]] %in% pair)
    perm <- vegan::adonis2(
      as.matrix(sub[, vars]) ~ sub[[group_col]],
      method = method,
      permutations = 999
    )
    # Groups are based on the number of variables comparing
    data.frame(
      group1 = pair[1],
      group2 = pair[2],
      F = perm$F[1],
      R2 = perm$R2[1],
      p = perm$`Pr(>F)`[1]
    )
  })
  do.call(rbind, results)
}
pairwise_results <- pairwise_permanova(
  analysis_df,
  group_col = "Condition",
  vars = c("Sphericity", "Lumen_vol")
)
pairwise_results$p_adj <- p.adjust(pairwise_results$p, method = "BH")
# Shows the p value comparisons between each condition in the Console panel
print(pairwise_results)

# Graphing the scatter plot
# Assigns sphericity values to the x-axis and individual lumen volume values to the y-axis
ggplot(plot_df, aes(x = Sphericity, y = Lumen_vol)) +
  
  # Ellipse based on filtered data only
  stat_ellipse(
    data = analysis_df,
    aes(fill = Condition),
    geom = "polygon",
    type = "norm",
    # 60% of data
    level = 0.60,
    # Transparency
    alpha = 0.25,
    color = NA
  ) +
  
  # Ellipse outline
  stat_ellipse(
    data = analysis_df,
    aes(color = Condition),
    type = "norm",
    level = 0.60,
    # Outline thickness
    linewidth = 1
  ) +
  
  # All points order of overlay on the graph
  # ***CHANGE THE WRITTEN CONDITIONS TO YOUR CONDITIONS***
  # CTRL (bottom)
  geom_point(
    data = subset(plot_df, Condition == "CTRL"),
    aes(color = Condition),
    # Point size
    size = 1,
    # Transparency
    alpha = 0.85
  ) +
  
  # SGEF KD (middle)
  geom_point(
    data = subset(plot_df, Condition == "SGEF KD"),
    aes(color = Condition),
    size = 1,
    alpha = 0.85
  ) +
  
  # KD + iCRT3 (TOP)
  geom_point(
    data = subset(plot_df, Condition == "SGEF KD + iCRT3"),
    aes(color = Condition),
    size = 1,
    alpha = 0.95
  ) +
  
  # ***ALL COLORS USED IN SCATTER GRAPHS FOR THE PAPER ARE LISTED HERE - THEY CAN BE UNCOMMENTED WHEN NEEDED***
  scale_color_manual(
    values = c(
      "CTRL" = "blue",
      # orange
      "SGEF KD" = rgb(255, 128, 0, maxColorValue = 255),
      # green
      "SGEF KD + iCRT3" = rgb(0, 90, 0, maxColorValue = 255)
      # magenta
      # "SGEF KD + GM6001" = rgb(173, 7, 227, maxColorValue = 255)
      # grey
      # "CTRL + GM6001" = rgb(96, 96, 96, maxColorValue = 255)
      # turquoise
      # "WT Rescue" = rgb(64, 224, 208, maxColorValue = 255)
    )
  ) +
  
  # ***CHANGE THE Y-AXIS SCALE BASED ON YOUR DATA***
  scale_y_continuous(
    limits = c(NA, 30000),
    breaks = seq(0, 30000, by = 10000)
  ) +
  
  # ***CHANGE THE X-AXIS SCALE BASED ON YOUR DATA***
  scale_x_continuous(
    limits = c(0, 0.8),
    breaks = seq(0, 0.8, by = 0.2)
  ) +
  
  scale_fill_manual(
    values = c(
      "CTRL" = "blue",
      "SGEF KD" = rgb(255, 128, 0, maxColorValue = 255),
      "SGEF KD + iCRT3" = rgb(0, 90, 0, maxColorValue = 255)
    )
  ) +
  
  theme_classic(base_size = 14) +
  theme(
      axis.ticks.length = unit(0.25, "cm"),
      aspect.ratio = 0.6,
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(face = "bold"),
      axis.text  = element_text(face = "bold", color = "black")
  )+

  #annotate(
   # "text",
    #x = Inf,
    #y = Inf,
    #label = label_text,
    #hjust = 1.1,
    #vjust = 1.1,
    #size = 4
  #) +
  
  labs(
    x = "Sphericity", 
    y = "Individual lumen volume"
      
    )
  
