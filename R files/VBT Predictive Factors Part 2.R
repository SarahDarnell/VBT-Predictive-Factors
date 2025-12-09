#VBT Predictive Factors ~ part 2 (graphs)
#Written by Sarah Darnell, last modified 12.8.25

library(readr)
library(dplyr)
library(ggplot2)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/VBT Predictive Factors CRAMPP2")

#import latest dataset
redcap <- read_csv("Edited data files/redcap_post_supplementary_vars.csv")

#############################################
##CPP measures box plots (GSRS, ICSI, GUPI)##
#############################################

#GSRS box plot
ggplot(redcap, aes(x = Group, y = gsrs_bl)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(
    y = "GSRS"
  )

#ICSI box plot
ggplot(redcap, aes(x = Group, y = icsi_bl)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(
    y = "ICSI"
  )

#GUPI box plot
ggplot(redcap, aes(x = Group, y = gupi_bl)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(
    y = "GUPI"
  )


##############################################
## Correlation matrices (GSRS, ICSI, GUPI ) ##
##############################################

#subset needed variables
correlation_subset <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  select(record_id, vbt_fu_pain, bl_urine_ml, 
         gupi_bl, icsi_bl, gsrs_bl)

vars <- correlation_subset %>% select(vbt_fu_pain, bl_urine_ml, 
                                      gupi_bl, icsi_bl, gsrs_bl)

#spearman matrix
spearman <- cor(vars, method = "spearman", use = "pairwise.complete.obs")

#pearson matrix
pearson <- cor(vars, method = "pearson", use = "pairwise.complete.obs")









