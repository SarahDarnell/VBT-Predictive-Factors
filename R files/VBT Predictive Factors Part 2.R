#VBT Predictive Factors ~ part 2 (graphs)
#Written by Sarah Darnell, last modified 12.18.25

library(readr)
library(dplyr)
library(ggplot2)
library(ghibli)
library(ragg)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/VBT Predictive Factors CRAMPP2")

#import latest dataset
redcap <- read_csv("Edited data files/redcap_post_table10.csv")

########################
##Histogram of FU pain##
########################

histogram_fupain <- ggplot(redcap, aes(x = vbt_fu_pain, fill = Group)) +
  geom_histogram(color = "white", linewidth = 0.1, position = 'identity', binwidth = 5, boundary = 0) + 
  scale_fill_ghibli_d("YesterdayMedium", direction = -1) +
  labs(fill="") +
  labs(
    x = "Pain at First Urge (0-100 VAS)",
    y = "Count"
  ) + 
  theme_classic() +
  theme(
    text = element_text(family = "sans", size = 9),
    legend.position = c(0.75, 0.75),
    legend.title = element_blank()
  )

ggsave("Plots/figure1_hist.png", plot = histogram_fupain, width = 5, height = 4, 
       dpi = 600, units = "in", device = "png")

#############################
##Plot of FU pain VBT vs IP##
#############################

redcap_noHC <- redcap %>%
  filter(Group != "Pain Free Control")

scatter_plot_fupain <- ggplot(redcap_noHC, aes(x = ipb_fupain, y = vbt_fu_pain, color = Group)) +
  geom_point(size = 2) +
  scale_color_ghibli_d("YesterdayMedium", direction = -1) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Laboratory - Pain at First Urge (0-100 VAS)",
    y = "Virtual - Pain at First Urge (0-100 VAS)"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.75, 0.85), 
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(color = "black", linewidth = 0.5, fill = "white")
  )

ggsave("Plots/figure2_scatt.png", plot = scatter_plot_fupain, width = 5, height = 4, 
       dpi = 600, units = "in", device = "png")

#version with dotted lines at 15
scatter_plot_fupain_v2 <- ggplot(redcap_noHC, aes(x = ipb_fupain, y = vbt_fu_pain, color = Group)) +
  geom_point(size = 2) +
  scale_color_ghibli_d("YesterdayMedium", direction = -1) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_vline(xintercept = 15, linetype = "dotted", color = "gray50") +
  geom_hline(yintercept = 15, linetype = "dotted", color = "gray50") +
  labs(
    x = "Laboratory - Pain at First Urge (0-100 VAS)",
    y = "Virtual - Pain at First Urge (0-100 VAS)"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.75, 0.85), 
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(color = "black", linewidth = 0.5, fill = "white")
  )

ggsave("Plots/figure3_scatt.png", plot = scatter_plot_fupain_v2, width = 5, height = 4, 
       dpi = 600, units = "in", device = "png")

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

#uncomment to save output
sink("Logs/5.1.26/cor_matrix_log.txt")

#subset needed variables
correlation_subset <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  select(record_id, vbt_fu_pain, bl_urine_ml, 
         gupi_bl, icsi_bl, gsrs_bl, mcgill_1,
         mcgill_2, mcgill_3)

vars <- correlation_subset %>% select(vbt_fu_pain, bl_urine_ml, 
                                      gupi_bl, icsi_bl, gsrs_bl,
                                      mcgill_1, mcgill_2, mcgill_3)

#spearman matrix
cor(vars, method = "spearman", use = "pairwise.complete.obs")


#pearson matrix
cor(vars, method = "pearson", use = "pairwise.complete.obs")

sink()


###########################################
## Regression Models (GSRS, ICSI, GUPI) ##
###########################################

#uncomment to save output - update date here and throughout ggplots 
sink("Logs/12.18.25/regression_log.txt")


#subset needed variables
regression_subset <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  select(record_id, vbt_fu_pain, bl_urine_ml, 
         vbt_fs_pain, vbt_mt_pain, mcgill_1,
         mcgill_2, mcgill_3, promis_anx_t_score, 
         promis_dep_t_score, `PCS-T`, gsrs_bl, icsi_bl, gupi_bl)

#Model #1: mcgill_1/2/3 ~ FS pain + FU pain + MT pain + volume voided

mcgill_1_model_1 <- lm(mcgill_1 ~ 
                       vbt_fs_pain + 
                       vbt_fu_pain +
                       vbt_mt_pain +
                       bl_urine_ml, 
                  data = regression_subset)
#print summary of model
summary(mcgill_1_model_1)

mcgill_2_model_1 <- lm(mcgill_2 ~ 
                       vbt_fs_pain + 
                       vbt_fu_pain +
                       vbt_mt_pain +
                       bl_urine_ml, 
                     data = regression_subset)
#print summary of model
summary(mcgill_2_model_1)

mcgill_3_model_1 <- lm(mcgill_3 ~ 
                       vbt_fs_pain + 
                       vbt_fu_pain +
                       vbt_mt_pain +
                       bl_urine_ml, 
                     data = regression_subset)
#print summary of model
summary(mcgill_3_model_1)

#Model #2: mcgill_1/2/3 ~ anxiety, depression, PCS

mcgill_1_model_2 <- lm(mcgill_1 ~ 
                      promis_anx_t_score + 
                      promis_dep_t_score +
                      `PCS-T`, 
                     data = regression_subset)

#print summary of model
summary(mcgill_1_model_2)

mcgill_2_model_2 <- lm(mcgill_2 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`, 
                       data = regression_subset)

#print summary of model
summary(mcgill_2_model_2)

mcgill_3_model_2 <- lm(mcgill_3 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`, 
                       data = regression_subset)

#print summary of model
summary(mcgill_3_model_2)

#Model #3: mcgill_1/2/3 ~ anxiety, depression, PCS, 
#vbt_fs_pain, vbt_fu_pain, vbt_mt_pain, bl_urine_ml

mcgill_1_model_3 <- lm(mcgill_1 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         vbt_fs_pain + 
                         vbt_fu_pain +
                         vbt_mt_pain +
                         bl_urine_ml,
                       data = regression_subset)

#print summary of model
summary(mcgill_1_model_3)

mcgill_2_model_3 <- lm(mcgill_2 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         vbt_fs_pain + 
                         vbt_fu_pain +
                         vbt_mt_pain +
                         bl_urine_ml,
                       data = regression_subset)

#print summary of model
summary(mcgill_2_model_3)

mcgill_3_model_3 <- lm(mcgill_3 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         vbt_fs_pain + 
                         vbt_fu_pain +
                         vbt_mt_pain +
                         bl_urine_ml,
                       data = regression_subset)

#print summary of model
summary(mcgill_3_model_3)

#Model #4: mcgill_1/2/3 ~ anxiety, depression, PCS, GSRS

mcgill_1_model_4 <- lm(mcgill_1 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         gsrs_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_1_model_4)

mcgill_2_model_4 <- lm(mcgill_2 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         gsrs_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_2_model_4)

mcgill_3_model_4 <- lm(mcgill_3 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         gsrs_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_3_model_4)

#Model #5: mcgill_1/2/3 ~ anxiety, depression, PCS, ICSI

mcgill_1_model_5 <- lm(mcgill_1 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         icsi_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_1_model_5)

mcgill_2_model_5 <- lm(mcgill_2 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         icsi_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_2_model_5)

mcgill_3_model_5 <- lm(mcgill_3 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         icsi_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_3_model_5)

#Model #6: mcgill_1/2/3 ~ anxiety, depression, PCS, GUPI

mcgill_1_model_6 <- lm(mcgill_1 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         gupi_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_1_model_6)

mcgill_2_model_6 <- lm(mcgill_2 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         gupi_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_2_model_6)

mcgill_3_model_6 <- lm(mcgill_3 ~ 
                         promis_anx_t_score + 
                         promis_dep_t_score +
                         `PCS-T`+
                         gupi_bl,
                       data = regression_subset)

#print summary of model
summary(mcgill_3_model_6)


#uncomment to stop logging
sink()

