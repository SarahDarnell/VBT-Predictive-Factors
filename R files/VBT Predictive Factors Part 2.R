#VBT Predictive Factors ~ part 2 (graphs)
#Written by Sarah Darnell, last modified 12.18.25

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

#uncomment to save output
sink("Logs/3.2.26/cor_matrix_log.txt")

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

