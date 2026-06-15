#VBT Predictive Factors ~ part 2 (graphs)
#Written by Sarah Darnell, last modified 12.18.25

library(readr)
library(dplyr)
library(ggplot2)
library(ghibli)
library(ragg)
library(patchwork)
library(flextable)
library(reshape)

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

redcap_plots <- redcap %>%
  mutate(Group = case_when(
    Group == "Dysmenorrhea" ~ "Dysmenorrhea",
    Group == "Dysmenorrhea plus Bladder Pain" ~ "DYSB",
    Group == "Pain Free Control" ~ "Control"
  )) %>%
  mutate(Group = factor(Group, levels = c("Control", "Dysmenorrhea", "DYSB")))

ghibli_cols <- rev(ghibli_palette("YesterdayMedium", type = "discrete"))

my_colors <- c(
  "Control"      = ghibli_cols[3],   # green
  "Dysmenorrhea" = ghibli_cols[1],   # blue
  "DYSB"         = ghibli_cols[2]    # yellow
)

#GSRS box plot
gsrs <- ggplot(redcap_plots, aes(x = Group, y = gsrs_bl, fill = Group)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, linewidth = 0.3) +
  scale_fill_manual (values = my_colors) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "GSRS"
  )

#ICSI box plot
icsi <- ggplot(redcap_plots, aes(x = Group, y = icsi_bl, fill = Group)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, linewidth = 0.3) +
  scale_fill_manual (values = my_colors) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "ICSI"
  )

#GUPI box plot
gupi <- ggplot(redcap_plots, aes(x = Group, y = gupi_bl, fill = Group)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, linewidth = 0.3) +
  scale_fill_manual (values = my_colors) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) + labs(
    title = "GUPI"
  )

design <- "
123
123
123"

fig5 <- icsi + gupi + gsrs + plot_layout(design = design)

ggsave("Plots/figure5_box.png", plot = fig5, width = 5, height = 4, 
       dpi = 600, units = "in", device = "png")

##############################################
## Correlation matrices (GSRS, ICSI, GUPI ) ##
##############################################

#subset needed variables
correlation_subset <- redcap_plots %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  filter(Group == "Dysmenorrhea" | Group == "DYSB") %>%
  select(record_id, vbt_fu_pain, bl_urine_ml, 
         gupi_bl, icsi_bl, gsrs_bl, mcgill_1,
         mcgill_2, mcgill_3)

vars <- correlation_subset %>% select(vbt_fu_pain, bl_urine_ml, 
                                      gupi_bl, icsi_bl, gsrs_bl,
                                      mcgill_1, mcgill_2, mcgill_3)

#spearman matrix
table8 <- cor(vars, method = "spearman", use = "pairwise.complete.obs")

#save as table
ft <- as.data.frame(round(table8, 3)) %>%
  tibble::rownames_to_column("Variable") %>%
  flextable()

print(ft)

save_as_docx(ft, path = "Tables/Final/Table8_corr.docx")

#heatmap
#Mask diagonal (the 1s)
table8_masked <- table8
diag(table8_masked) <- NA

# Rename rows and columns with nicer labels
new_names <- c("FU Pain", "Urine (ml)", "GUPI", "ICSI", "GSRS", 
               "Pelvic Pain", "Bladder Pain", "Bowel Pain")

rownames(table8_masked) <- new_names
colnames(table8_masked) <- new_names

# Melt to long format for ggplot
melted_corr <- reshape2::melt(table8_masked, na.rm = FALSE)

# Determine color scale limits from non-diagonal values only
corr_range <- range(table8_masked, na.rm = TRUE)

heatmap <- ggplot(melted_corr, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))), 
            color = "black", size = 4) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-.6, .6),
    breaks = seq(-.6, .6, by = 0.3),
    na.value = "white",
    name = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed()


ggsave("Plots/figure6_heatmap_v3.png", plot = heatmap, width = 5, height = 4, 
       dpi = 600, units = "in", device = "png")









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

