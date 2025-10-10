#VBT Predictive Factors Sub-analyses - MPA projects for ZFK and KJ
##Written by Sarah Darnell, last modified 10.10.25

library(readr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(coin)
library(tidyr)
library(flextable)
library(officer)
library(patchwork)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/VBT Predictive Factors CRAMPP2")

#load latest version of dataset

#pull out only VBT vars
redcap_subset <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1")

###############################################################################
##ZFK project - THC usage and correlations to anx, dep, sleep, and pain at FU##
###############################################################################

#define vars
vars <- c("promis_anx_t_score", "promis_dep_t_score", "promis_sd_t_score",
          "vbt_fu_pain", "mh23")

#visualize vars for normality, change vars and uncomment to view
#ggplot(redcap_subset, aes(mh23)) + geom_histogram()

##Mann-Whitney test for correlation between THC usage and vars

#convert THC variable to a factor
redcap_subset$ms_thc <- factor(redcap_subset$ms_thc, levels = c("No", "Yes"))

#table with medians stratified by THC usage for vars
thc_table <- redcap_subset %>%
  select(all_of(vars), ms_thc) %>%
  pivot_longer(cols = -ms_thc, names_to = "Item", values_to = "Value") %>% 
  group_by(ms_thc, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f], n=%d", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE),
                                            sum(!is.na(Value))),
                   .groups = "drop") %>%
  pivot_wider(names_from = ms_thc, values_from = `Median [IQR]`) 

#Mann-Whitney test
mw <- lapply(vars, function(var) {
  temp <- redcap_subset[, c(var, "ms_thc")]
  temp <- temp[complete.cases(temp), ]  # remove NAs
  
  # Only run if exactly 2 groups and variable has enough unique values
  if (length(unique(temp$ms_thc)) != 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)
  }
  
  test_result <- wilcox.test(as.formula(paste(var, "~ ms_thc")), data = temp)
  
  # Compute rank-biserial correlation effect size
  eff <- temp %>%
    wilcox_effsize(as.formula(paste(var, "~ ms_thc")), ci = TRUE)
  
  data.frame(
    Variable = var,
    W = test_result$statistic,
    p_value = test_result$p.value,
    Effect_Size = eff$effsize,
    stringsAsFactors = FALSE
  )
})

mw_results <- do.call(rbind, mw) %>%
  select(-W)

#combine thc_table with mann-whitney results
names(thc_table)[1] <- "Variable"

thc_table_full <- left_join(thc_table, mw_results, by = "Variable")

#reformat and save table
ft <- flextable(thc_table_full) %>%
  bold(i = which(thc_table_full$Variable != ""), j = 1) %>% # Bold variable rows
  align(align = "left", part = "all") %>%                   # Align left
  fontsize(size = 9, part = "all") %>%                      # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%     # Fixed width layout
  width(j = 1, width = 2.25) %>%                            # Widen first column for variable names
  width(j = 2:ncol(thc_table_full), width = 1.25) %>%       # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/MPA/thc_table_n.docx")

#box plot of depression across THC usage
dep_box_plot <- ggplot(redcap_subset, aes(x = ms_thc, y = promis_dep_t_score, fill = ms_thc)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = "Depression t-score",
    x = "THC usage"
  ) +
  scale_fill_brewer(palette = "Set2")

ggsave("plots/dep_box_plot.png", plot = dep_box_plot)

#box plot of anxiety across THC usage
anx_box_plot<- ggplot(redcap_subset, aes(x = ms_thc, y = promis_anx_t_score, fill = ms_thc)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = "Anxiety t-score",
    x = "THC usage"
  ) +
  scale_fill_brewer(palette = "Set2")

ggsave("plots/anx_box_plot.png", plot = anx_box_plot)

#######################################################################################
##KJ project - SES and correlations anx, dep, race, ethnicity, gender, and pain at FU##
#######################################################################################

##Table with medians for continuous vars

#define median vars for spearman
median_vars <- c("promis_anx_t_score", "promis_dep_t_score", "vbt_fu_pain", "mh23")

#create table
table_median <- redcap_subset %>%
  select(all_of(median_vars)) %>%
  dplyr::summarize(across(everything(), ~ sprintf("%.1f [%.1f-%.1f], n=%d", 
                                                  median(., na.rm = TRUE), 
                                                  quantile(., 0.25, na.rm = TRUE),
                                                  quantile(., 0.75, na.rm = TRUE),
                                                  sum(!is.na(.))))) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Median [IQR]")

#Spearman's test for MacArthurs Q1 - Standing in US
Spearman_results <- lapply(median_vars, function(var) {
  test_result <- cor.test(redcap_subset$macarthur_ladder_us, 
                          redcap_subset[[var]], method = "spearman", exact = FALSE)
  rho <- test_result$estimate
  p_value <- test_result$p.value
  data.frame(
    Item = var,
    U.S. = rho,
    p_value = p_value, 
    stringsAsFactors = FALSE
  )
})

Spearman_results_df <- do.call(rbind, Spearman_results) %>%
  mutate(
    p_value = ifelse(
      p_value < 0.001,            # cap very small p-values
      "<0.001",
      format(round(p_value, 3),   # round to 3 decimal places
             scientific = FALSE)
    ))
rownames(Spearman_results_df) <- NULL

#Spearman's test for MacArthurs Q2 - Standing in community
Spearman_results_2 <- lapply(median_vars, function(var) {
  test_result <- cor.test(redcap_subset$macarthur_ladder_community, 
                          redcap_subset[[var]], method = "spearman", exact = FALSE)
  rho <- test_result$estimate
  p_value <- test_result$p.value
  data.frame(
    Item = var,
    Community = rho,
    p_value = p_value, 
    stringsAsFactors = FALSE
  )
})

Spearman_results_df_2 <- do.call(rbind, Spearman_results_2) %>%
  mutate(
    p_value = ifelse(
      p_value < 0.001,            # cap very small p-values
      "<0.001",
      format(round(p_value, 3),   # round to 3 decimal places
             scientific = FALSE)
    ))
rownames(Spearman_results_df_2) <- NULL

#combine Spearman's results
spearman_results_full <- left_join(Spearman_results_df, Spearman_results_df_2, 
                                   by = "Item")

#combine median table with spearman's results
SES_table_full <- left_join(table_median, spearman_results_full, by = "Item")


#reformat and save table
ft <- flextable(SES_table_full) %>%
  bold(i = which(SES_table_full$Item != ""), j = 1) %>% # Bold variable rows
  align(align = "left", part = "all") %>%                   # Align left
  fontsize(size = 9, part = "all") %>%                      # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%     # Fixed width layout
  width(j = 1, width = 2.25) %>%                            # Widen first column for variable names
  width(j = 2:ncol(SES_table_full), width = 1.25) %>%       # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/MPA/ses_table_n.docx")

#scatter plot of US standing and anxiety
anx_US_plot <- ggplot(redcap_subset, aes(x = macarthur_ladder_us, y = promis_anx_t_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(x = "SSS in U.S. (1–10)", y = "Anxiety t-score") +
  theme_minimal()

#scatter plot of community standing and anxiety
anx_comm_plot <- ggplot(redcap_subset, aes(x = macarthur_ladder_community, y = promis_anx_t_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  labs(x = "SSS in community (1–10)", y = "Anxiety t-score") +
  theme_minimal()

#scatter plot of US standing and depression
dep_US_plot <- ggplot(redcap_subset, aes(x = macarthur_ladder_us, y = promis_dep_t_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(x = "SSS in U.S. (1–10)", y = "Depression t-score") +
  theme_minimal()

#scatter plot of community standing and depression
dep_comm_plot <- ggplot(redcap_subset, aes(x = macarthur_ladder_community, y = promis_dep_t_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(x = "SSS in community (1–10)", y = "Depression t-score") +
  theme_minimal()

#scatter plot of US standing and mh23
mp_US_plot <- ggplot(redcap_subset, aes(x = macarthur_ladder_us, y = mh23)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(x = "SSS in U.S. (1–10)", y = "Menstrual pain (VAS)") +
  theme_minimal()

#scatter plot of community standing and mh23
mp_comm_plot <- ggplot(redcap_subset, aes(x = macarthur_ladder_community, y = mh23)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(x = "SSS in community (1–10)", y = "Menstrual pain (VAS)") +
  theme_minimal()

#combine plots into grid
grid_2x3 <- (anx_US_plot + dep_US_plot + mp_US_plot) /
  (anx_comm_plot + dep_comm_plot + mp_comm_plot)

ggsave("plots/ses_2x3_plot.png", plot = grid_2x3)

grid_3x2 <- (anx_US_plot + anx_comm_plot) /
  (dep_US_plot + dep_comm_plot) /
  (mp_US_plot + mp_comm_plot)

ggsave("plots/ses_3x2_plot.png", plot = grid_3x2)


