#VBT Predictive Factors Sub-analyses - MPA projects for ZFK and KJ
##Written by Sarah Darnell, last modified 10.28.25

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
redcap <- read_csv("Edited data files/redcap_post_supplementary_vars.csv")

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
#ggplot(redcap_subset, aes(macarthur_ladder_us)) + geom_histogram()

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

##Additional changes for MPA conference##

#Prepping vars
#Education, regrouping to have 3 levels instead of 6
redcap_subset$mh5_education <- as.factor(redcap_subset$mh5_education)

redcap_subset <- redcap_subset %>%
  mutate(`education_mpa` = case_when(
    mh5_education == "Grade School" | 
      mh5_education == "High School" | 
      mh5_education == "Some College" 
    ~ "Some college or less", 
    mh5_education == "Associate's Degree" |
      mh5_education == "Bachelor's Degree" 
    ~ "College Degree", 
    mh5_education == "Post-Graduate Degree" 
    ~ "Post-Graduate Degree"
  ))

#Employment, regrouping to have 2 levels instead of 7
redcap_subset <- redcap_subset %>%
  mutate(multi_employment = ifelse(rowSums(
    select(., mh6_employment___1:mh6_employment___7)) > 1, 1, 0)) %>%
  mutate(mh6_employment_1_revised = ifelse((mh6_employment___1 == 1) &
                                             (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment_2_revised = ifelse((mh6_employment___2 == 1) &
                                             (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment_3_revised = ifelse((mh6_employment___3 == 1) &
                                             (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment_4_revised = ifelse((mh6_employment___4 == 1) &
                                             (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment_5_revised = ifelse((mh6_employment___5 == 1) &
                                             (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment_6_revised = ifelse((mh6_employment___6 == 1) &
                                             (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment_7_revised = ifelse((mh6_employment___7 == 1) &
                                             (multi_employment != 1), 1, 0)) %>%
  mutate(Employment_mpa = case_when(
    multi_employment == 1 & (mh6_employment___1 == 1 | mh6_employment___2 == 1)
    ~ "Employed", 
    multi_employment == 1 & (mh6_employment___1 != 1 | mh6_employment___2 != 1)
    ~ "Unemployed", 
    mh6_employment_1_revised == 1 ~ "Employed", 
    mh6_employment_2_revised == 1 ~ "Employed", 
    mh6_employment_3_revised == 1 ~ "Unemployed", 
    mh6_employment_4_revised == 1 ~ "Unemployed", 
    mh6_employment_5_revised == 1 ~ "Unemployed",
    mh6_employment_6_revised == 1 ~ "Unemployed",
    mh6_employment_7_revised == 1 ~ "Unemployed"
  ))

#demo table, stratified by THC usage 
demo_vars <- c("Age", 
              "Race", 
              "mh4_ethnicity", 
              "education_mpa", 
              "Employment_mpa", 
              "mh_income",
              "promis_anx_t_score",
              "promis_dep_t_score",
              "promis_sd_t_score",
              "mh23",
              "vbt_fu_pain"
              )

demo_factor_vars <- c("Race", 
                      "mh4_ethnicity", 
                      "education_mpa", 
                      "Employment_mpa", 
                      "mh_income"
                      )

demo_nonnormal_vars <- c("Age",
                         "promis_anx_t_score",
                         "promis_dep_t_score",
                         "promis_sd_t_score",
                         "mh23",
                         "vbt_fu_pain"
                          )


demo_mpa_thc <- CreateTableOne(demo_vars, data = redcap_subset, 
                               factorVars = demo_factor_vars, 
                       strata = "ms_thc")

demo_mpa_thc_df <- as.data.frame(print(demo_mpa_thc, 
                               nonnormal = demo_nonnormal_vars,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE))


#Step 1: save rownames as a column
demo_mpa_thc_df <- data.frame(rowname = rownames(demo_mpa_thc_df), 
                              demo_mpa_thc_df, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(demo_mpa_thc_df))) {
  row_label <- demo_mpa_thc_df$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- demo_mpa_thc_df[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- demo_mpa_thc_df[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- demo_mpa_thc_df[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(demo_mpa_thc_df) %>%
  bold(i = which(demo_mpa_thc_df$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(demo_mpa_thc_df), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/MPA/thc_demographics.docx")

##Linear regression models
#Set order levels for education
redcap_subset$education_mpa <- factor(
  redcap_subset$education_mpa,
  levels = c("Some college or less",
             "College Degree",
             "Post-Graduate Degree")
)

#create 0/1 variable for hispanic ethnicity
redcap_subset <- redcap_subset %>%
  mutate(hispanic_mpa = case_when(
    mh4_ethnicity == "Hispanic or Latino" ~ 1, 
    mh4_ethnicity != "Hispanic or Latino" ~ 0
  ))

#Model #1: anxiety ~ White + Hispanic + Education  + THC use
thc_model_1 <- lm(promis_anx_t_score ~ 
                    mh3_race_5_revised + 
                    hispanic_mpa +
                    education_mpa +
                    ms_thc, 
                  data = redcap_subset)
#print summary of model
summary(thc_model_1)

#Model #2: anxiety ~ Asian + Hispanic + Education  + THC use
thc_model_2 <- lm(promis_anx_t_score ~ 
                    mh3_race_2_revised + 
                    hispanic_mpa +
                    education_mpa +
                    ms_thc, 
                  data = redcap_subset)
#print summary of model
summary(thc_model_2)

#Model #3: depression ~ White + Hispanic + Education  + THC use
thc_model_3 <- lm(promis_dep_t_score ~ 
                    mh3_race_5_revised + 
                    hispanic_mpa +
                    education_mpa +
                    ms_thc, 
                  data = redcap_subset)
#print summary of model
summary(thc_model_3)

#Model #4: depression ~ Asian + Hispanic + Education  + THC use
thc_model_4 <- lm(promis_dep_t_score ~ 
                    mh3_race_2_revised + 
                    hispanic_mpa +
                    education_mpa +
                    ms_thc, 
                  data = redcap_subset)
#print summary of model
summary(thc_model_4)

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


##Additional changes for MPA conference##

#create low and high SSS variable
redcap_subset <- redcap_subset %>%
  mutate(sss_group = case_when(
    macarthur_ladder_us < 6 ~ "Low SSS", 
    macarthur_ladder_us > 5 ~ "High SSS"))

#create demo table, stratified by SSS group (using same variables as thc project)

demo_mpa_ses <- CreateTableOne(demo_vars, data = redcap_subset, 
                               factorVars = demo_factor_vars, 
                               strata = "sss_group")

demo_mpa_ses_df <- as.data.frame(print(demo_mpa_ses, 
                                       nonnormal = demo_nonnormal_vars,
                                       printToggle = FALSE,
                                       quote = FALSE,
                                       noSpaces = TRUE,
                                       showAllLevels = TRUE))


#Step 1: save rownames as a column
demo_mpa_ses_df <- data.frame(rowname = rownames(demo_mpa_ses_df), 
                              demo_mpa_ses_df, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(demo_mpa_ses_df))) {
  row_label <- demo_mpa_ses_df$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- demo_mpa_ses_df[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- demo_mpa_ses_df[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- demo_mpa_ses_df[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(demo_mpa_ses_df) %>%
  bold(i = which(demo_mpa_ses_df$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(demo_mpa_ses_df), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/MPA/ses_demographics.docx")

##Linear regression models
#Set order levels for education and income
redcap_subset$education_mpa <- factor(
  redcap_subset$education_mpa,
  levels = c("Some college or less",
             "College Degree",
             "Post-Graduate Degree")
)

redcap_subset$mh_income <- factor(
  redcap_subset$mh_income,
  levels = c("< $25,000",
             "$25,000 - < $50,000",
             "$50,000 - < $75,000",
             "$75,000 - < $100,000",
             "$100,000 - < $150,000",
             "$150,000 or greater",
             "Unknown")
)

#Model #1: anxiety ~ Black + Education + Income + SSS_US
ses_model_1 <- lm(promis_anx_t_score ~ 
                    mh3_race_4_revised + 
                    education_mpa +
                    mh_income + 
                    macarthur_ladder_us, 
                  data = redcap_subset)
#print summary of model
summary(ses_model_1)

#Model #2: depression ~ Black + Education + Income + SSS_US
ses_model_2 <- lm(promis_dep_t_score ~ 
                    mh3_race_4_revised + 
                    education_mpa +
                    mh_income + 
                    macarthur_ladder_us, 
                  data = redcap_subset)
#print summary of model
summary(ses_model_2)

#Model #3: Sleep disturbance ~ Black + Education + Income + SSS_US
ses_model_3 <- lm(promis_sd_t_score ~ 
                    mh3_race_4_revised + 
                    education_mpa +
                    mh_income + 
                    macarthur_ladder_us, 
                  data = redcap_subset)
#print summary of model
summary(ses_model_3)

#Model #4: mh23 ~ Black + Education + Income + SSS_US
ses_model_4 <- lm(mh23 ~ 
                    mh3_race_4_revised + 
                    education_mpa +
                    mh_income + 
                    macarthur_ladder_us, 
                  data = redcap_subset)
#print summary of model
summary(ses_model_4)

