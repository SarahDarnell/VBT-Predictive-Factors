#VBT Predictive Factors ~ part 1
#Written by Sarah Darnell, last modified 7.25.25

library(readr)
library(dplyr)
library(tableone)
library(flextable)
library(officer)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/VBT Predictive Factors CRAMPP2")

#import redcap pull of variables
redcap <- read_csv("Raw files/EH22236CRAMPP2-VBTPredictiveFactors_DATA_2025-07-18_1444.csv", 
                   col_types = cols(redcap_survey_identifier = col_skip(), 
                                    participant_status_timestamp = col_skip()))

#import age
age <- read_csv("Raw files/age.csv")

#merge age into redcap dataset, for virtual_assessment_arm_1 event only
redcap <- redcap %>%
  left_join(age, by = "record_id") %>%
  mutate(Age = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                      Age, NA))
#########################
##Table 1: Demographics##
#########################

#recode variables
#group, and also add to the other events 
redcap <- redcap %>%
  mutate(group_arm2 = case_match(
    group_arm2, 
    1 ~ "Dysmenorrhea", 
    2 ~ "Pain Free Control", 
    3 ~ "Dysmenorrhea plus Bladder Pain"
  )) %>%
  rename(Group = group_arm2) %>%
  group_by(record_id) %>%
  mutate(Group = first(Group)) %>%
  slice(-1) %>%
  ungroup()
#assigned sex
redcap <- redcap %>%
  mutate(mh_assigned_sex = case_match(
    mh_assigned_sex, 
    1 ~ "Female"
  )) 
#gender
redcap <- redcap %>%
  mutate(multi_gender = ifelse(rowSums(
    select(., mh_gender___1:mh_gender___8)) > 1, 1, 0)) %>%
  mutate(mh_gender___1 = ifelse((mh_gender___1 == 1) &
                               (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender___2 = ifelse((mh_gender___2 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender___3 = ifelse((mh_gender___3 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender___4 = ifelse((mh_gender___4 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender___5 = ifelse((mh_gender___5 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender___6 = ifelse((mh_gender___6 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender___7 = ifelse((mh_gender___7 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender___8 = ifelse((mh_gender___8 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(Gender = case_when(
    multi_gender == 1 ~ "Multiple Genders", 
    mh_gender___1 == 1 ~ "Woman", 
    mh_gender___2 == 1 ~ "Man", 
    mh_gender___3 == 1 ~ "Transgender Woman/Trans Femme", 
    mh_gender___4 == 1 ~ "Transgender Man/Trans Masc", 
    mh_gender___5 == 1 ~ "Nonbinary/Genderqueer/Gender Expansive",
    mh_gender___6 == 1 ~ "Two-spirit",
    mh_gender___7 == 1 | mh_gender___8 == 1 ~ "Unknown"
  ))
#race
redcap <- redcap %>%
  mutate(multi_race = ifelse(rowSums(
    select(., mh3_race___1:mh3_race___7)) > 1, 1, 0)) %>%
  mutate(mh3_race___1 = ifelse((mh3_race___1 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race___2 = ifelse((mh3_race___2 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race___3 = ifelse((mh3_race___3 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race___4 = ifelse((mh3_race___4 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race___5 = ifelse((mh3_race___5 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race___6 = ifelse((mh3_race___6 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race___7 = ifelse((mh3_race___7 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(Race = case_when(
    multi_race == 1 ~ "Multiple Races", 
    mh3_race___1 == 1 ~ "Other", 
    mh3_race___2 == 1 ~ "Asian", 
    mh3_race___3 == 1 ~ "Other", 
    mh3_race___4 == 1 ~ "Black", 
    mh3_race___5 == 1 ~ "White",
    mh3_race___6 == 1 ~ "Unknown",
    mh3_race___7 == 1 | mh_gender___8 == 1 ~ "Other"
  ))
#ethnicity
redcap <- redcap %>%
  mutate(mh4_ethnicity = case_match(
    mh4_ethnicity, 
    1 ~ "Hispanic or Latino", 
    2 ~ "Not Hispanic or Latino", 
    3 ~ "Uknown"
  )) 
#education
redcap <- redcap %>%
  mutate(mh5_education = case_match(
    mh5_education, 
    1 ~ "Grade School", 
    2 ~ "High School", 
    3 ~ "Some College", 
    4 ~ "Associate's Degree", 
    5 ~ "Bachelor's Degree", 
    6 ~ "Post-Graduate Degree"
  )) 
#employment
redcap <- redcap %>%
  mutate(multi_employment = ifelse(rowSums(
    select(., mh6_employment___1:mh6_employment___7)) > 1, 1, 0)) %>%
  mutate(mh6_employment___1 = ifelse((mh6_employment___1 == 1) &
                                 (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment___2 = ifelse((mh6_employment___2 == 1) &
                                 (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment___3 = ifelse((mh6_employment___3 == 1) &
                                 (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment___4 = ifelse((mh6_employment___4 == 1) &
                                 (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment___5 = ifelse((mh6_employment___5 == 1) &
                                 (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment___6 = ifelse((mh6_employment___6 == 1) &
                                 (multi_employment != 1), 1, 0)) %>%
  mutate(mh6_employment___7 = ifelse((mh6_employment___7 == 1) &
                                 (multi_employment != 1), 1, 0)) %>%
  mutate(Employment = case_when(
    multi_employment == 1 ~ "Multiple Employment Statuses", 
    mh6_employment___1 == 1 ~ "Work Full-Time", 
    mh6_employment___2 == 1 ~ "Work Part-Time", 
    mh6_employment___3 == 1 ~ "Homemaker", 
    mh6_employment___4 == 1 ~ "Retired", 
    mh6_employment___5 == 1 ~ "Disabled",
    mh6_employment___6 == 1 ~ "Student",
    mh6_employment___7 == 1 ~ "Unemployed"
  ))
#Smoking
redcap <- redcap %>%
  mutate(mh_smoking = case_match(
    mh_smoking, 
    1 ~ "Yes", 
    0 ~ "No"
  )) 
#THC
redcap <- redcap %>%
  mutate(ms_thc = case_match(
    ms_thc, 
    1 ~ "Yes", 
    0 ~ "No"
  )) 
#Problem with drugs or alcohol
redcap <- redcap %>%
  mutate(mh9b1 = case_match(
    mh9b1, 
    1 ~ "Yes", 
    0 ~ "No"
  )) 
#births, also forces text input into numerics (visually inspected, a few pts
#wrote in 0, so coercing text to 0)
redcap <- redcap %>%
  mutate(across(
    c(mh15_vagbirths, mh14_csection),
    ~ as.numeric(ifelse(
      redcap_event_name == "virtual_assessment_arm_1",
      ifelse(is.na(as.numeric(.x)), 0, as.numeric(.x)),
      .x
    ))
  )) 
#income
redcap <- redcap %>%
  mutate(mh_income = case_match(
    mh_income, 
    1 ~ "< $25,000", 
    2 ~ "$25,000 - < $50,000", 
    3 ~ "$50,000 - < $75,000", 
    4 ~ "$75,000 - < $100,000", 
    5 ~ "$100,000 - < $150,000", 
    6 ~ "$150,000 or greater", 
    7 ~ "Unknown"
  )) 
  
#saving file
write_csv(redcap, "Edited data files/redcap.csv")  

#Defining vars for table 1
redcap_table1 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Assigned Sex at Birth` = mh_assigned_sex, 
    Ethnicity = mh4_ethnicity, 
    Education = mh5_education, 
    `Nicotine Usage` = mh_smoking, 
    `THC (Marijuana) Usage` = ms_thc, 
    `Have you ever had a problem with drugs or alcohol?` = mh9b1, 
    `Vaginal Births` = mh15_vagbirths, 
    `Caesarean Section Births` = mh14_csection, 
    Income = mh_income
  )

vars <- c("Age", "Assigned Sex at Birth", "Gender", "Race", "Ethnicity", 
          "Education", "Employment", "Income", "Vaginal Births", 
          "Caesarean Section Births", "Nicotine Usage", "THC (Marijuana) Usage", 
          "Have you ever had a problem with drugs or alcohol?")
factor_vars <- c("Assigned Sex at Birth", "Gender", "Race", "Ethnicity", 
                 "Education", "Employment", "Income", "Nicotine Usage", 
                 "THC (Marijuana) Usage", 
                 "Have you ever had a problem with drugs or alcohol?")


#Creating table
demo <- CreateTableOne(vars, data = redcap_table1, factorVars = factor_vars, 
                       strata = "Group")

demo_df <- as.data.frame(print(demo, 
                               nonnormal = "Age",
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE))

# Remove p-value/test columns
cols_to_remove <- c("p", "test")
demo_df <- demo_df[, !colnames(demo_df) %in% cols_to_remove]

#Step 1: save rownames as a column
demo_df <- data.frame(rowname = rownames(demo_df), demo_df, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(demo_df))) {
  row_label <- demo_df$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- demo_df[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- demo_df[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- demo_df[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(demo_df) %>%
  bold(i = which(demo_df$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(demo_df), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table1.docx")

######################################################################
##Table 2: Menstrual Pain Characteristics and Hormonal Therapy Usage##
######################################################################

#recode variables
#ocps
redcap <- redcap %>%
  mutate(mh_ocps = case_match(
    mh_ocps, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#patch
redcap <- redcap %>%
  mutate(mh_patch = case_match(
    mh_patch, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#ring
redcap <- redcap %>%
  mutate(mh_ring = case_match(
    mh_ring, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#implant
redcap <- redcap %>%
  mutate(mh_implant = case_match(
    mh_implant, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#shot
redcap <- redcap %>%
  mutate(mh_shot = case_match(
    mh_shot, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#pills
redcap <- redcap %>%
  mutate(mh_p_pills = case_match(
    mh_p_pills, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#gnrh agonist
redcap <- redcap %>%
  mutate(mh_gnrh_agonist = case_match(
    mh_gnrh_agonist, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  ))
#gnrh antagonist
redcap <- redcap %>%
  mutate(mh_gnrh_antagonist = case_match(
    mh_gnrh_antagonist, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#hormonal iud
redcap <- redcap %>%
  mutate(mh_iudh = case_match(
    mh_iudh, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#copper iud
redcap <- redcap %>%
  mutate(mh_iudc = case_match(
    mh_iudc, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  ))
#Aromatase
redcap <- redcap %>%
  mutate(mh_aromatase = case_match(
    mh_aromatase, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  )) 
#other
redcap <- redcap %>%
  mutate(mh_hormonal_other = case_match(
    mh_hormonal_other, 
    1 ~ "Currently taking", 
    2 ~ "Have taken in the past"
  ))
#period in last 6 months
redcap <- redcap %>%
  mutate(mh_period_6months = case_match(
    mh_period_6months, 
    1 ~ "Yes", 
    0 ~ "No"
  )) 
#painful periods
redcap <- redcap %>%
  mutate(mh18_painfulperiodsyn = case_match(
    mh18_painfulperiodsyn, 
    1 ~ "Yes", 
    0 ~ "No"
  )) 
#painful periods at time of menarche
redcap <- redcap %>%
  mutate(mh19 = case_match(
    mh19, 
    1 ~ "Yes", 
    0 ~ "No"
  )) 
#total years without a period (making new var with only 0-10 numeric, keeping
#old var for reference later)
redcap <- redcap %>%
  mutate(mh19b = case_when(
    mh19b == 999 ~ NA, 
    mh19b == 0 ~ NA, 
    mh19b == 11 ~ NA, 
    TRUE ~ mh19b
  ))
#menstrual cycle regularity 
redcap <- redcap %>%
  mutate(mh24 = case_match(
    mh24, 
    0 ~ "Usually or always irregular", 
    1 ~ "Sometimes irregular", 
    2 ~ "Always regular", 
    4 ~ "Using a type of birth control that prevents periods"
  ))
#menstrual cycle length
redcap <- redcap %>%
  mutate(mh25 = case_match(
    mh25, 
    1 ~ "1-21 days", 
    2 ~ "22-35 days", 
    3 ~ "Greater than 35 days"
  ))
#menstrual phase
redcap <- redcap %>%
  mutate(
    vbt_instructions_and_urgency_zones_timestamp = 
      as.POSIXct(vbt_instructions_and_urgency_zones_timestamp, 
                 format = "%Y-%m-%d %H:%M:%S"),
    mh26 = as.Date(mh26),
    `day of cycle` = floor(abs(as.numeric(difftime(
      vbt_instructions_and_urgency_zones_timestamp, mh26, units = "days"))))
  ) %>%
  mutate(`Menstrual Cycle Phase` = case_when(
    `day of cycle` <= 14 ~ "Follicular Phase", 
    `day of cycle` >= 15 ~ "Luteal Phase"
  ))
#avg days of bleeding per period (making new var with only 0-10 numeric, 
#keeping old var for reference later)
redcap <- redcap %>%
  mutate(mh27 = case_when(
    mh27 == 11 ~ NA, 
    TRUE ~ mh27
  ))
#Hormonal therapy to treat menstrual pain
redcap <- redcap %>%
  mutate(mh28 = case_match(
    mh28, 
    1 ~ "Yes", 
    0 ~ "No"
  )) 
#bladder, bowel, abdominal pain outside of period
redcap <- redcap %>%
  mutate(mh30 = case_match(
    mh30, 
    1 ~ "Yes", 
    0 ~ "No"
  ))
#tampon test pain (making new var with only 0-10 numeric, 
#keeping old var for reference later)
redcap <- redcap %>%
  mutate(tampon_test = case_when(
    tampon_test == 99 ~ NA, 
    tampon_test == 0 ~ 0,
    tampon_test == 10 ~ 10, 
    TRUE ~ tampon_test
  ))

#saving file
write_csv(redcap, "Edited data files/redcap.csv")  

#Defining vars for table 1
redcap_table2 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Oral Contraceptive Pills` = mh_ocps,
    `Contraceptive Patch` = mh_patch,
    `Vaginal Ring` = mh_ring,
    `Progestin Implant` = mh_implant,
    `Progestin Shot` = mh_shot,
    `Progestin Pills` = mh_p_pills,
    `GnRH Agonists` = mh_gnrh_agonist,
    `GnRH Antagonists` = mh_gnrh_antagonist,
    `Hormonal IUD` = mh_iudh,
    `Copper IUD` = mh_iudc,
    `Aromatase Inhibitors` = mh_aromatase,
    `Other Hormonal Medication` = mh_hormonal_other,
    `Period in the last 6 months?` = mh_period_6months,
    `Ever had painful periods?` = mh18_painfulperiodsyn,
    `Painful periods at time of menarche?` = mh19,
    `What age did your painful periods start, if not menarche?` = mh19a,
    `Years since menarche without a period` = mh19b,
    `Days with menstrual pelvic pain >= 4 in an average month` = mh20,
    `Days of missed work, school, or activities due to painful period in last 3 months` = mh21,
    `Days spent in bed due to painful period in last 3 months` = mh22,
    `Average pain during worst day of period when not taking pain relievers in last 3 months (VAS)` = mh23,
    `Average pain during worst day of period when taking NSAID pain relievers in last 3 months (VAS)` = mh23a,
    `Average pain during worst day of period when taking acetaminophen pain relievers in last 3 months (VAS)` = mh27b,
    `Menstrual Cycle Regularity` = mh24,
    `Menstrual Cycle Length` = mh25,
    `Average bleeding days per period` = mh27,
    `Used hormonal therpary to treat menstrual pain` = mh28,
    `Bladder, bowel, or abdomino-pelvic pain outside of period in last 3 months` = mh30,
    `Pain with tampon test` = tampon_test
  )

vars <- c("Oral Contraceptive Pills", "Contraceptive Patch", "Vaginal Ring", 
          "Progestin Implant", "Progestin Shot", "Progestin Pills", 
          "GnRH Agonists", "GnRH Antagonists", "Hormonal IUD", "Copper IUD", 
          "Aromatase Inhibitors", "Other Hormonal Medication", 
          "Period in the last 6 months?", "Ever had painful periods?", 
          "Painful periods at time of menarche?", 
          "What age did your painful periods start, if not menarche?", 
          "Years since menarche without a period", 
          "Days with menstrual pelvic pain >= 4 in an average month", 
          "Days of missed work, school, or activities due to painful period in last 3 months", 
          "Days spent in bed due to painful period in last 3 months", 
          "Average pain during worst day of period when not taking pain relievers in last 3 months (VAS)", 
          "Average pain during worst day of period when taking NSAID pain relievers in last 3 months (VAS)", 
          "Average pain during worst day of period when taking acetaminophen pain relievers in last 3 months (VAS)", 
          "Menstrual Cycle Regularity", "Menstrual Cycle Length", 
          "Menstrual Cycle Phase", "Average bleeding days per period", 
          "Used hormonal therpary to treat menstrual pain", 
          "Bladder, bowel, or abdomino-pelvic pain outside of period in last 3 months", 
          "Pain with tampon test")

factor_vars <- c("Oral Contraceptive Pills", "Contraceptive Patch", "Vaginal Ring", 
                 "Progestin Implant", "Progestin Shot", "Progestin Pills", 
                 "GnRH Agonists", "GnRH Antagonists", "Hormonal IUD", "Copper IUD", 
                 "Aromatase Inhibitors", "Other Hormonal Medication", 
                 "Period in the last 6 months?", "Ever had painful periods?", 
                 "Painful periods at time of menarche?", "Menstrual Cycle Regularity", 
                 "Menstrual Cycle Length", "Menstrual Cycle Phase",
                  "Used hormonal therpary to treat menstrual pain", 
                  "Bladder, bowel, or abdomino-pelvic pain outside of period in last 3 months")

#Creating table
demo <- CreateTableOne(vars, data = redcap_table2, factorVars = factor_vars, 
                       strata = "Group")

demo_df <- as.data.frame(print(demo, 
                               nonnormal = c("What age did your painful periods start, if not menarche?",
                                             "Years since menarche without a period",
                                             "Days with menstrual pelvic pain >= 4 in an average month", 
                                             "Days of missed work, school, or activities due to painful period in last 3 months", 
                                             "Days spent in bed due to painful period in last 3 months",
                                             "Average pain during worst day of period when not taking pain relievers in last 3 months (VAS)",
                                             "Average pain during worst day of period when taking NSAID pain relievers in last 3 months (VAS)",
                                             "Average pain during worst day of period when taking acetaminophen pain relievers in last 3 months (VAS)", 
                                             "Pain with tampon test"),
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE))

# Remove p-value/test columns
cols_to_remove <- c("p", "test")
demo_df <- demo_df[, !colnames(demo_df) %in% cols_to_remove]

#Step 1: save rownames as a column
demo_df <- data.frame(rowname = rownames(demo_df), demo_df, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(demo_df))) {
  row_label <- demo_df$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- demo_df[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- demo_df[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- demo_df[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(demo_df) %>%
  bold(i = which(demo_df$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(demo_df), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table2.docx")
