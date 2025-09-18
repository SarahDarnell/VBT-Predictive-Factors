#VBT Predictive Factors ~ part 1
#Written by Sarah Darnell, last modified 9.5.25

library(readr)
library(dplyr)
library(tableone)
library(flextable)
library(officer)
library(stringr)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/VBT Predictive Factors CRAMPP2")

#import redcap pull of variables
redcap <- read_csv("Raw files/EH22236CRAMPP2-VBTPredictiveFactors_DATA_2025-09-04_1051.csv", 
                   col_types = cols(redcap_survey_identifier = col_skip(), 
                                    participant_status_timestamp = col_skip()))

#import age
age <- read_csv("Raw files/age.csv")

#merge age into redcap dataset, for virtual_assessment_arm_1 event only
redcap <- redcap %>%
  left_join(age, by = "record_id") %>%
  mutate(Age = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                      Age, NA))

#remove invalid records (see analysis notes)
redcap <- redcap %>%
  filter(record_id != 1154)

#########################
##Table 1: Demographics##
#########################

#recode variables
#group, and also add to the other events 
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    group_arm2 = first(na.omit(group_arm2))
  ) %>%
  ungroup() %>%
  mutate(group_arm2 = case_match(
    group_arm2, 
    1 ~ "Dysmenorrhea", 
    2 ~ "Pain Free Control", 
    3 ~ "Dysmenorrhea plus Bladder Pain"
  )) %>%
  rename(Group = group_arm2)
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
write_csv(redcap, "Edited data files/redcap_post_table1.csv")  

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
  mutate(mh_ocps = case_when(
    mh_ocps == 1 ~ "Currently taking",
    mh_ocps == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_ocps) ~ "Have never taken"
  )) 
#patch
redcap <- redcap %>%
  mutate(mh_patch = case_when(
    mh_patch == 1 ~ "Currently taking",
    mh_patch == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_patch) ~ "Have never taken"
  )) 
#ring
redcap <- redcap %>%
  mutate(mh_ring = case_when(
    mh_ring == 1 ~ "Currently taking",
    mh_ring == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_ring) ~ "Have never taken"
  )) 
#implant
redcap <- redcap %>%
  mutate(mh_implant = case_when(
    mh_implant == 1 ~ "Currently taking",
    mh_implant == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_implant) ~ "Have never taken"
  )) 
#shot
redcap <- redcap %>%
  mutate(mh_shot = case_when(
    mh_shot == 1 ~ "Currently taking",
    mh_shot == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_shot) ~ "Have never taken"
  )) 
#pills
redcap <- redcap %>%
  mutate(mh_p_pills = case_when(
    mh_p_pills == 1 ~ "Currently taking",
    mh_p_pills == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_p_pills) ~ "Have never taken"
  )) 
#gnrh agonist
redcap <- redcap %>%
  mutate(mh_gnrh_agonist = case_when(
    mh_gnrh_agonist == 1 ~ "Currently taking",
    mh_gnrh_agonist == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_gnrh_agonist) ~ "Have never taken"
  ))
#gnrh antagonist
redcap <- redcap %>%
  mutate(mh_gnrh_antagonist = case_when(
    mh_gnrh_antagonist == 1 ~ "Currently taking",
    mh_gnrh_antagonist == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_gnrh_antagonist) ~ "Have never taken"
  )) 
#hormonal iud
redcap <- redcap %>%
  mutate(mh_iudh = case_when(
    mh_iudh == 1 ~ "Currently taking",
    mh_iudh == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_iudh) ~ "Have never taken"
  )) 
#copper iud
redcap <- redcap %>%
  mutate(mh_iudc = case_when(
    mh_iudc == 1 ~ "Currently taking",
    mh_iudc == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_iudc) ~ "Have never taken"
  ))
#Aromatase
redcap <- redcap %>%
  mutate(mh_aromatase = case_when(
    mh_aromatase == 1 ~ "Currently taking",
    mh_aromatase == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_aromatase) ~ "Have never taken"
  )) 
#other
redcap <- redcap %>%
  mutate(mh_hormonal_other = case_when(
    mh_hormonal_other == 1 ~ "Currently taking",
    mh_hormonal_other == 2 ~ "Have taken in the past",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_hormonal_other) ~ "Have never taken"
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
#days of pain above a 4, fixed so that when mh18_painfulperiodsyn == 0, mh20 == 0
redcap <- redcap %>%
  mutate(mh20 = case_when(
    mh18_painfulperiodsyn == "No" ~ 0, 
    TRUE ~ mh20
  ))
#painful periods at time of menarche
redcap <- redcap %>%
  mutate(mh19 = case_when(
    mh19 == 1 ~ "Yes",
    mh19 == 0 ~ "No",
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh19) ~ "Have never had painful periods"
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
write_csv(redcap, "Edited data files/redcap_post_table2.csv")  

#Defining vars for table 2
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

#Creating summary table 2
sum <- CreateTableOne(vars, data = redcap_table2, factorVars = factor_vars, 
                       strata = "Group")

sum_df <- as.data.frame(print(sum, 
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
                               showAllLevels = TRUE, 
                               pValues = FALSE))

#Creating table with comparisons for DYS and DYSB
redcap_table2_p <- redcap_table2 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table2_p, factorVars = factor_vars, 
                       strata = "Group")

comp_df <- as.data.frame(print(comp, 
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
                               showAllLevels = TRUE, 
                               pValues = TRUE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table2 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#reformat and save table
#Step 1: save rownames as a column
table2 <- data.frame(rowname = rownames(table2), table2, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(table2))) {
  row_label <- table2$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- table2[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- table2[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- table2[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(table2) %>%
  bold(i = which(table2$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(table2), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table2.docx")

############################
##Table 3: Medical History##
############################

#converting some of the diagnoses variables from logical to numeric
logicals <- c("mh_anx", "mh_dep", "mh_cancer", "mh_ibs", "mh_kidney_stone", 
              "mh_endo", "mh_fibroids", "mh_cysts", "mh_diarrhea", "mh_uti", 
              "mh_vag_infection", "mh_child_pelvic", "mh_heart", "mh_kidney", 
              "mh_lung", "mh_liver", "mh_pid", "mh_constipation", 
              "mh_cyst_surg", "mh_ovary_surg", "mh_vaginal_surg", 
              "mh_other_surg", "mh_fibroid_surg")
redcap <- redcap %>%
  mutate(across(all_of(logicals), as.numeric))

#recode variables
#medical insurance
redcap <- redcap %>%
  mutate(mh_insurance_yn = case_match(
    mh_insurance_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#regular physician
redcap <- redcap %>%
  mutate(mh_physician_yn = case_match(
    mh_physician_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#diagnosed anxiety
redcap <- redcap %>%
  mutate(mh_anx = case_when(
    mh_anx == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_anx) ~ "No"
  ))
#diagnosed depression
redcap <- redcap %>%
  mutate(mh_dep = case_when(
    mh_dep == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_dep) ~ "No"
  ))
#diagnosed cancer
redcap <- redcap %>%
  mutate(mh_cancer = case_when(
    mh_cancer == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_cancer) ~ "No"
  ))
#diagnosed ibs
redcap <- redcap %>%
  mutate(mh_ibs = case_when(
    mh_ibs == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_ibs) ~ "No"
  ))
#diagnosed kidney stone
redcap <- redcap %>%
  mutate(mh_kidney_stone = case_when(
    mh_kidney_stone == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_kidney_stone) ~ "No"
  ))
#diagnosed endo
redcap <- redcap %>%
  mutate(mh_endo = case_when(
    mh_endo == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_endo) ~ "No"
  ))
#diagnosed fibroids
redcap <- redcap %>%
  mutate(mh_fibroids = case_when(
    mh_fibroids == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_fibroids) ~ "No"
  ))
#diagnosed cysts
redcap <- redcap %>%
  mutate(mh_cysts = case_when(
    mh_cysts == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_cysts) ~ "No"
  ))
#diagnosed PID
redcap <- redcap %>%
  mutate(mh_pid = case_when(
    mh_pid == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_pid) ~ "No"
  ))
#diagnosed constipation
redcap <- redcap %>%
  mutate(mh_constipation = case_when(
    mh_constipation == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_constipation) ~ "No"
  ))
#diagnosed diarrhea
redcap <- redcap %>%
  mutate(mh_diarrhea = case_when(
    mh_diarrhea == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_diarrhea) ~ "No"
  ))
#diagnosed uti
redcap <- redcap %>%
  mutate(mh_uti = case_when(
    mh_uti == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_uti) ~ "No"
  ))
#diagnosed vaginal infection
redcap <- redcap %>%
  mutate(mh_vag_infection = case_when(
    mh_vag_infection == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_vag_infection) ~ "No"
  ))
#diagnosed child pelvic problem
redcap <- redcap %>%
  mutate(mh_child_pelvic = case_when(
    mh_child_pelvic == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_child_pelvic) ~ "No"
  ))
#diagnosed heart condition
redcap <- redcap %>%
  mutate(mh_heart = case_when(
    mh_heart == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_heart) ~ "No"
  ))
#diagnosed kidney condition
redcap <- redcap %>%
  mutate(mh_kidney = case_when(
    mh_kidney == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_kidney) ~ "No"
  ))
#diagnosed lung condition
redcap <- redcap %>%
  mutate(mh_lung = case_when(
    mh_lung == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_lung) ~ "No"
  ))
#diagnosed liver condition
redcap <- redcap %>%
  mutate(mh_liver = case_when(
    mh_liver == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_liver) ~ "No"
  ))
#fibroid surg
redcap <- redcap %>%
  mutate(mh_fibroid_surg = case_when(
    mh_fibroid_surg == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_fibroid_surg) ~ "No"
  ))
#cyst surg
redcap <- redcap %>%
  mutate(mh_cyst_surg = case_when(
    mh_cyst_surg == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_cyst_surg) ~ "No"
  ))
#ovary surg
redcap <- redcap %>%
  mutate(mh_ovary_surg = case_when(
    mh_ovary_surg == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_ovary_surg) ~ "No"
  ))
#vaginal surg
redcap <- redcap %>%
  mutate(mh_vaginal_surg = case_when(
    mh_vaginal_surg == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_vaginal_surg) ~ "No"
  ))
#other pelvic surg
redcap <- redcap %>%
  mutate(mh_other_surg = case_when(
    mh_other_surg == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_other_surg) ~ "No"
  ))
#acetaminophen use
redcap <- redcap %>%
  mutate(mh_acetaminophen_yn = case_match(
    mh_acetaminophen_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#acetaminophen amounts
redcap <- redcap %>%
  mutate(mh_acetaminophen_amount = case_when(
    mh_acetaminophen_amount == 1 ~ "1-2", 
    mh_acetaminophen_amount == 2 ~ "3-5", 
    mh_acetaminophen_amount == 3 ~ "6-14", 
    mh_acetaminophen_amount == 4 ~ "15+", 
    mh_acetaminophen_yn == "No" ~ "0"
  )) 
#ibuprofen use
redcap <- redcap %>%
  mutate(mh_ibuprofen_yn = case_match(
    mh_ibuprofen_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#ibuprofen amounts
redcap <- redcap %>%
  mutate(mh_ibuprofen_amount = case_when(
    mh_ibuprofen_amount == 1 ~ "1-2", 
    mh_ibuprofen_amount == 2 ~ "3-5", 
    mh_ibuprofen_amount == 3 ~ "6-14", 
    mh_ibuprofen_amount == 4 ~ "15+", 
    mh_ibuprofen_yn == "No" ~ "0"
  )) 
#other med use
redcap <- redcap %>%
  mutate(mh_othermeds_yn = case_match(
    mh_othermeds_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#other med amounts
redcap <- redcap %>%
  mutate(mh_othermed_amount = case_when(
    mh_othermed_amount == 1 ~ "1-2", 
    mh_othermed_amount == 2 ~ "3-5", 
    mh_othermed_amount == 3 ~ "6-14", 
    mh_othermed_amount == 4 ~ "15+", 
    mh_othermeds_yn == "No" ~ "0"
  )) 
#beta blocker use
redcap <- redcap %>%
  mutate(mh_betablocker = case_when(
    mh_betablocker == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_betablocker) ~ "No"
  ))
#triptans use
redcap <- redcap %>%
  mutate(mh_triptans = case_when(
    mh_triptans == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_triptans) ~ "No"
  ))
#antidepressants use
redcap <- redcap %>%
  mutate(mh_antidepressants = case_when(
    mh_antidepressants == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_antidepressants) ~ "No"
  ))
#tranq use
redcap <- redcap %>%
  mutate(mh_tranq = case_when(
    mh_tranq == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_tranq) ~ "No"
  ))

#saving file
write_csv(redcap, "Edited data files/redcap_post_table3.csv")  

#Defining vars for table 3
redcap_table3 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Has Medical Insurace` = mh_insurance_yn,
    `Has a Regular Physician` = mh_physician_yn, 
    `Diagnosed with anxiety` = mh_anx, 
    `Diagnosed with depression` = mh_dep, 
    `Diagnosed with invasive cancer` = mh_cancer, 
    `Diagnosed with IBS, Crohn's or Ulcerative Colitis` = mh_ibs, 
    `Diagnosed with kidney stones` = mh_kidney_stone, 
    `Diagnosed with endometriosis (w/o chronic pain)` = mh_endo,
    `Diagnosed with fibroids` = mh_fibroids, 
    `Diagnosed with persistant ovarian cysts` = mh_cysts, 
    `Diagnosed with pelvic inflammatory disease` = mh_pid, 
    `Diagnosed with chronic constipation` = mh_constipation, 
    `Diagnosed with chronic diarrhea` = mh_diarrhea, 
    `Diagnosed with repeated UTIs` = mh_uti, 
    `Diagnosed with repeated vaginal infections` = mh_vag_infection, 
    `Diagnosed with childhood pelvic health problem` = mh_child_pelvic, 
    `Diagnosed with chronic heart condition (needing treatment)` = mh_heart, 
    `Diagnosed with chronic kidney condition (needing treatment)` = mh_kidney, 
    `Diagnosed with chronic lung condition (needing treatment)` = mh_lung, 
    `Diagnosed with chronic liver condition (needing treatment)` = mh_liver, 
    `Fibroid removal surgery` = mh_fibroid_surg, 
    `Ovarian cyst removal surgery` = mh_ovary_surg, 
    `Vaginal surgery` = mh_vaginal_surg, 
    `Other major pelvic surgery` = mh_other_surg, 
    `Used acetaminophen regulary in past year to treat menstrual pain` = mh_acetaminophen_yn, 
    `Number of acetaminophen tablets taken on average per period` = mh_acetaminophen_amount, 
    `Used ibuprofen regulary in past year to treat menstrual pain` = mh_ibuprofen_yn, 
    `Number of ibuprofen tablets taken on average per period` = mh_ibuprofen_amount, 
    `Used other anti-inflammatory regulary in past year to treat menstrual pain` = mh_othermeds_yn, 
    `Number of other anti-inflammatory tablets taken on average per period` = mh_othermed_amount, 
    `Used beta-blockers regularly in past year (for any indication)` = mh_betablocker, 
    `Used triptans regularly in past year (for any indication)` = mh_triptans,
    `Used antidepressants regularly in past year (for any indication)` = mh_antidepressants, 
    `Used minor tranquilizers regularly in past year (for any indication)` = mh_tranq
  )

vars = c("Has Medical Insurace", "Has a Regular Physician", "Diagnosed with anxiety", 
         "Diagnosed with depression", "Diagnosed with invasive cancer", 
         "Diagnosed with IBS, Crohn's or Ulcerative Colitis", "Diagnosed with kidney stones", 
         "Diagnosed with endometriosis (w/o chronic pain)", "Diagnosed with fibroids", 
         "Diagnosed with persistant ovarian cysts", "Diagnosed with pelvic inflammatory disease", 
         "Diagnosed with chronic constipation", "Diagnosed with chronic diarrhea", 
         "Diagnosed with repeated UTIs", "Diagnosed with repeated vaginal infections", 
         "Diagnosed with childhood pelvic health problem", 
         "Diagnosed with chronic heart condition (needing treatment)", 
         "Diagnosed with chronic kidney condition (needing treatment)",
         "Diagnosed with chronic lung condition (needing treatment)", 
         "Diagnosed with chronic liver condition (needing treatment)", 
         "Fibroid removal surgery", "Ovarian cyst removal surgery", "Vaginal surgery", 
         "Other major pelvic surgery", 
         "Used acetaminophen regulary in past year to treat menstrual pain", 
         "Number of acetaminophen tablets taken on average per period", 
         "Used ibuprofen regulary in past year to treat menstrual pain", 
         "Number of ibuprofen tablets taken on average per period", 
         "Used other anti-inflammatory regulary in past year to treat menstrual pain", 
         "Number of other anti-inflammatory tablets taken on average per period", 
         "Used beta-blockers regularly in past year (for any indication)", 
         "Used triptans regularly in past year (for any indication)", 
         "Used antidepressants regularly in past year (for any indication)", 
         "Used minor tranquilizers regularly in past year (for any indication)"
         )

#Creating summary table 3
sum <- CreateTableOne(vars, data = redcap_table3, factorVars = vars, 
                      strata = "Group")

sum_df <- as.data.frame(print(sum,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = TRUE, 
                              pValues = FALSE))

#Creating table with comparisons for DYS and DYSB
redcap_table3_p <- redcap_table3 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table3_p, factorVars = vars, 
                       strata = "Group")

comp_df <- as.data.frame(print(comp,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE, 
                               pValues = TRUE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table3 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#reformat and save table
#Step 1: save rownames as a column
table3 <- data.frame(rowname = rownames(table3), table3, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(table3))) {
  row_label <- table3$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- table3[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- table3[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- table3[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(table3) %>%
  bold(i = which(table3$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(table3), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table3.docx")

######################################
##Table 4: Pain and Sensory Profiles##
######################################

#recode variables
#bps/ic criteria
redcap <- redcap %>%
  mutate(bps_ic_bl = case_match(
    bps_ic_bl,
    1 ~ "Yes", 
    0 ~ "No"
  )) %>%
  group_by(record_id) %>%
  mutate(
    bps_ic_bl = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                       bps_ic_bl[redcap_event_name == "consent_ids_arm_1"][1],
                       NA
    )
  ) %>%
  ungroup()
#ibs criteria
redcap <- redcap %>%
  mutate(ibs_bl = case_match(
    ibs_bl,
    1 ~ "Yes", 
    0 ~ "No"
  )) %>%
  group_by(record_id) %>%
  mutate(
    ibs_bl = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                    ibs_bl[redcap_event_name == "consent_ids_arm_1"][1],
                       NA
    )
  ) %>%
  ungroup()
#copc bodymap summed score
redcap <- redcap %>%
  mutate(copc_sum = copcy_bodymap_ans1___1 + copcy_bodymap_ans1___2 +
         copcy_bodymap_ans1___3 + copcy_bodymap_ans1___4 + copcy_bodymap_ans1___5 
         + copcy_bodymap_ans1___6 + copcy_bodymap_ans1___7 + copcy_bodymap_ans1___8
         + copcy_bodymap_ans1___9 + copcy_bodymap_ans1___10 + copcy_bodymap_ans1___11
         + copcy_bodymap_ans1___12 + copc_bodymap_ans2___13 + copc_bodymap_ans2___14
         + copc_bodymap_ans2___15 + copc_bodymap_ans2___16 + copc_bodymap_ans2___17
         + copc_bodymap_ans2___18 + copc_bodymap_ans2___19 + copc_bodymap_ans2___20
         + copc_bodymap_ans2___21 + copc_bodymap_ans2___22 + copc_bodymap_ans2___23)
#vulvodynia criteria
redcap <- redcap %>%
  mutate(`Meets criteria for Vulvodynia` = copc_vulvo_1 + copc_vulvo_2) %>%
  mutate(`Meets criteria for Vulvodynia` = case_match(
    `Meets criteria for Vulvodynia`, 
    2 ~ "Yes", 
    1 ~ "No", 
    0 ~ "No"
  ))
#endometriosis critiera
redcap <- redcap %>%
  mutate(`Meets criteria for Diagnosed Endometriosis` = copc_endo_1 + 
           copc_endo_2 + copc_endo_3) %>%
  mutate(`Meets criteria for Diagnosed Endometriosis` = case_when(
    `Meets criteria for Diagnosed Endometriosis`== 3 ~ "Yes", 
    `Meets criteria for Diagnosed Endometriosis` == 2 ~ "No", 
    `Meets criteria for Diagnosed Endometriosis` == 1 ~ "No", 
    `Meets criteria for Diagnosed Endometriosis` == 0 ~ "No", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(`Meets criteria for Diagnosed Endometriosis`) ~ "No"
  ))
#persistent fatigue
redcap <- redcap %>%
  mutate(mh_fatigue = case_when(
    mh_fatigue == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_fatigue) ~ "No"
  ))
#sensitivity to sounds
redcap <- redcap %>%
  mutate(mh_gss1 = case_when(
    mh_gss1 == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_gss1) ~ "No"
  ))
#sensitivity to odors
redcap <- redcap %>%
  mutate(mh_gss2 = case_when(
    mh_gss2 == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_gss2) ~ "No"
  ))
#sensitivity to bright lights
redcap <- redcap %>%
  mutate(mh_gss3 = case_when(
    mh_gss3 == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_gss3) ~ "No"
  ))
#sensitivity to chemicals
redcap <- redcap %>%
  mutate(mh_gss4 = case_when(
    mh_gss4 == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_gss4) ~ "No"
  ))
#bleeding amount heaviest 
redcap <- redcap %>%
  mutate(werf_a2_10 = case_match(
    werf_a2_10,
    1 ~ "Spotting",
    2 ~ "Light", 
    3 ~ "Moderate",
    4 ~ "Heavy"
  ))
#bleeding amount average
redcap <- redcap %>%
  mutate(werf_a2_11 = case_match(
    werf_a2_11,
    1 ~ "Spotting",
    2 ~ "Light", 
    3 ~ "Moderate",
    4 ~ "Heavy"
  ))
#intercourse
redcap <- redcap %>%
  mutate(werf_c15sexyesno = case_match(
    werf_c15sexyesno, 
    1 ~ "No",
    2 ~ "Yes"
  ))
#pain with intercourse
redcap <- redcap %>%
  mutate(werf_c15 = case_match(
    werf_c15, 
    1 ~ "Yes", 
    0 ~ "No"
  ))
#pain with intercourse during last intercourse
redcap <- redcap %>%
  mutate(werf_c17 = case_when(
    werf_c15sexyesno == "No" ~ "No", 
    werf_c17 == 1 ~ "No", 
    werf_c17 == 2 ~ "Yes, during intercourse/penetration",
    werf_c17 == 3 ~ "Yes, in the 24 hours following intercourse/penetration",
    werf_c17 == 4 ~ "Yes, both during and in the 24 hours following intercourse/penetration"
  ))
#pain with intercourse severity 
redcap <- redcap %>%
  mutate(werf_c19 = case_when(
    werf_c19 == 0 ~ 0,
    werf_c19 == 1 ~ 1, 
    werf_c19 == 2 ~ 2, 
    werf_c19 == 3 ~ 3,
    werf_c19 == 4 ~ 4, 
    werf_c19 == 5 ~ 5, 
    werf_c19 == 6 ~ 6, 
    werf_c19 == 7 ~ 7, 
    werf_c19 == 8 ~ 8, 
    werf_c19 == 9 ~ 9, 
    werf_c19 == 10 ~ 10, 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(werf_c19) ~ 0
  ))
#pain with intercourse frequency
redcap <- redcap %>%
  mutate(werf_c21 = case_when(
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(werf_c21) ~ "Never", 
    record_id %in% c(7, 232, 544, 1069, 1541, 1893) ~ "Never", 
    werf_c21 == 1 ~ "Occasionally (less than a quarter of times)", 
    werf_c21 == 2 ~ "Often (a quarter to half of the times)", 
    werf_c21 == 3 ~ "Usually (more than half of the times)", 
    werf_c21 == 4 ~ "Always (every time)" 
  ))
#pain with intercourse causing intercourse to stop
redcap <- redcap %>%
  mutate(werf_c23 = case_when(
    werf_c23 == 1 ~ "Yes", 
    werf_c23 == 0 ~ "No", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(werf_c23) ~ "No"
  ))
#diary bleeding amount heaviest
redcap_subset <- redcap %>%
  group_by(record_id) %>%
  summarize(
    dd_bleeding_max = max(pmax(dd_bleeding, dd_bleeding_days, na.rm = TRUE), na.rm = TRUE),
    .groups = "drop"
  )

redcap <- redcap %>%
  left_join(
    redcap_subset,
    by = "record_id"
  ) %>%
  mutate(
    dd_bleeding_max = ifelse(redcap_event_name == "virtual_assessment_arm_1", dd_bleeding_max, NA_real_)
  )

redcap <- redcap %>%
  mutate(dd_bleeding_max = case_match(
    dd_bleeding_max,
    0 ~ "None",
    1 ~ "Spotting",
    2 ~ "Light", 
    3 ~ "Moderate",
    4 ~ "Heavy"
  ))  

redcap <- redcap %>%
  mutate(bleeding_max_consistency = dd_bleeding_max == werf_a2_10)

#uncomment to view counts (72% consistnecy from retroactive report and diaries)
#redcap %>%
#  filter(!is.na(bleeding_max_consistency)) %>%
#  count(bleeding_max_consistency)

#diary pain average
redcap_subset_2 <- redcap %>%
  group_by(record_id) %>%
  filter(str_starts(redcap_event_name, "diary")) %>%
  summarize(
    dd_complete_count = n()
  ) %>%
  ungroup()

redcap <- redcap %>%
  left_join(
    redcap_subset_2,
    by = "record_id"
  ) %>%
  mutate(
    dd_complete_count = ifelse(redcap_event_name == "virtual_assessment_arm_1", dd_complete_count, NA_real_)
  )

redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(dd_pain_sum = sum(dd_menstrual, dd_menstrual_days, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dd_pain_sum = if_else(redcap_event_name == "virtual_assessment_arm_1", 
                               dd_pain_sum, NA_real_))

redcap <- redcap %>%
  mutate(dd_avg_menstrual_pain = dd_pain_sum / dd_complete_count)

#diary bleeding amount average, rounded to nearest whole number
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(dd_bleeding_sum = sum(dd_bleeding, dd_bleeding_days, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dd_bleeding_sum = if_else(redcap_event_name == "virtual_assessment_arm_1", 
                                   dd_bleeding_sum, NA_real_))

redcap <- redcap %>%
  mutate(dd_avg_bleeding = round(dd_bleeding_sum / dd_complete_count)) %>%
  mutate(dd_avg_bleeding = case_match(
    dd_avg_bleeding,
    0 ~ "None",
    1 ~ "Spotting",
    2 ~ "Light", 
    3 ~ "Moderate",
    4 ~ "Heavy"
  ))

redcap <- redcap %>%
  mutate(bleeding_avg_consistency = dd_avg_bleeding == werf_a2_11)

#uncomment to view counts (59% consistnecy from retroactive report and diaries)
#redcap %>%
#  filter(!is.na(bleeding_avg_consistency)) %>%
#  count(bleeding_avg_consistency)

#number diary days with pain >3
redcap_subset_3 <- redcap %>%
  group_by(record_id) %>%
  filter(str_starts(redcap_event_name, "diary")) %>%
  mutate(dd_pain_3_day1 = ifelse(dd_menstrual > 3, 1, 0)) %>%
  mutate(dd_pain_3_day2 = ifelse(redcap_event_name == "diary__day_2_arm_1" & 
                                   dd_menstrual_days > 3, 1, 0)) %>%
  mutate(dd_pain_3_day3 = ifelse(redcap_event_name == "diary__day_3_arm_1" & 
                                   dd_menstrual_days > 3, 1, 0)) %>%
  mutate(dd_pain_3_day4 = ifelse(redcap_event_name == "diary__day_4_arm_1" & 
                                   dd_menstrual_days > 3, 1, 0)) %>%
  mutate(dd_pain_3_day5 = ifelse(redcap_event_name == "diary__day_5_arm_1" & 
                                   dd_menstrual_days > 3, 1, 0)) %>%
  mutate(dd_pain_3 = sum(dd_pain_3_day1, dd_pain_3_day2, dd_pain_3_day3, 
                         dd_pain_3_day4, dd_pain_3_day5, na.rm = TRUE)) %>%
  slice_head() %>%
  ungroup() %>%
  select(1, 333) 
  
redcap <- redcap %>%
  left_join(
    redcap_subset_3,
    by = "record_id"
  ) %>%
  mutate(
    dd_pain_3 = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                               dd_pain_3, NA_real_)
  )

#number diary days with moderate or heavy bleeding
redcap_subset_4 <- redcap %>%
  group_by(record_id) %>%
  filter(str_starts(redcap_event_name, "diary")) %>%
  mutate(dd_bleeding_m_h_day1 = ifelse(dd_bleeding > 2, 1, 0)) %>%
  mutate(dd_bleeding_m_h_day2 = ifelse(redcap_event_name == "diary__day_2_arm_1" & 
                                   dd_bleeding_days > 2, 1, 0)) %>%
  mutate(dd_bleeding_m_h_day3 = ifelse(redcap_event_name == "diary__day_3_arm_1" & 
                                         dd_bleeding_days > 2, 1, 0)) %>%
  mutate(dd_bleeding_m_h_day4 = ifelse(redcap_event_name == "diary__day_4_arm_1" & 
                                         dd_bleeding_days > 2, 1, 0)) %>%
  mutate(dd_bleeding_m_h_day5 = ifelse(redcap_event_name == "diary__day_5_arm_1" & 
                                         dd_bleeding_days > 2, 1, 0)) %>%
  mutate(dd_bleeding_m_h = sum(dd_bleeding_m_h_day1, dd_bleeding_m_h_day2, 
                               dd_bleeding_m_h_day3, dd_bleeding_m_h_day4, 
                               dd_bleeding_m_h_day5, na.rm = TRUE)) %>%
  slice_head() %>%
  ungroup() %>%
  select(1, 334) 

redcap <- redcap %>%
  left_join(
    redcap_subset_4,
    by = "record_id"
  ) %>%
  mutate(
    dd_bleeding_m_h = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                             dd_bleeding_m_h, NA_real_)
  )

#number diary days with bleeding
redcap_subset_5 <- redcap %>%
  group_by(record_id) %>%
  filter(str_starts(redcap_event_name, "diary")) %>%
  mutate(dd_bleeding_day1 = ifelse(dd_bleeding > 0, 1, 0)) %>%
  mutate(dd_bleeding_day2 = ifelse(redcap_event_name == "diary__day_2_arm_1" & 
                                         dd_bleeding_days > 0, 1, 0)) %>%
  mutate(dd_bleeding_day3 = ifelse(redcap_event_name == "diary__day_3_arm_1" & 
                                         dd_bleeding_days > 0, 1, 0)) %>%
  mutate(dd_bleeding_day4 = ifelse(redcap_event_name == "diary__day_4_arm_1" & 
                                         dd_bleeding_days > 0, 1, 0)) %>%
  mutate(dd_bleeding_day5 = ifelse(redcap_event_name == "diary__day_5_arm_1" & 
                                         dd_bleeding_days > 0, 1, 0)) %>%
  mutate(dd_bleeding_number = sum(dd_bleeding_day1, dd_bleeding_day2, 
                                  dd_bleeding_day3, dd_bleeding_day4, 
                                  dd_bleeding_day5, na.rm = TRUE)) %>%
  slice_head() %>%
  ungroup() %>%
  select(1, 335) 

redcap <- redcap %>%
  left_join(
    redcap_subset_5,
    by = "record_id"
  ) %>%
  mutate(
    dd_bleeding_number = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                                dd_bleeding_number, NA_real_)
  )

#diary max bowel pain
redcap_subset_6 <- redcap %>%
  group_by(record_id) %>%
  summarize(
    dd_bowel_max = max(pmax(dd_bowel, dd_bowel_days, na.rm = TRUE), na.rm = TRUE),
    .groups = "drop"
  )

redcap <- redcap %>%
  left_join(
    redcap_subset_6,
    by = "record_id"
  ) %>%
  mutate(
    dd_bowel_max = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                          dd_bowel_max, NA_real_)
  )

#diary max bladder pain
redcap_subset_7 <- redcap %>%
  group_by(record_id) %>%
  summarize(
    dd_bladder_max = max(pmax(dd_bladder, dd_bladder_days, na.rm = TRUE), na.rm = TRUE),
    .groups = "drop"
  )

redcap <- redcap %>%
  left_join(
    redcap_subset_7,
    by = "record_id"
  ) %>%
  mutate(
    dd_bladder_max = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                            dd_bladder_max, NA_real_)
  )

#diary days using medication, removes anyone without complete diary data
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(dd_medication = if (any(
                             (redcap_event_name == "diary__day_1_arm_1" & 
                                 is.na(dd_nsaid_yn)) | 
                             (redcap_event_name == "virtual_assessment_arm_1" & 
                              dd_complete_count < 5)
                             ))  {
    NA_real_
  } else {
    sum(dd_nsaid_yn, dd_acetaminophen_yn, dd_painreliever_yn, 
        dd_painkillers_yn_days, na.rm = TRUE)
  }) %>%
  ungroup()

redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(dd_medication_multi = case_when(
    redcap_event_name == "diary__day_1_arm_1" ~ sum(dd_nsaid_yn, 
                                                    dd_acetaminophen_yn, 
                                                    dd_painreliever_yn, na.rm = TRUE)
  )) %>%
  mutate(dd_medication = case_when(
    dd_medication_multi == 2 ~ dd_medication - 1, 
    dd_medication_multi == 3 ~ dd_medication - 2, 
    dd_medication_multi < 2 ~ dd_medication
  )) %>%
  mutate(
    dd_medication = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                           dd_medication[redcap_event_name == "diary__day_1_arm_1"][1],
                         NA
    )
  ) %>%
  ungroup()

#diary max menstrual pain 
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    max_pain_bl = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                         max_pain_bl[redcap_event_name == "consent_ids_arm_1"][1],
                       NA
    )
  ) %>%
  ungroup()

#diary avearge pelvic pain day before period 
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    dd_painbefore = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                           dd_painbefore[redcap_event_name == "diary__day_1_arm_1"][1],
                         NA
    )
  ) %>%
  ungroup()

#saving file
write_csv(redcap, "Edited data files/redcap_post_table4.csv") 

#Defining vars for table 4
redcap_table4 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Average NMPP, last 7 days` = mcgill_1,
    `Average dysuria, last 7 days` = mcgill_2, 
    `Average pain with bowel movements, last 7 days` = mcgill_3, 
    `Sexual intercourse, last 7 days` = mcgill_4, 
    `Average dyspareunia, last 7 days` = mcgill_5,
    `Meets criteria for BPS/IC` = bps_ic_bl, 
    `Meets criteria for IBS` = ibs_bl, 
    `Number bodily pain sites, last 30 days` = copc_sum,
    `Persistent fatigue` = mh_fatigue, 
    `Sensitivity to sounds` = mh_gss1, 
    `Sensitivity to odors` = mh_gss2, 
    `Sensitivity to bright lights` = mh_gss3, 
    `Sensitivity to chemicals` = mh_gss4,
    `Bleeding on heaviest day of period` = werf_a2_10, 
    `Bleeding on average per period` = werf_a2_11, 
    `Ever had sexual intercourse` = werf_c15sexyesno, 
    `Ever had dyspareunia`= werf_c15, 
    `Age when dyspareunia began` = werf_c16, 
    `Dyspareunia during most recent sexual intercourse` = werf_c17, 
    `Max severity of dyspareunia during most recent sexual intercourse` = werf_c19, 
    `Frequency of dyspareunia, last 12 months` = werf_c21, 
    `Dyspareunia resulting in disruption of sexual intercourse` = werf_c23, 
    `Days of complete menstrual diary data` = dd_complete_count, 
    `Days of bleeding reported on diary` = dd_bleeding_number, 
    `Days of moderate to heavy bleeding reported on diary` = dd_bleeding_m_h,
    `Average pelvic pain, 24 hours before period onset` = dd_painbefore,
    `Days with menstrual pain > 3 reported on diary` = dd_pain_3, 
    `Average menstrual pain reported on diary` = dd_avg_menstrual_pain, 
    `Max menstrual pain reported on diary` = max_pain_bl, 
    `Max bowel pain reported on diary` = dd_bowel_max, 
    `Max bladder pain reported on diary` = dd_bladder_max, 
    `Days of pain reliever usage reported on diary` = dd_medication
  )

   
vars = c("Average NMPP, last 7 days", 
         "Average dysuria, last 7 days", 
         "Average pain with bowel movements, last 7 days",
         "Sexual intercourse, last 7 days", 
         "Average dyspareunia, last 7 days", 
         "Meets criteria for BPS/IC", 
         "Meets criteria for IBS", 
         "Number bodily pain sites, last 30 days", 
         "Persistent fatigue", 
         "Sensitivity to sounds", 
         "Sensitivity to odors", 
         "Sensitivity to chemicals", 
         "Ever had sexual intercourse", 
         "Ever had dyspareunia", 
         "Age when dyspareunia began", 
         "Dyspareunia during most recent sexual intercourse", 
         "Max severity of dyspareunia during most recent sexual intercourse", 
         "Frequency of dyspareunia, last 12 months", 
         "Dyspareunia resulting in disruption of sexual intercourse", 
         "Days of complete menstrual diary data", 
         "Days of bleeding reported on diary", 
         "Days of moderate to heavy bleeding reported on diary", 
         "Average pelvic pain, 24 hours before period onset", 
         "Days with menstrual pain > 3 reported on diary", 
         "Average menstrual pain reported on diary", 
         "Max menstrual pain reported on diary", 
         "Max bowel pain reported on diary", 
         "Max bladder pain reported on diary", 
         "Days of pain reliever usage reported on diary", 
         "Meets criteria for Vulvodynia", 
         "Meets criteria for Diagnosed Endometriosis"
)
    
factor_vars <- c("Meets criteria for BPS/IC", 
                 "Meets criteria for IBS",
                 "Meets criteria for Vulvodynia", 
                 "Meets criteria for Diagnosed Endometriosis",
                 "Persistent fatigue", 
                 "Sensitivity to sounds", 
                 "Sensitivity to odors", 
                 "Sensitivity to chemicals",
                 "Sexual intercourse, last 7 days",
                 "Ever had sexual intercourse", 
                 "Ever had dyspareunia", 
                 "Dyspareunia during most recent sexual intercourse",
                 "Frequency of dyspareunia, last 12 months", 
                 "Dyspareunia resulting in disruption of sexual intercourse", 
                 "Bleeding on heaviest day of period", 
                 "Bleeding on average per period"
)

nonnormal_vars <- c("Average NMPP, last 7 days", 
                    "Average dysuria, last 7 days", 
                    "Average pain with bowel movements, last 7 days",
                    "Average dyspareunia, last 7 days",
                    "Number bodily pain sites, last 30 days",
                    "Age when dyspareunia began", 
                    "Max severity of dyspareunia during most recent sexual intercourse", 
                    "Days of complete menstrual diary data", 
                    "Days of bleeding reported on diary", 
                    "Days of moderate to heavy bleeding reported on diary", 
                    "Average pelvic pain, 24 hours before period onset", 
                    "Days with menstrual pain > 3 reported on diary", 
                    "Average menstrual pain reported on diary", 
                    "Max menstrual pain reported on diary", 
                    "Max bowel pain reported on diary", 
                    "Max bladder pain reported on diary", 
                    "Days of pain reliever usage reported on diary"
)
    
#Creating summary table 4
sum <- CreateTableOne(vars, data = redcap_table4, factorVars = factor_vars, 
                      strata = "Group")

sum_df <- as.data.frame(print(sum,
                              nonnormal = nonnormal_vars,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = TRUE, 
                              pValues = FALSE))

#Creating table with comparisons for DYS and DYSB
redcap_table4_p <- redcap_table4 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table4_p, factorVars = factor_vars, 
                       strata = "Group")

comp_df <- as.data.frame(print(comp,
                               nonnormal = nonnormal_vars,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE, 
                               pValues = TRUE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table4 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#reformat and save table
#Step 1: save rownames as a column
table4 <- data.frame(rowname = rownames(table4), table4, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(table4))) {
  row_label <- table4$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- table4[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- table4[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- table4[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(table4) %>%
  bold(i = which(table4$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(table4), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table4.docx")

    
###################################
##Table 5: Psycho-social Profiles##
###################################    
    
#recode variables 
#promis physical function
redcap <- redcap %>%
  mutate(promis_pf_sum = promis_pf_1 + promis_pf_2 + promis_pf_3 + promis_pf_4)
    
#create lookup table of t-scores and SE from promis manual
promis_pf_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(22.5, 26.6, 28.9, 30.5, 31.9, 33.2, 34.4, 35.6, 36.7, 
              37.9, 39.2, 40.5, 41.9, 43.5, 45.5, 48.3, 57.0),
  se = c(4.0, 2.8, 2.5, 2.4, 2.3, 2.3, 2.3, 2.3, 2.3, 
         2.3, 2.4, 2.4, 2.5, 2.6, 2.8, 3.3, 6.6)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_pf_lookup, by = c("promis_pf_sum" = "summed_score")) %>%
  rename(
    promis_pf_t_score = t_score,
    promis_pf_se = se
  ) 

#promis anxiety
redcap <- redcap %>%
  mutate(promis_anx_sum = promis_anx_1 + promis_anx_2 + promis_anx_3 + promis_anx_4)

#create lookup table of t-scores and SE from promis manual
promis_anx_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(40.3, 48.0, 51.2, 53.7, 55.8, 57.7, 59.5, 61.4, 63.4, 
              65.3, 67.3, 69.3, 71.2, 73.3, 75.4, 77.9, 81.6),
  se = c(6.1, 3.6, 3.1, 2.8, 2.7, 2.6, 2.6, 2.6, 2.6, 
         2.7, 2.7, 2.7, 2.7, 2.7, 2.7, 2.9, 3.7)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_anx_lookup, by = c("promis_anx_sum" = "summed_score")) %>%
  rename(
    promis_anx_t_score = t_score,
    promis_anx_se = se
  ) 
    
#promis depression
redcap <- redcap %>%
  mutate(promis_dep_sum = promis_dep_1 + promis_dep_2 + promis_dep_3 + promis_dep_4)

#create lookup table of t-scores and SE from promis manual
promis_dep_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(41.0, 49.0, 51.8, 53.9, 55.7, 57.3, 58.9, 60.5, 62.2, 
              63.9, 65.7, 67.5, 69.4, 71.2, 73.3, 75.7, 79.4),
  se = c(6.2, 3.2, 2.7, 2.4, 2.3, 2.3, 2.3, 2.3, 2.3, 2.3, 
         2.3, 2.3, 2.3, 2.4, 2.4, 2.6, 2.6)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_dep_lookup, by = c("promis_dep_sum" = "summed_score")) %>%
  rename(
    promis_dep_t_score = t_score,
    promis_dep_se = se
  )     
    
#promis fatigue
redcap <- redcap %>%
  mutate(promis_fat_sum = promis_fat_1 + promis_fat_2 + promis_fat_3 + promis_fat_4)

#create lookup table of t-scores and SE from promis manual
promis_fat_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(33.7, 39.7, 43.1, 46.0, 48.6, 51.0, 53.1, 55.1, 
              57.0, 58.8, 60.7, 62.7, 64.6, 66.7, 69.0, 71.6, 75.8),
  se = c(4.9, 3.1, 2.7, 2.6, 2.5, 2.5, 2.4, 2.4, 2.3, 2.3, 
         2.3, 2.4, 2.4, 2.4, 2.5, 2.7, 3.9)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_fat_lookup, by = c("promis_fat_sum" = "summed_score")) %>%
  rename(
    promis_fat_t_score = t_score,
    promis_fat_se = se
  )     

#promis sleep disturbance
#promis_sd_3 and promis_sd_4 need to be reverse scored
redcap <- redcap %>%
  mutate(promis_sd_3 = case_match(
    promis_sd_3, 
    5 ~ 1, 
    4 ~ 2, 
    3 ~ 3, 
    2 ~ 4, 
    1 ~ 5
  ))

redcap <- redcap %>%
  mutate(promis_sd_4 = case_match(
    promis_sd_4, 
    5 ~ 1, 
    4 ~ 2, 
    3 ~ 3, 
    2 ~ 4, 
    1 ~ 5
  ))

redcap <- redcap %>%
  mutate(promis_sd_sum = promis_sd_1 + promis_sd_2 + promis_sd_3 + promis_sd_4)

#create lookup table of t-scores and SE from promis manual
promis_sd_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(32.0, 37.5, 41.1, 43.8, 46.2, 48.4, 50.5, 52.4, 
              54.3, 56.1, 57.9, 59.8, 61.7, 63.8, 66.0, 68.8, 73.3),
  se = c(5.2, 4.0, 3.7, 3.5, 3.5, 3.4, 3.4, 3.4, 3.4, 
         3.4, 3.3, 3.3, 3.3, 3.4, 3.4, 3.7, 4.6)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_sd_lookup, by = c("promis_sd_sum" = "summed_score")) %>%
  rename(
    promis_sd_t_score = t_score,
    promis_sd_se = se
  )     

#promis social roles and activies
redcap <- redcap %>%
  mutate(promis_sr_sum = promis_sr_1 + promis_sr_2 + promis_sr_3 + promis_sr_4)

#create lookup table of t-scores and SE from promis manual
promis_sr_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(27.5, 31.8, 34.0, 35.7, 37.3, 38.8, 40.5, 42.3, 
              44.2, 46.2, 48.1, 50.0, 51.9, 53.7, 55.8, 58.3, 64.2),
  se = c(4.1, 2.5, 2.3, 2.2, 2.1, 2.2, 2.3, 2.3, 2.3, 
         2.3, 2.2, 2.2, 2.2, 2.3, 2.3, 2.7, 5.1)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_sr_lookup, by = c("promis_sr_sum" = "summed_score")) %>%
  rename(
    promis_sr_t_score = t_score,
    promis_sr_se = se
  )   

#promis pain interference
redcap <- redcap %>%
  mutate(promis_pi_sum = promis_pi_1 + promis_pi_2 + promis_pi_3 + promis_pi_4)

#create lookup table of t-scores and SE from promis manual
promis_pi_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(41.6, 49.6, 52.0, 53.9, 55.6, 57.1, 58.5, 59.9, 
              61.2, 62.5, 63.8, 65.2, 66.6, 68.0, 69.7, 71.6, 75.6),
  se = c(6.1, 2.5, 2.0, 1.9, 1.9, 1.9, 1.8, 1.8, 1.8, 
         1.8, 1.8, 1.8, 1.8, 1.8, 1.9, 2.1, 3.7)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_pi_lookup, by = c("promis_pi_sum" = "summed_score")) %>%
  rename(
    promis_pi_t_score = t_score,
    promis_pi_se = se
  )  

#promis cognitive function
redcap <- redcap %>%
  mutate(promis_cf_sum = promis_cf_1 + promis_cf_2)

#create lookup table of t-scores and SE from promis manual
promis_cf_lookup <- tibble(
  summed_score = 2:10,
  t_score = c(29.5, 34.4, 38.0, 41.2, 44.3, 47.3, 50.5, 54.7, 61.2),
  se = c(6.4, 5.9, 5.7, 5.7, 5.8, 5.8, 5.7, 5.9, 6.9)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_cf_lookup, by = c("promis_cf_sum" = "summed_score")) %>%
  rename(
    promis_cf_t_score = t_score,
    promis_cf_se = se
  )  

#PROPr score for promis 29+2

#calculate theta scores for all t scores

redcap <- redcap %>%
  mutate(theta_phys = (promis_pf_t_score - 50)/10) %>%
  mutate(theta_dep = (promis_dep_t_score - 50)/10) %>%
  mutate(theta_fat = (promis_fat_t_score - 50)/10) %>%
  mutate(theta_slp = (promis_sd_t_score - 50)/10) %>%
  mutate(theta_pain = (promis_pi_t_score - 50)/10) %>%
  mutate(theta_cog = (promis_cf_t_score - 50)/10) %>%
  mutate(theta_sr = (promis_sr_t_score - 50)/10)

#use PROPr function from github to calculate scores
source("R files/propr.maut.function.201709.R")

redcap_propr <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rowwise() %>%
  mutate(
    results = list(
      propr.maut.function.201709(
        c(theta_cog, theta_dep, theta_fat,
          theta_pain, theta_phys, theta_slp, theta_sr)
      )
    ),
    PROPr              = results$PROPr,
    cognition_utility  = results$cognition,
    depression_utility = results$depression,
    fatigue_utility    = results$fatigue,
    pain_utility       = results$pain,
    physical_utility   = results$physical,
    sleep_utility      = results$sleep,
    social_utility     = results$social
  ) %>%
  select(-results) %>%   # drop the temporary list column
  ungroup() %>%
  select(1, 366:373)

#merge PROPr and individual utility scores back into dataset
redcap <- redcap %>%
  left_join(
    redcap_propr,
    by = "record_id"
  ) %>%
  mutate(
    PROPr = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                                PROPr, NA_real_)
  ) %>%
  mutate(
    cognition_utility = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                               cognition_utility, NA_real_)
  ) %>%
  mutate(
    depression_utility = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                                depression_utility, NA_real_)
  ) %>%
  mutate(
    fatigue_utility = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                             fatigue_utility, NA_real_)
  ) %>%
  mutate(
    pain_utility = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                          pain_utility, NA_real_)
  ) %>%
  mutate(
    physical_utility = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                              physical_utility, NA_real_)
  ) %>%
  mutate(
    sleep_utility = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                           sleep_utility, NA_real_)
  ) %>%
  mutate(
    social_utility = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                            social_utility, NA_real_)
  )

#promis positive affect
redcap <- redcap %>%
  mutate(promis_pos_sum = promis_swb_p_027r1 + promis_swb_p_025r1 + 
           promis_swb_p_026r1 + promis_swb_p_029r1)

#create lookup table of t-scores and SE from promis manual
promis_pos_lookup <- tibble(
  summed_score = 4:20,
  t_score = c(22.0, 25.7, 28.0, 30.0, 31.8, 33.6, 35.4, 37.4, 39.5, 41.6, 
              43.8, 46.2, 48.7, 51.2, 53.8, 56.8, 63.0),
  se = c(3.6, 2.7, 2.5, 2.5, 2.4, 2.5, 2.5, 2.5, 2.5, 2.5, 2.6, 2.6, 
         2.7, 2.6, 2.6, 3.0, 5.3)
)

#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_pos_lookup, by = c("promis_pos_sum" = "summed_score")) %>%
  rename(
    promis_pos_t_score = t_score,
    promis_pos_se = se
  ) 

#pain catastrophizing total and sub-scales
#total
redcap <- redcap %>%
  mutate(`PCS-T` = pcs1a + pcs1b + pcs1c + pcs1d + pcs1e + pcs1f +
         pcs1g + pcs1h + pcs1i + pcs1j + pcs1k + pcs1l + pcs1m)
#rumination sub scale
redcap <- redcap %>%
  mutate(`PCS-R` = pcs1h + pcs1i + pcs1j + pcs1k)
#magnification sub scale
redcap <- redcap %>%
  mutate(`PCS-M` = pcs1f + pcs1g + pcs1m)
#helplessness sub scale
redcap <- redcap %>%
  mutate(`PCS-H` = pcs1a + pcs1b + pcs1c + pcs1d + pcs1e + pcs1l)

#gss
redcap <- redcap %>%
  mutate(gss_sum = gss_balance + gss_mouth + gss_heart + 
           gss_chemicals + gss_sound + gss_light)

#aces
redcap <- redcap %>%
  mutate(aces_sum = swear + push + touch + loved + eat + divorced +
           mother + drugs + depressed + prison)

#perceived stress scale
redcap <- redcap %>%
  mutate(pss_sum = pss10_1 + pss10_2 + pss10_3 + pss10_4 + pss10_5 +
           pss10_6 + pss10_7 + pss10_8 + pss10_9 + pss10_10)

#GSRS summed score 
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    gsrs_bl = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                     gsrs_bl[redcap_event_name == "consent_ids_arm_1"][1],
                         NA
    )
  ) %>%
  ungroup()

#icsi summed score 
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    icsi_bl = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                     icsi_bl[redcap_event_name == "consent_ids_arm_1"][1],
                     NA
    )
  ) %>%
  ungroup()

#gupi summed score 
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    gupi_bl = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                     gupi_bl[redcap_event_name == "consent_ids_arm_1"][1],
                     NA
    )
  ) %>%
  ungroup()

#promis belly pain
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    belly_pain_bl = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                           belly_pain_bl[redcap_event_name == "consent_ids_arm_1"][1],
                     NA
    )
  ) %>%
  ungroup()
#create lookup table of t-scores and SE from promis manual
promis_belly_lookup <- tibble(
  summed_score = 2:25,
  t_score = c(33.9, 42.2, 48.3, 54.0, 0.00, 43.3, 47.0, 49.9, 52.3, 54.4, 56.5, 
              58.4, 60.2, 61.9, 63.5, 65.0, 66.6, 68.1, 69.6, 71.1, 72.8, 74.7, 
              76.8, 80.0),
  se = c(6.2, 5.9, 5.6, 5.5, 0.0, 3.8, 3.3, 3.1, 3.0, 3.0, 3.0, 2.9, 2.8, 2.8, 
         2.8, 2.8, 2.8, 2.7, 2.7, 2.8, 2.8, 3.0, 3.1, 3.7)
)
#create new variables with t scores and SE 
redcap <- redcap %>%
  left_join(promis_belly_lookup, by = c("belly_pain_bl" = "summed_score")) %>%
  rename(
    promis_belly_t_score = t_score,
    promis_belly_se = se
  ) 
#calculate t score and se for sum of 6 (dependent response to question 1)
#load in dataset that contains responses for [promis_gisx78], and merge
redcap_sup_vars <- read_csv("Raw files/EH22236CRAMPP2-VBTPredictiveFactors_DATA_2025-09-18_1412.csv", 
                            col_types = cols(redcap_event_name = col_skip()))

redcap <- redcap %>%
  left_join(redcap_sup_vars, by = "record_id") %>%
  mutate(promis_gisx78 = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                                promis_gisx78, NA))
#t score and se
redcap <- redcap %>%
  mutate(promis_belly_t_score = case_when(
    promis_belly_t_score == 0.00 & promis_gisx78 == 1 ~ 58.6, 
    promis_belly_t_score == 0.00 & promis_gisx78 > 1 ~ 39.3, 
    TRUE ~ promis_belly_t_score
  ))

redcap <- redcap %>%
  mutate(promis_belly_se = case_when(
    promis_belly_se == 0.0 & promis_gisx78 == 1 ~ 5.3, 
    promis_belly_se == 0.0 & promis_gisx78 > 1 ~ 4.4, 
    TRUE ~ promis_belly_se
  ))


#saving file
write_csv(redcap, "Edited data files/redcap_post_table5.csv") 






































#prelim table 5
redcap_table5 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1")

vars <- c("promis_pf_t_score", "promis_anx_t_score", "promis_dep_t_score", 
          "promis_fat_t_score", "promis_sd_t_score", "promis_sr_t_score", 
          "promis_pi_t_score", "promis_cf_t_score", "promis_average_pain", 
          "PROPr", "cognition_utility", "depression_utility", "fatigue_utility",
          "pain_utility", "physical_utility", "sleep_utility", "social_utility")

#Creating summary table 5
sum <- CreateTableOne(vars, data = redcap_table5, strata = "Group")

sum_df <- as.data.frame(print(sum,
                              nonnormal = vars,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = TRUE, 
                              pValues = FALSE))

#Creating table with comparisons for DYS and DYSB
redcap_table5_p <- redcap_table5 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table5_p, strata = "Group")

comp_df <- as.data.frame(print(comp,
                               nonnormal = vars,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE, 
                               pValues = TRUE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table5 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#reformat and save table
#Step 1: save rownames as a column
table5 <- data.frame(rowname = rownames(table5), table5, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(table5))) {
  row_label <- table5$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- table5[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- table5[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- table5[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]


#building flextable
ft <- flextable(table5) %>%
  bold(i = which(table5$Variable != ""), j = 1) %>%      # Bold variable rows
  align(align = "left", part = "all") %>%                 # Align left
  fontsize(size = 9, part = "all") %>%                    # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%   # Fixed width layout
  width(j = 1, width = 2.25) %>%                          # Widen first column for variable names
  width(j = 2:ncol(table5), width = 1.25) %>%            # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table5.docx")









