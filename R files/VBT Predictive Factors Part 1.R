#VBT Predictive Factors ~ part 1
#Written by Sarah Darnell, last modified 7.21.25

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
  )) %>%
  rename(`Assigned Sex at Birth` = mh_assigned_sex)
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
  )) %>%
  rename(Ethnicity = mh4_ethnicity)
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
  )) %>%
  rename(Education = mh5_education)
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
  )) %>%
  rename(`Nicotine Usage` = mh_smoking)
#THC
redcap <- redcap %>%
  mutate(ms_thc = case_match(
    ms_thc, 
    1 ~ "Yes", 
    0 ~ "No"
  )) %>%
  rename(`THC (Marijuana) Usage` = ms_thc)
#Problem with drugs or alcohol
redcap <- redcap %>%
  mutate(mh9b1 = case_match(
    mh9b1, 
    1 ~ "Yes", 
    0 ~ "No"
  )) %>%
  rename(`Have you ever had a problem with drugs or alcohol?` = mh9b1)
#births, also forces text input into numerics (visually inspected, a few pts
#wrote in 0, so coercing text to 0)
redcap <- redcap %>%
  rename(`Vaginal Births` = mh15_vagbirths, 
         `Caesarean Section Births` = mh14_csection) %>%
  mutate(across(
    c(`Vaginal Births`, `Caesarean Section Births`),
    ~ ifelse(
      redcap_event_name == "virtual_assessment_arm_1",
      ifelse(is.na(as.numeric(.x)), 0, as.numeric(.x)),
      .x
    )
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
  )) %>%
  rename(Income = mh_income)  
  
#saving file
write_csv(redcap, "Edited data files/redcap.csv")  

#Defining vars for table 1
vars <- c("Age", "Assigned Sex at Birth", "Gender", "Race", "Ethnicity", 
          "Education", "Employment", "Income", "Vaginal Births", 
          "Caesarean Section Births", "Nicotine Usage", "THC (Marijuana) Usage", 
          "Have you ever had a problem with drugs or alcohol?")
factor_vars <- c("Assigned Sex at Birth", "Gender", "Race", "Ethnicity", 
                 "Education", "Employment", "Income", "Nicotine Usage", 
                 "THC (Marijuana) Usage", 
                 "Have you ever had a problem with drugs or alcohol?")
redcap_table1 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1")

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

#building flextable
ft <- flextable(demo_df) %>%
  bold(i = which(demo_df$Variable != ""), j = 1) %>%            # Bold variable rows
  align(align = "left", part = "all") %>%                        # Align left
  fontsize(size = 9, part = "all") %>%                           # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%          # Fixed width layout
  width(j = 1, width = 2.25) %>%                                 # Widen first column for variable names
  width(j = 2:ncol(demo_df), width = 1.25) %>%                   # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table1.docx")


