#VBT Predictive Factors ~ part 1
#Written by Sarah Darnell

library(readr)
library(dplyr)
library(tableone)
library(flextable)
library(officer)
library(stringr)
library(readxl)
library(tibble)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/VBT Predictive Factors CRAMPP2")

#import redcap pull of variables
redcap <- read_csv("Raw files/EH22236CRAMPP2-VBTPredictiveFactors_DATA_2026-04-21_1413.csv", 
                   col_types = cols(redcap_survey_identifier = col_skip(), 
                                    participant_status_timestamp = col_skip()))

#import age
age <- read_csv("Raw files/age.csv")

#merge age into redcap dataset, for virtual_assessment_arm_1 event only
redcap <- redcap %>%
  left_join(age, by = "record_id") %>%
  mutate(Age = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                      Age, NA))

###############################################
##Initial Cleaning - Groups and bladder times##
###############################################

#remove invalid records (see analysis notes)
redcap <- redcap %>%
  filter(record_id != 1154)

#time between finishing drinking and FS
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = as.numeric(vbt_fs_time - vbt_time_drinking)/60,
          time_drinking_fs_mins = ifelse(time_drinking_fs_mins < 0, 
                                        time_drinking_fs_mins + 1440, 
                                        time_drinking_fs_mins))

#time bwtn drinking and FU
redcap <- redcap %>%
  mutate(time_drinking_fu_mins = as.numeric(vbt_fu_time - vbt_time_drinking)/60,
         time_drinking_fu_mins = ifelse(time_drinking_fu_mins < 0, 
                                        time_drinking_fu_mins + 1440, 
                                        time_drinking_fu_mins))

#time between drinking and mt
redcap <- redcap %>%
  mutate(time_drinking_mt_mins = as.numeric(vbt_mt_time - vbt_time_drinking)/60,
         time_drinking_mt_mins = ifelse(time_drinking_mt_mins < 0, 
                                        time_drinking_mt_mins + 1440, 
                                        time_drinking_mt_mins))

#time bwtn drinking and cap out
redcap <- redcap %>%
  mutate(time_drinking_cap_mins = as.numeric(vbt_cap_out_time - vbt_time_drinking)/60,
         time_drinking_cap_mins = ifelse(time_drinking_cap_mins < 0, 
                                         time_drinking_cap_mins + 1440, 
                                         time_drinking_cap_mins))

#flag for negative times, very fast, or total times that are over 150 mins
redcap_flag <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rowwise() %>%
  mutate(
    vbt_flag = any(c(time_drinking_fs_mins, time_drinking_fu_mins, time_drinking_mt_mins, time_drinking_cap_mins) < 0, na.rm = TRUE) |
      any(c(time_drinking_fs_mins, time_drinking_fu_mins, time_drinking_mt_mins, time_drinking_cap_mins) > 150, na.rm = TRUE) |
      all(is.na(c(time_drinking_fs_mins, time_drinking_fu_mins, time_drinking_mt_mins, time_drinking_cap_mins))) |
      any(c(time_drinking_cap_mins, time_drinking_mt_mins) < 5, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  select(1, 313)

redcap <- redcap %>%
  left_join(
    redcap_flag,
    by = "record_id"
  ) %>%
  mutate(
    vbt_flag = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                      vbt_flag, NA_real_)
  )

#sub-setting key group and key bladder vars to double check flags
redcap_key <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  select(record_id, group_arm2, vbt_time_drinking, time_drinking_fs_mins, vbt_fs_time,
         vbt_fs_pain, time_drinking_fu_mins, vbt_fu_time, vbt_fu_pain,
         time_drinking_mt_mins, vbt_mt_time, vbt_mt_pain, time_drinking_cap_mins, 
         time_drinking_cap_mins, vbt_cap_out_time, vbt_flag)

#flag results, 4/24/26 - 18 flagged, details below for keep vs throw out
#146: throw out - all times blank
#307: throw out - finished in 1 minute
#759: keep - drinking, fs, and fu times entered as 0900 when meant 2100
redcap <- redcap %>%
  mutate(time_drinking_mt_mins = ifelse(record_id == "759", 73, time_drinking_mt_mins))
#782: throw out - nonsensical times
#811: keep - drinking time meant to be 11pm instead of 12am
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = ifelse(record_id == "811", 24, time_drinking_fs_mins),
         time_drinking_fu_mins = ifelse(record_id == "811", 54, time_drinking_fu_mins), 
         time_drinking_mt_mins = ifelse(record_id == "811", 97, time_drinking_mt_mins))
#830: throw out - finished in 2 minutes
#865: keep - drinking time meant to be 18:01 instead of 16:01 
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = ifelse(record_id == "865", 17, time_drinking_fs_mins),
         time_drinking_fu_mins = ifelse(record_id == "865", 40, time_drinking_fu_mins), 
         time_drinking_mt_mins = ifelse(record_id == "865", 56, time_drinking_mt_mins))
#1723: throw out - didn't finish task, times are off
#1789: throw out - finished in 1 minute
#1806: keep - drinking time meant to be 13:15 instead of 01:15
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = ifelse(record_id == "1806", 25, time_drinking_fs_mins),
         time_drinking_fu_mins = ifelse(record_id == "1806", 45, time_drinking_fu_mins), 
         time_drinking_mt_mins = ifelse(record_id == "1806", 54, time_drinking_mt_mins))
#1939: keep - drinking time meant to be 20:19 instead of 08:19
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = ifelse(record_id == "1939", 18, time_drinking_fs_mins),
         time_drinking_fu_mins = ifelse(record_id == "1939", 28, time_drinking_fu_mins), 
         time_drinking_mt_mins = ifelse(record_id == "1939", 36, time_drinking_mt_mins))
#1959: keep - drinking time meant to be 14:50 instead of 02:50
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = ifelse(record_id == "1959", 20, time_drinking_fs_mins),
         time_drinking_fu_mins = ifelse(record_id == "1959", 54, time_drinking_fu_mins), 
         time_drinking_mt_mins = ifelse(record_id == "1959", 62, time_drinking_mt_mins))
#2128: keep - drinking time meant to be 20:25 instead of 08:25
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = ifelse(record_id == "2128", 30, time_drinking_fs_mins),
         time_drinking_fu_mins = ifelse(record_id == "2128", 44, time_drinking_fu_mins), 
         time_drinking_mt_mins = ifelse(record_id == "2128", 84, time_drinking_mt_mins),
         time_drinking_cap_mins = ifelse(record_id == "2128", NA, time_drinking_cap_mins))
#2475: throw out - nonsensical times and ratings
#2509: both - FS and FU done correctly (keep), MT and cap out done past time (throw)
redcap <- redcap %>%
  mutate(time_drinking_mt_mins = ifelse(record_id == "2509", NA, time_drinking_mt_mins),
         vbt_mt_pain = ifelse(record_id == "2509", NA, vbt_mt_pain),
         vbt_mt_urgency = ifelse(record_id == "2509", NA, vbt_mt_urgency),
         vbt_sharp = ifelse(record_id == "2509", NA, vbt_sharp),
         vbt_pressing = ifelse(record_id == "2509", NA, vbt_pressing),
         time_drinking_cap_mins = ifelse(record_id == "2509", NA, time_drinking_cap_mins))
#2276: throw out - finished task in 4 minutes
#2926: keep - drinking time meant to be 21:58 instead of 09:58
redcap <- redcap %>%
  mutate(time_drinking_fs_mins = ifelse(record_id == "2926", 61, time_drinking_fs_mins),
         time_drinking_fu_mins = ifelse(record_id == "2926", 61, time_drinking_fu_mins), 
         time_drinking_mt_mins = ifelse(record_id == "2926", 110, time_drinking_mt_mins))
#2962: throw - finished task in 1 minute

#re-run flag for negative times, very fast, or total times that are over 150 mins
redcap <- redcap %>%
  select(-313)

#flag for negative times, very fast, or total times that are over 150 mins
redcap_flag <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rowwise() %>%
  mutate(
    vbt_flag = any(c(time_drinking_fs_mins, time_drinking_fu_mins, time_drinking_mt_mins, time_drinking_cap_mins) < 0, na.rm = TRUE) |
      any(c(time_drinking_fs_mins, time_drinking_fu_mins, time_drinking_mt_mins, time_drinking_cap_mins) > 150, na.rm = TRUE) |
      all(is.na(c(time_drinking_fs_mins, time_drinking_fu_mins, time_drinking_mt_mins, time_drinking_cap_mins))) |
      any(c(time_drinking_cap_mins, time_drinking_mt_mins) < 5, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  select(1, 313)

redcap <- redcap %>%
  left_join(
    redcap_flag,
    by = "record_id"
  ) %>%
  mutate(
    vbt_flag = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                      vbt_flag, NA_real_)
  )

#dummy var for cap out - if MT has time, remove cap out time
redcap <- redcap %>%
  mutate(capped_out = case_when(
    redcap_event_name == "virtual_assessment_arm_1" & is.na(vbt_cap_out_time) ~ 0,
    redcap_event_name == "virtual_assessment_arm_1" & !is.na(vbt_cap_out_time) &
      time_drinking_mt_mins < 150 ~ 0, 
    redcap_event_name == "virtual_assessment_arm_1" & !is.na(vbt_cap_out_time) &
      time_drinking_mt_mins > 150 | is.na(time_drinking_mt_mins) ~ 1, 
    .default = NA
  ))

#MT urgency and pain, throw out anyone who capped out
redcap <- redcap %>%
  mutate(vbt_mt_urgency = case_when(
    capped_out == 0 ~ vbt_mt_urgency, 
    capped_out == 1 ~ NA
  )) %>%
  mutate(vbt_mt_pain = case_when(
    capped_out == 0 ~ vbt_mt_pain, 
    capped_out == 1 ~ NA
  ))

#add group to other rows
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    group_arm2 = first(na.omit(group_arm2)), 
    group_arm2 = case_match(
      group_arm2, 
      1 ~ "Dysmenorrhea", 
      2 ~ "Pain Free Control", 
      3 ~ "Dysmenorrhea plus Bladder Pain", 
      4 ~ "CP"
    )) %>%
  ungroup() 

#define groups based of FU and MT pain 
redcap <- redcap %>%  
  mutate(Group = case_when(
    redcap_event_name == "virtual_assessment_arm_1" & 
      group_arm2 == "Pain Free Control" ~ "Pain Free Control",
    redcap_event_name == "virtual_assessment_arm_1" &
      group_arm2 != "Pain Free Control" &
      vbt_fu_pain < 16  | 
      (is.na(vbt_fu_pain) & vbt_mt_pain < 16) ~ "Dysmenorrhea", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      group_arm2 != "Pain Free Control" &
      vbt_fu_pain > 15 | 
      (is.na(vbt_fu_pain) & vbt_mt_pain > 25) ~ "Dysmenorrhea plus Bladder Pain", 
    redcap_event_name == "virtual_assessment_arm_1" &
      group_arm2 != "Pain Free Control" &
      is.na(vbt_fu_pain) & 
      is.na(vbt_mt_pain) ~ "Unsuable",
    redcap_event_name == "virtual_assessment_arm_1" &
      group_arm2 != "Pain Free Control" &
      is.na(vbt_fu_pain) & 
      vbt_mt_pain > 15 & 
      vbt_mt_pain < 26 ~ "Grey zone"
  )) 

#flag rows where group_arm2 doesn't match groups above
redcap <- redcap %>%
  mutate(group_mismatch = case_when(
    redcap_event_name == "virtual_assessment_arm_1" &
      group_arm2 == Group ~ 0, 
    redcap_event_name == "virtual_assessment_arm_1" &
      group_arm2 != Group ~ 1
  ))

#visually inspect
redcap_groups <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  select(record_id, group_arm2, Group, group_mismatch, vbt_flag, vbt_fu_pain, vbt_mt_pain)

#add group and vbt flag to other rows
redcap <- redcap %>%
  group_by(record_id) %>%
  mutate(
    Group = first(na.omit(Group)),
    vbt_flag = first(na.omit(vbt_flag))) %>%
  ungroup()

#remove rows with flagged vbts or groups that are unusable or grey zone
redcap <- redcap %>%
  filter(vbt_flag == 0 & 
           (Group == "Dysmenorrhea" | 
              Group == "Dysmenorrhea plus Bladder Pain" | 
              Group == "Pain Free Control"))

#save cleaned dataset
write_csv(redcap, "Edited data files/redcap_post_cleaning.csv")  

#########################
##Table 1: Demographics##
#########################

#recode variables
#gender
redcap <- redcap %>%
  mutate(multi_gender = ifelse(rowSums(
    select(., mh_gender___1:mh_gender___8)) > 1, 1, 0)) %>%
  mutate(mh_gender_1_revised = ifelse((mh_gender___1 == 1) &
                               (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender_2_revised = ifelse((mh_gender___2 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender_3_revised = ifelse((mh_gender___3 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender_4_revised = ifelse((mh_gender___4 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender_5_revised = ifelse((mh_gender___5 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender_6_revised = ifelse((mh_gender___6 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender_7_revised = ifelse((mh_gender___7 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(mh_gender_8_revised = ifelse((mh_gender___8 == 1) &
                                  (multi_gender != 1), 1, 0)) %>%
  mutate(Gender = case_when(
    multi_gender == 1 ~ "Multiple Genders", 
    mh_gender_1_revised == 1 ~ "Woman", 
    mh_gender_2_revised == 1 ~ "Man", 
    mh_gender_3_revised == 1 ~ "Transgender Woman/Trans Femme", 
    mh_gender_4_revised == 1 ~ "Transgender Man/Trans Masc", 
    mh_gender_5_revised == 1 ~ "Nonbinary/Genderqueer/Gender Expansive",
    mh_gender_6_revised == 1 ~ "Two-spirit",
    mh_gender_7_revised == 1 | mh_gender_8_revised == 1 ~ "Unknown"
  ))
#race
redcap <- redcap %>%
  mutate(multi_race = ifelse(rowSums(
    select(., mh3_race___1:mh3_race___7)) > 1, 1, 0)) %>%
  mutate(mh3_race_1_revised = ifelse((mh3_race___1 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race_2_revised = ifelse((mh3_race___2 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race_3_revised = ifelse((mh3_race___3 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race_4_revised = ifelse((mh3_race___4 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race_5_revised = ifelse((mh3_race___5 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race_6_revised = ifelse((mh3_race___6 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(mh3_race_7_revised = ifelse((mh3_race___7 == 1) &
                                  (multi_race != 1), 1, 0)) %>%
  mutate(Race = case_when(
    multi_race == 1 ~ "Multiple Races", 
    mh3_race_1_revised == 1 ~ "Other", 
    mh3_race_2_revised == 1 ~ "Asian", 
    mh3_race_3_revised == 1 ~ "Other", 
    mh3_race_4_revised == 1 ~ "Black", 
    mh3_race_5_revised == 1 ~ "White",
    mh3_race_6_revised == 1 ~ "Unknown",
    mh3_race_7_revised == 1 ~ "Other"
  ))
#ethnicity
redcap <- redcap %>%
  mutate(mh4_ethnicity = case_match(
    mh4_ethnicity, 
    1 ~ "Hispanic or Latino", 
    2 ~ "Not Hispanic or Latino", 
    3 ~ "Unknown"
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
  mutate(Employment = case_when(
    multi_employment == 1 ~ "Multiple Employment Statuses", 
    mh6_employment_1_revised == 1 ~ "Work Full-Time", 
    mh6_employment_2_revised == 1 ~ "Work Part-Time", 
    mh6_employment_3_revised == 1 ~ "Homemaker", 
    mh6_employment_4_revised == 1 ~ "Retired", 
    mh6_employment_5_revised == 1 ~ "Disabled",
    mh6_employment_6_revised == 1 ~ "Student",
    mh6_employment_7_revised == 1 ~ "Unemployed"
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
#new var for ever given birth
redcap <- redcap %>%
  mutate(birth_yn = case_when(
    redcap_event_name == "virtual_assessment_arm_1" &
      mh15_vagbirths > 0 | mh14_csection > 0 ~ 1, 
    redcap_event_name == "virtual_assessment_arm_1" &
      mh15_vagbirths == 0 & mh14_csection == 0 ~ 0
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
    Ethnicity = mh4_ethnicity, 
    Education = mh5_education, 
    `Nicotine Usage` = mh_smoking, 
    `THC (Marijuana) Usage` = ms_thc, 
    `Ever had a problem with drugs or alcohol` = mh9b1, 
    `Given Birth` = birth_yn,
    Income = mh_income
  )

vars <- c("Age", "Gender", "Race", "Ethnicity", 
          "Education", "Employment", "Income", "Given Birth", "Nicotine Usage", "THC (Marijuana) Usage", 
          "Ever had a problem with drugs or alcohol")
factor_vars <- c("Gender", "Race", "Ethnicity", 
                 "Education", "Employment", "Income", "Given Birth", 
                 "Nicotine Usage", "THC (Marijuana) Usage", 
                 "Ever had a problem with drugs or alcohol")


#Creating table
demo <- CreateTableOne(vars, data = redcap_table1, factorVars = factor_vars, 
                       strata = "Group")

demo_df <- as.data.frame(print(demo, 
                               nonnormal = "Age",
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE))

#clean up
demo_df <- demo_df %>%
  rownames_to_column("Variable")

demo_df <- demo_df %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

# Remove p-value/test columns
cols_to_remove <- c("p", "test")
demo_df <- demo_df[, !colnames(demo_df) %in% cols_to_remove]


#building flextable
demo_ft <- flextable(demo_df) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(demo_ft, path = "Tables/Final/Table1_demographics.docx")

######################################################################
##Table 2: Menstrual Pain Characteristics and Hormonal Therapy Usage##
######################################################################

#recode variables
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
  mutate(`Menstrual Cycle Phase at time of Assessment` = case_when(
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

#count of responses for mh23a and mh27b, uncomment to view
#redcap_subset <- redcap %>%
#  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
#  select(record_id, Group, mh23a, mh27b)
#redcap_subset %>%
#  group_by(Group) %>%
#  summarise(
#    mh23a = sum(!is.na(mh23a)),
#    mh27b = sum(!is.na(mh27b))
#  )

#saving file
write_csv(redcap, "Edited data files/redcap_post_table2.csv")  

#Defining vars for table 2
redcap_table2 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
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
    `Used hormonal therapy to treat menstrual pain` = mh28,
    `Bladder, bowel, or abdomino-pelvic pain outside of period in last 3 months` = mh30
  )

vars <- c("Period in the last 6 months?", "Ever had painful periods?", 
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
          "Menstrual Cycle Phase at time of Assessment", "Average bleeding days per period", 
          "Used hormonal therpary to treat menstrual pain", 
          "Bladder, bowel, or abdomino-pelvic pain outside of period in last 3 months")

factor_vars <- c("Period in the last 6 months?", "Ever had painful periods?", 
                 "Painful periods at time of menarche?", "Menstrual Cycle Regularity", 
                 "Menstrual Cycle Length", "Menstrual Cycle Phase at time of Assessment",
                  "Used hormonal therpary to treat menstrual pain", 
                  "Bladder, bowel, or abdomino-pelvic pain outside of period in last 3 months")

#Creating summary table 2
sum <- CreateTableOne(vars, data = redcap_table2, factorVars = factor_vars, 
                       strata = "Group")

sum_df <- as.data.frame(print(sum, 
                               nonnormal = c("What age did your painful periods start, if not menarche?",
                                             "Years since menarche without a period",
                                             "Average bleeding days per period",
                                             "Days with menstrual pelvic pain >= 4 in an average month", 
                                             "Days of missed work, school, or activities due to painful period in last 3 months", 
                                             "Days spent in bed due to painful period in last 3 months",
                                             "Average pain during worst day of period when not taking pain relievers in last 3 months (VAS)",
                                             "Average pain during worst day of period when taking NSAID pain relievers in last 3 months (VAS)",
                                             "Average pain during worst day of period when taking acetaminophen pain relievers in last 3 months (VAS)"),
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table2_p <- redcap_table2 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table2_p, factorVars = factor_vars, 
                       strata = "Group")

comp_df <- as.data.frame(print(comp, 
                               nonnormal = c("What age did your painful periods start, if not menarche?",
                                             "Years since menarche without a period",
                                             "Average bleeding days per period",
                                             "Days with menstrual pelvic pain >= 4 in an average month", 
                                             "Days of missed work, school, or activities due to painful period in last 3 months", 
                                             "Days spent in bed due to painful period in last 3 months",
                                             "Average pain during worst day of period when not taking pain relievers in last 3 months (VAS)",
                                             "Average pain during worst day of period when taking NSAID pain relievers in last 3 months (VAS)",
                                             "Average pain during worst day of period when taking acetaminophen pain relievers in last 3 months (VAS)"),
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE, 
                               pValues = TRUE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table2 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table2 <- table2 %>%
  rownames_to_column("Variable")

table2 <- table2 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table2 <- flextable(table2) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table2, path = "Tables/Final/Table2_menstrualpain.docx")

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
#acetaminophen use
redcap <- redcap %>%
  mutate(mh_acetaminophen_yn = case_match(
    mh_acetaminophen_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#ibuprofen use
redcap <- redcap %>%
  mutate(mh_ibuprofen_yn = case_match(
    mh_ibuprofen_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#other med use
redcap <- redcap %>%
  mutate(mh_othermeds_yn = case_match(
    mh_othermeds_yn,
    1 ~ "Yes", 
    0 ~ "No"
  ))
#antidepressants use
redcap <- redcap %>%
  mutate(mh_antidepressants = case_when(
    mh_antidepressants == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_antidepressants) ~ "No"
  ))

#saving file
write_csv(redcap, "Edited data files/redcap_post_table3.csv")  

#Defining vars for table 3
redcap_table3 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Has Medical Insurance` = mh_insurance_yn,
    `Has a Regular Physician` = mh_physician_yn, 
    `Diagnosed with anxiety` = mh_anx, 
    `Diagnosed with depression` = mh_dep, 
    `Diagnosed with kidney stones` = mh_kidney_stone, 
    `Diagnosed with endometriosis (w/o chronic pain)` = mh_endo,
    `Diagnosed with fibroids` = mh_fibroids, 
    `Used acetaminophen regularly in past year to treat menstrual pain` = mh_acetaminophen_yn, 
    `Used ibuprofen regularly in past year to treat menstrual pain` = mh_ibuprofen_yn, 
    `Used other anti-inflammatory regularly in past year to treat menstrual pain` = mh_othermeds_yn, 
    `Used antidepressants regularly in past year (for any indication)` = mh_antidepressants
  )

vars = c("Has Medical Insurace", "Has a Regular Physician", "Diagnosed with anxiety", 
         "Diagnosed with depression", "Diagnosed with kidney stones", 
         "Diagnosed with endometriosis (w/o chronic pain)", "Diagnosed with fibroids", 
         "Used acetaminophen regulary in past year to treat menstrual pain", 
         "Used ibuprofen regulary in past year to treat menstrual pain", 
         "Used other anti-inflammatory regulary in past year to treat menstrual pain", 
         "Used antidepressants regularly in past year (for any indication)"
         )

#Creating summary table 3
sum <- CreateTableOne(vars, data = redcap_table3, factorVars = vars, 
                      strata = "Group")

sum_df <- as.data.frame(print(sum,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table3_p <- redcap_table3 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table3_p, factorVars = vars, 
                       strata = "Group")

comp_df <- as.data.frame(print(comp,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table3 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table3 <- table3 %>%
  rownames_to_column("Variable")

table3 <- table3 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table3 <- flextable(table3) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table3, path = "Tables/Final/Table3_medhx.docx")

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
#sensitivity to chemicals
redcap <- redcap %>%
  mutate(mh_gss4 = case_when(
    mh_gss4 == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_gss4) ~ "No"
  ))
#intercourse
redcap <- redcap %>%
  mutate(werf_c15sexyesno = case_match(
    werf_c15sexyesno, 
    1 ~ "No",
    2 ~ "Yes"
  ))

#saving file
write_csv(redcap, "Edited data files/redcap_post_table4.csv") 

#Defining vars for table 4
redcap_table4 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Average NMPP, last 7 days` = mcgill_1,
    `Average dysuria, last 7 days` = mcgill_2, 
    `Average pain with bowel movements, last 7 days` = mcgill_3, 
    `Had sexual intercourse, last 7 days` = mcgill_4, 
    `Average dyspareunia, last 7 days` = mcgill_5,
    `Meets criteria for BPS/IC` = bps_ic_bl, 
    `Meets criteria for IBS` = ibs_bl, 
    `Number bodily pain sites, last 30 days` = copc_sum,
    `Persistent fatigue` = mh_fatigue, 
    `Sensitivity to sounds` = mh_gss1, 
    `Sensitivity to odors` = mh_gss2, 
    `Sensitivity to bright lights` = mh_gss3, 
    `Sensitivity to chemicals` = mh_gss4,
    `Ever had sexual intercourse` = werf_c15sexyesno
  )

   
vars = c("Average NMPP, last 7 days", 
         "Average dysuria, last 7 days", 
         "Average pain with bowel movements, last 7 days",
         "Had sexual intercourse, last 7 days", 
         "Ever had sexual intercourse",
         "Average dyspareunia, last 7 days", 
         "Meets criteria for BPS/IC", 
         "Meets criteria for IBS", 
         "Number bodily pain sites, last 30 days", 
         "Persistent fatigue", 
         "Sensitivity to sounds", 
         "Sensitivity to odors", 
         "Sensitivity to chemicals"
)
    
factor_vars <- c("Meets criteria for BPS/IC", 
                 "Meets criteria for IBS",
                 "Meets criteria for Diagnosed Endometriosis",
                 "Persistent fatigue", 
                 "Sensitivity to sounds", 
                 "Sensitivity to odors", 
                 "Sensitivity to chemicals",
                 "Had sexual intercourse, last 7 days",
                 "Ever had sexual intercourse"
)

nonnormal_vars <- c("Average NMPP, last 7 days", 
                    "Average dysuria, last 7 days", 
                    "Average pain with bowel movements, last 7 days",
                    "Average dyspareunia, last 7 days",
                    "Number bodily pain sites, last 30 days"
)
    
#Creating summary table 4
sum <- CreateTableOne(vars, data = redcap_table4, factorVars = factor_vars, 
                      strata = "Group")

sum_df <- as.data.frame(print(sum,
                              nonnormal = nonnormal_vars,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

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
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table4 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table4 <- table4 %>%
  rownames_to_column("Variable")

table4 <- table4 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table4 <- flextable(table4) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table4, path = "Tables/Final/Table4_painsensory.docx")

    
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
  select(1, 380:387)

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
  mutate(gss_balance = case_when(
    gss_balance == 1 ~ 1,
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(gss_balance) ~ 0
  )) %>%
  mutate(gss_mouth = case_when(
    gss_mouth == 1 ~ 1,
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(gss_mouth) ~ 0
  )) %>%
  mutate(gss_heart = case_when(
    gss_heart == 1 ~ 1,
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(gss_heart) ~ 0
  )) %>%
  mutate(gss_chemicals = case_when(
    gss_chemicals == 1 ~ 1,
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(gss_chemicals) ~ 0
  )) %>%
  mutate(gss_sound = case_when(
    gss_sound == 1 ~ 1,
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(gss_sound) ~ 0
  )) %>%
  mutate(gss_light = case_when(
    gss_light == 1 ~ 1,
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(gss_light) ~ 0
  ))

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
redcap_sup_vars <- read_csv("Raw files/EH22236CRAMPP2-VBTPredictiveFactors_DATA_2026-04-23_1230.csv")

#filter to only inclue gisx78
redcap_sup_gisx78 <- redcap_sup_vars %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  select(record_id, promis_gisx78)

redcap <- redcap %>%
  left_join(redcap_sup_gisx78, by = "record_id") %>%
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

#Defining vars for table 5
redcap_table5 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Promis physical function` = promis_pf_t_score, 
    `Promis anxiety` = promis_anx_t_score, 
    `Promis depression` = promis_dep_t_score, 
    `Promis fatigue` = promis_fat_t_score, 
    `Promis sleep disturbance` = promis_sd_t_score, 
    `Promis social roles` = promis_sr_t_score, 
    `Promis pain interference` = promis_pi_t_score, 
    `Promis cognitive function` = promis_cf_t_score, 
    `Promis average pain` = promis_average_pain, 
    `Promis PROPr` = PROPr, 
    `Promis belly pain` = promis_belly_t_score, 
    `GSS` = gss_sum, 
    `MacArthur U.S. standing` = macarthur_ladder_us, 
    `MacArthur community standing` = macarthur_ladder_community, 
    `ACES` = aces_sum, 
    `Perceived stress` = pss_sum, 
    `GSRS` = gsrs_bl, 
    `ICSI` = icsi_bl, 
    `GUPI` = gupi_bl
  )

vars <- c("Promis physical function", 
          "Promis anxiety", 
          "Promis depression", 
          "Promis fatigue", 
          "Promis sleep disturbance", 
          "Promis social roles", 
          "Promis pain interference", 
          "Promis cognitive function", 
          "Promis average pain", 
          "Promis PROPr", 
          "Promis belly pain", 
          "PCS-T", 
          "PCS-R", 
          "PCS-M", 
          "PCS-H", 
          "GSS", 
          "MacArthur U.S. standing", 
          "MacArthur community standing", 
          "ACES",
          "Perceived stress",
          "GSRS",
          "ICSI",
          "GUPI"
)

nonnormal_vars <- c("Promis physical function", 
                    "Promis anxiety", 
                    "Promis depression", 
                    "Promis fatigue", 
                    "Promis sleep disturbance", 
                    "Promis social roles", 
                    "Promis pain interference", 
                    "Promis cognitive function", 
                    "Promis average pain", 
                    "Promis PROPr", 
                    "Promis belly pain", 
                    "PCS-T", 
                    "PCS-R", 
                    "PCS-M", 
                    "PCS-H", 
                    "GSS", 
                    "MacArthur U.S. standing", 
                    "MacArthur community standing", 
                    "ACES",
                    "Perceived stress",
                    "GSRS",
                    "ICSI",
                    "GUPI"
)

#Creating summary table 5
sum <- CreateTableOne(vars, data = redcap_table5, strata = "Group")

sum_df <- as.data.frame(print(sum,
                              nonnormal = nonnormal_vars,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table5_p <- redcap_table5 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table5_p, strata = "Group")

comp_df <- as.data.frame(print(comp,
                               nonnormal = vars,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table5 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table5 <- table5 %>%
  rownames_to_column("Variable")

table5 <- table5 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table5 <- flextable(table5) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table5, path = "Tables/Final/Table5_psychosocial.docx")

########################
##Table 6: Medications##
########################

#recode vars
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
#triptans use
redcap <- redcap %>%
  mutate(mh_triptans = case_when(
    mh_triptans == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_triptans) ~ "No"
  ))
#beta blocker use
redcap <- redcap %>%
  mutate(mh_betablocker = case_when(
    mh_betablocker == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_betablocker) ~ "No"
  ))
#tranq use
redcap <- redcap %>%
  mutate(mh_tranq = case_when(
    mh_tranq == 1 ~ "Yes", 
    redcap_event_name == "virtual_assessment_arm_1" & 
      is.na(mh_tranq) ~ "No"
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
#ibuprofen amounts
redcap <- redcap %>%
  mutate(mh_ibuprofen_amount = case_when(
    mh_ibuprofen_amount == 1 ~ "1-2", 
    mh_ibuprofen_amount == 2 ~ "3-5", 
    mh_ibuprofen_amount == 3 ~ "6-14", 
    mh_ibuprofen_amount == 4 ~ "15+", 
    mh_ibuprofen_yn == "No" ~ "0"
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

#saving file
write_csv(redcap, "Edited data files/redcap_post_table6.csv")

#Defining vars for table 6
redcap_table6 <- redcap %>%
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
    `Used triptans regularly in past year (for any indication)` = mh_triptans,
    `Used beta-blockers regularly in past year (for any indication)` = mh_betablocker, 
    `Used minor tranquilizers regularly in past year (for any indication)` = mh_tranq,
    `Number of acetaminophen tablets taken on average per period` = mh_acetaminophen_amount, 
    `Number of ibuprofen tablets taken on average per period` = mh_ibuprofen_amount, 
    `Number of other anti-inflammatory tablets taken on average per period` = mh_othermed_amount
  )

vars <- c("Oral Contraceptive Pills", "Contraceptive Patch", "Vaginal Ring", 
          "Progestin Implant", "Progestin Shot", "Progestin Pills", 
          "GnRH Agonists", "GnRH Antagonists", "Hormonal IUD", "Copper IUD", 
          "Aromatase Inhibitors", "Other Hormonal Medication",
          "Used triptans regularly in past year (for any indication)",
          "Used beta-blockers regularly in past year (for any indication)", 
          "Used minor tranquilizers regularly in past year (for any indication)",
          "Number of acetaminophen tablets taken on average per period", 
          "Number of ibuprofen tablets taken on average per period", 
          "Number of other anti-inflammatory tablets taken on average per period" 
          )

#Creating summary table 6
sum <- CreateTableOne(vars, data = redcap_table6, strata = "Group")

sum_df <- as.data.frame(print(sum,
                              nonnormal = nonnormal_vars,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table6_p <- redcap_table6 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table6_p, strata = "Group")

comp_df <- as.data.frame(print(comp,
                               nonnormal = vars,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table6 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table6 <- table6 %>%
  rownames_to_column("Variable")

table6 <- table6 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table6 <- flextable(table6) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table6, path = "Tables/Final/Table6_medication.docx")

#####################################
##Table 7: Diagnoses and Procedures##
#####################################

#recode vars
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

#saving file
write_csv(redcap, "Edited data files/redcap_post_table7.csv")

#Defining vars for table 7
redcap_table7 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Diagnosed with persistent ovarian cysts` = mh_cysts, 
    `Diagnosed with pelvic inflammatory disease` = mh_pid, 
    `Diagnosed with chronic constipation` = mh_constipation, 
    `Diagnosed with chronic diarrhea` = mh_diarrhea, 
    `Diagnosed with repeated UTIs` = mh_uti, 
    `Diagnosed with repeated vaginal infections` = mh_vag_infection, 
    `Diagnosed with childhood pelvic health problem` = mh_child_pelvic, 
    `Diagnosed with chronic heart condition (needing treatment)` = mh_heart, 
    `Diagnosed with chronic lung condition (needing treatment)` = mh_lung, 
    `Diagnosed with chronic liver condition (needing treatment)` = mh_liver, 
    `Fibroid removal surgery` = mh_fibroid_surg, 
    `Vaginal surgery` = mh_vaginal_surg, 
    `Other major pelvic surgery` = mh_other_surg
  )

vars = c("Diagnosed with persistent ovarian cysts", "Diagnosed with pelvic inflammatory disease", 
         "Diagnosed with chronic constipation", "Diagnosed with chronic diarrhea", 
         "Diagnosed with repeated UTIs", "Diagnosed with repeated vaginal infections", 
         "Diagnosed with childhood pelvic health problem", 
         "Diagnosed with chronic heart condition (needing treatment)",
         "Diagnosed with chronic lung condition (needing treatment)", 
         "Diagnosed with chronic liver condition (needing treatment)", 
         "Fibroid removal surgery", "Vaginal surgery", 
         "Other major pelvic surgery"
)

#Creating summary table 6
sum <- CreateTableOne(vars, data = redcap_table7, strata = "Group")

sum_df <- as.data.frame(print(sum,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table7_p <- redcap_table7 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table7_p, strata = "Group")

comp_df <- as.data.frame(print(comp,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table7 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table7 <- table7 %>%
  rownames_to_column("Variable")

table7 <- table7 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table7 <- flextable(table7) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table7, path = "Tables/Final/Table7_diagnoses.docx")


##################
##Table 8: Diary##
################## 

#recode vars
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
  select(1, 409) 

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
  select(1, 410) 

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
  select(1, 411) 

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
write_csv(redcap, "Edited data files/redcap_post_table8.csv") 


#Defining vars for table 8
redcap_table8 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Days of moderate to heavy bleeding reported (5 days max)` = dd_bleeding_m_h,
    `Average pelvic pain (24 hours before period onset)` = dd_painbefore,
    `Days with menstrual pain > 3 (5 days max)` = dd_pain_3, 
    `Average menstrual pain` = dd_avg_menstrual_pain, 
    `Max menstrual pain` = max_pain_bl, 
    `Max bowel pain` = dd_bowel_max, 
    `Max bladder pain` = dd_bladder_max, 
    `Days of pain reliever usage (5 days max)` = dd_medication
  )


vars = c( 
         "Days of moderate to heavy bleeding reported (5 days max)", 
         "Average pelvic pain (24 hours before period onset)", 
         "Days with menstrual pain > 3 (5 days max)", 
         "Average menstrual pain", 
         "Max menstrual pain", 
         "Max bowel pain", 
         "Max bladder pain", 
         "Days of pain reliever usage (5 days max)"
)

#Creating summary table 8
sum <- CreateTableOne(vars, data = redcap_table8, strata = "Group")

sum_df <- as.data.frame(print(sum,
                              printToggle = FALSE,
                              nonnormal = vars,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table8_p <- redcap_table8 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table8_p, strata = "Group")

comp_df <- as.data.frame(print(comp,
                               printToggle = FALSE,
                               nonnormal = vars,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table8 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table8 <- table8 %>%
  rownames_to_column("Variable")

table8 <- table8 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table8 <- flextable(table8) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table8, path = "Tables/Final/Table8_diary.docx")

########################
##Table 9: Dyspareunia##
########################

#recode vars
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
#vulvodynia criteria
redcap <- redcap %>%
  mutate(`Meets criteria for Vulvodynia` = copc_vulvo_1 + copc_vulvo_2) %>%
  mutate(`Meets criteria for Vulvodynia` = case_match(
    `Meets criteria for Vulvodynia`, 
    2 ~ "Yes", 
    1 ~ "No", 
    0 ~ "No"
  ))

#saving file
write_csv(redcap, "Edited data files/redcap_post_table9.csv") 


#Defining vars for table 9
redcap_table9 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Ever had dyspareunia`= werf_c15, 
    `Age when dyspareunia began` = werf_c16, 
    `Dyspareunia during most recent sexual intercourse` = werf_c17, 
    `Max severity of dyspareunia during most recent sexual intercourse` = werf_c19, 
    `Frequency of dyspareunia, last 12 months` = werf_c21, 
    `Dyspareunia resulting in disruption of sexual intercourse` = werf_c23
  )

vars = c("Ever had dyspareunia", 
         "Age when dyspareunia began", 
         "Dyspareunia during most recent sexual intercourse", 
         "Max severity of dyspareunia during most recent sexual intercourse", 
         "Frequency of dyspareunia, last 12 months", 
         "Dyspareunia resulting in disruption of sexual intercourse", 
         "Meets criteria for Vulvodynia"
)

factor_vars <- c("Ever had dyspareunia", 
                 "Dyspareunia during most recent sexual intercourse",
                 "Frequency of dyspareunia, last 12 months", 
                 "Dyspareunia resulting in disruption of sexual intercourse",
                 "Meets criteria for Vulvodynia"
)

nonnormal_vars <- c("Age when dyspareunia began", 
                    "Max severity of dyspareunia during most recent sexual intercourse"
)

#Creating summary table 9
sum <- CreateTableOne(vars, data = redcap_table9, strata = "Group", factorVars = factor_vars)

sum_df <- as.data.frame(print(sum,
                              printToggle = FALSE,
                              nonnormal = nonnormal_vars,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table9_p <- redcap_table9 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table9_p, strata = "Group", factorVars = factor_vars)

comp_df <- as.data.frame(print(comp,
                               printToggle = FALSE,
                               nonnormal = nonnormal_vars,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table9 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table9 <- table9 %>%
  rownames_to_column("Variable")

table9 <- table9 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table9 <- flextable(table9) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table9, path = "Tables/Final/Table9_dyspareunia.docx")


#################
##Table 10: VBT##
################# 

#load in VBT urine pan volume spreadsheet
urine_volumes <- read_excel("Raw files/URINE SAMPLE MEASURES_04.21.26.xlsx")

#merge with larger dataset
redcap <- redcap %>%
  left_join(urine_volumes, by = "record_id") %>%
  mutate(bl_urine_ml = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                              bl_urine_ml, NA))

#load in supp vars
redcap_sup_vars <- read_csv("Raw files/EH22236CRAMPP2-VBTPredictiveFactors_DATA_2026-04-23_1230.csv")

#merge in in-person bladder test fu pain from sup vars
redcap_sup_ipb <- redcap_sup_vars %>%
  filter(redcap_event_name == "inperson_visit__ba_arm_1") %>%
  select(record_id, ipb_fupain)

redcap <- redcap %>%
  left_join(redcap_sup_ipb, by = "record_id") %>%
  mutate(ipb_fupain = ifelse(redcap_event_name == "virtual_assessment_arm_1", 
                             ipb_fupain, NA))

#tampon test pain 
redcap <- redcap %>%
  mutate(tampon_test = case_when(
    tampon_test == 99 ~ NA, 
    tampon_test == 0 ~ 0,
    tampon_test == 10 ~ 10, 
    TRUE ~ tampon_test
  ))

#count of responses for tampon test, urine, and in person tests - uncomment to view
#redcap_subset <- redcap %>%
#  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
#  select(record_id, Group, tampon_test, bl_urine_ml, ipb_fupain)
#redcap_subset %>%
#  group_by(Group) %>%
#  summarise(
#    tampon_test = sum(!is.na(tampon_test)),
#    bl_urine_ml = sum(!is.na(bl_urine_ml)), 
#    ipb_fupain = sum(!is.na(ipb_fupain))
#  )

#saving file
write_csv(redcap, "Edited data files/redcap_post_table10.csv") 


#Defining vars for table 10
redcap_table10 <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  rename(
    `Time to FS (mins)` = time_drinking_fs_mins, 
    `Time to FU (mins)` = time_drinking_fu_mins, 
    `Time to MT (mins)` = time_drinking_mt_mins, 
    `FS bladder urgency` = vbt_fs_urgency, 
    `FS bladder pain` = vbt_fs_pain, 
    `FU bladder urgency` = vbt_fu_urgency, 
    `FU bladder pain` = vbt_fu_pain, 
    `MT bladder urgency` = vbt_mt_urgency, 
    `MT bladder pain` = vbt_mt_pain, 
    `MT sharp pain` = vbt_sharp, 
    `MT pressing pain` = vbt_pressing, 
    `Capped out of bladder task` = capped_out, 
    `Urine Volume (ml)` = bl_urine_ml, 
    `Pain with tampon test` = tampon_test
  )

vars <- c("Time to FS (mins)",
          "Time to FU (mins)",
          "Time to MT (mins)",
          "FS bladder urgency",
          "FS bladder pain",
          "FU bladder urgency",
          "FU bladder pain",
          "MT bladder urgency",
          "MT bladder pain", 
          "MT sharp pain",
          "MT pressing pain",
          "Capped out of bladder task", 
          "Urine Volume (ml)",
          "Pain with tampon test"
)

#Creating summary table 10
sum <- CreateTableOne(vars, data = redcap_table10, strata = "Group", 
                      factorVars = "Capped out of bladder task")

sum_df <- as.data.frame(print(sum,
                              nonnormal = vars,
                              printToggle = FALSE,
                              quote = FALSE,
                              noSpaces = TRUE,
                              showAllLevels = FALSE))

# Remove test columns
cols_to_remove <- "test"
sum_df <- sum_df[, !colnames(sum_df) %in% cols_to_remove]

#Creating table with comparisons for DYS and DYSB
redcap_table10_p <- redcap_table10 %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table10_p, strata = "Group",
                       factorVars = "Capped out of bladder task")

comp_df <- as.data.frame(print(comp,
                               nonnormal = vars,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = FALSE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
table10 <- cbind(sum_df, p_dys_dysb = comp_df$p )

#clean up
table10 <- table10 %>%
  rownames_to_column("Variable")

table10 <- table10 %>%
  mutate(Variable = gsub("\\.\\.\\.", " ", Variable), # replace ... with space
         Variable = gsub("\\.\\.+", " ", Variable),   # replace .. with space
         Variable = gsub("\\.", " ", Variable),       # replace remaining dots with space
         Variable = gsub("^X\\s+", "  ", Variable),   # replace leading X + space with indent
         Variable = trimws(Variable))                 # trim extra whitespace

#building flextable
table10 <- flextable(table10) %>%
  set_header_labels(Variable = "") %>%        # removes "Variable" header
  autofit() %>%                               # fits column widths to content
  bold(part = "header") %>%                   # bold header row
  hline_top(part = "header", 
            border = fp_border(width = 1.5)) %>%   # line above header
  hline_bottom(part = "header", 
               border = fp_border(width = 1.5)) %>% # line below header
  hline_bottom(part = "body", 
               border = fp_border(width = 1.5)) %>% # line at bottom
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(table10, path = "Tables/Final/Table10_vbt.docx")

#OLD
#####################################
##FDR corrections for key variables##
#####################################

#Defining vars for FDR correction, throwing out anyone flagged for weird VBT times
redcap_table_fdr <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1") %>%
  filter(vbt_flag == 0) %>%
  rename(GSRS = gsrs_bl, 
         ICSI = icsi_bl, 
         GUPI = gupi_bl)

vars <- c("GSRS",
          "ICSI",
          "GUPI")

#compute p values for FDR correction
redcap_table_fdr_p <- redcap_table_fdr %>%
  filter(Group %in% c("Dysmenorrhea", "Dysmenorrhea plus Bladder Pain"))

comp <- CreateTableOne(vars, data = redcap_table_fdr_p, strata = "Group", test = TRUE)

comp_df_p <- as.data.frame(print(comp,
                               nonnormal = vars,
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE, 
                               pValues = TRUE)) %>%
  dplyr::select("p")

# Convert to numeric (some might be "<0.001" text)
comp_df_p <- comp_df_p %>%
  dplyr::mutate(
    p = as.numeric(gsub("<", "", p)),
    FDR_p = p.adjust(p, method = "fdr")
  )

#building flextable
ft <- flextable(comp_df_p) 

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/VBT_Predictive_Factors_Table_FDR.docx")

