#VBT Predictive Factors ~ part 1
#Written by Sarah Darnell, last modified 7.18.25

library(readr)
library(dplyr)

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
###########
##table 1##
###########

#recode variables
#group
redcap <- redcap %>%
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
  )) %>%
  rename(`Assigned Sex at Birth` = mh_assigned_sex)

#saving file
write_csv(redcap, "Edited data files/redcap.csv")           

