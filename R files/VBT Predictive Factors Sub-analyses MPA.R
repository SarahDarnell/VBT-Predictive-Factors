#VBT Predictive Factors Sub-analyses - MPA projects for ZFK and KJ
##Written by Sarah Darnell, last modified 9.18.25

library(readr)
library(dplyr)
library(purrr)
library(FSA)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/VBT Predictive Factors CRAMPP2")

#load latest version of dataset

##########################################################################
##ZFK project - THC usage and correlations to promis 29+2 and pain at FU##
##########################################################################

##kruskal-Wallis test for group comparisons of vars

#pull out only VBT vars
redcap_subset <- redcap %>%
  filter(redcap_event_name == "virtual_assessment_arm_1")

#define vars
vars <- c("promis_pf_t_score", "promis_anx_t_score", "promis_dep_t_score", 
          "promis_fat_t_score", "promis_sd_t_score", "promis_sr_t_score", 
          "promis_pi_t_score", "promis_cf_t_score", "promis_average_pain", 
          "PROPr", "vbt_fu_pain")

kw <- lapply(vars, function(var) {
  formula <- as.formula(paste(var, "~Group"))
  # Check if variable has enough data to run test
  temp <- redcap_subset[, c(var, "Group")]
  temp <- temp[complete.cases(temp), ]  # remove NAs
  
  if (length(unique(temp$Group)) < 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)  # skip this variable
  }
  test_result <- kruskal.test(formula, data = temp)
  
  data.frame(
    Variable = var, 
    Chi_Square = test_result$statistic, 
    df = test_result$parameter, 
    p_value = test_result$p.value, 
    stringsAsFactors = FALSE
  )
})

kw_results <- do.call(rbind, kw) 
kw_results <- kw_results %>%
  mutate(p_value = round(p_value, 5)) %>%
  select(-df)

#Dunn’s test to determine which groups differ 
dunn_results <- map(vars, function(var) {
  temp <- redcap_subset[, c(var, "Group")] %>% na.omit()
  
  dunn <- dunnTest(as.formula(paste(var, "~ Group")), 
                   data = temp, method = "bonferroni")
  
  dunn$res %>%
    mutate(Variable = var) %>%
    select(Variable, Comparison, Z = Z, P_unadj = P.unadj, P_adj = P.adj)
})

# Combine into one results table
dunn_results <- bind_rows(dunn_results)

dunn_results <- dunn_results %>%
  mutate(P_adj = ifelse(P_adj < 0.001, "<0.001", 
                        format(round(P_adj, 4), nsmall = 4)))

