setwd("C:/nameofyourfolderwhereyouputthedata/")

library(tidyverse)
library(metapsyTools)

library(readxl)
data <- read_excel("metapsydate_2.xlsx")

# choose proper control groups
table(data$condition_arm2, useNA = "always")
target <- c("medication", "wl", "cau", "ecau", "other ctr", "supp", "placebo", "mixed", "supp or ecau", "GHE: group HIV education")
data %>% filter(condition_arm2 %in% target) -> data

# data$time <- "post"
# data$rating <- "NA"
# data$rob <- "NA"
data$instrument <- "xx"

# For analysis of ideation
data %>%
  #filter (mean_arm1 > 0) %>%
  select(study, 
         #direct, 
         condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2, event_arm1,           
         event_arm2) %>% 
         #si_n_arm1,    	
         #si_n_arm2) %>%
  # rename(event_arm1 = si_event_arm1,
  #        event_arm2 = si_event_arm2,
  #        totaln_arm1 = si_n_arm1,
  #        totaln_arm2 = si_n_arm2) %>%
  mutate(across(c(mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2,
                   event_arm1, event_arm2, 
                   #totaln_arm1, totaln_arm2), 
                   time_weeks), as.numeric)) -> ideation_cont
ideation_cont$outcome_type <- "msd"

data %>%
  #filter (sd_arm1 > 0) %>%
  select(study, 
         #direct, c
         condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2,
         event_arm1,  totaln_arm1, totaln_arm2,         
         event_arm2) %>% 
        #	si_n_arm1,    	
         # si_n_arm2) %>%
  # rename(event_arm1 = si_event_arm1,
  #        event_arm2 = si_event_arm2,
  #        totaln_arm1 = si_n_arm1,
  #        totaln_arm2 = si_n_arm2) %>%
  mutate(across(c(time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2,
                   event_arm1, event_arm2, totaln_arm1, totaln_arm2), as.numeric)) -> ideation_dich
ideation_dich$outcome_type <- "deterioration"

# Sign of g based on binary outcomes is flipped!

ideation_dich$event_arm1 <- ideation_dich$totaln_arm1 - ideation_dich$event_arm1
ideation_dich$event_arm2 <- ideation_dich$totaln_arm2 - ideation_dich$event_arm2

ideation_cont %>% bind_rows(ideation_dich) -> ideation
ideation$time <- "post"
ideation$rating <- "NA"
# data$rob <- "NA"
data$instrument <- "xx"

# for analysis of attempts
data %>%
  filter(event_arm1 >= 0) %>%
  select(study, direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument, time_weeks, event_arm1,
         event_arm2, totaln_arm1, totaln_arm2) -> attempts
attempts$outcome_type <- "RR"

# check with MetaPsyTools
ideation <- checkDataFormat(ideation)
checkConflicts(ideation)
ideation <- calculateEffectSizes(ideation)

attempts <- checkDataFormat(attempts)
attempts <- calculateEffectSizes(attempts)

# check whether meta-analysis runs
resIdeation <- runMetaAnalysis(ideation, which.run = c("overall", "combined",
                                                       "lowest.highest", "outliers",
                                                       "influence", "threelevel", "threelevel.che"),
                               which.outliers = "combined",
                               which.influence = "combined",
                               which.rob = "threelevel.che",
                               vcov = "complex")

resAttempt <- runMetaAnalysis(attempts, es.measure = "RR", which.run = c("overall", "combined",
                                                                         "lowest.highest", "outliers",
                                                                         "influence", "threelevel", "threelevel.che"),
                              which.outliers = "combined",
                              which.influence = "combined",
                              which.rob = "threelevel.che",
                              vcov = "complex")

# inspect outcomes
resIdeation
resAttempt

# make forest plots
plot(resIdeation)
plot(resAttempt)

