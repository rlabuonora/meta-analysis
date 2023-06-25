#Libraries needed
library(tidyverse)
library(metapsyTools)
library(readxl)
library(magrittr)

#Reading dataset into workspace
#data <- read_excel("./data/non_response.xlsx")
data <- read_excel("./data/metapsydate_1.xlsx")

#for analysis of attempts#
data %>% filter(attempters_arm1 >= 0) %>%
  mutate(event_arm1 = n_resp_arm1,
         event_arm2 = n_resp_arm2,
         totaln_arm1 = n_arm1,
         totaln_arm2 = n_arm2) %>%
  # commented these lines out because checkDataFormat() expects the original names
  select(rating, study, n_arm1, mean_arm1, sd_arm1, n_arm2, mean_arm2, sd_arm2,
         direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, 
         instrument, time_weeks, event_arm1, event_arm2,totaln_arm1,totaln_arm2, time) %>% 
  mutate(across(c(time_weeks, event_arm1, event_arm2, totaln_arm1, totaln_arm2), 
                as.numeric)) -> attempts

attempts$outcome_type <- "RR"


# check with metasyTools

attempts <- checkDataFormat(attempts)
attempts <- calculateEffectSizes(attempts)
res_attempt <- runMetaAnalysis(attempts, es.measure = "RR", which.run = 
                                 c("overall", "combined", "lowest.highest", 
                                   "outliers", "influence", "threelevel", 
                                   "threelevel.che"),
                               which.outliers = "combined",
                               which.influence = "combined",
                               which.rob = "threelevel.che",
                               vcov = "complex")
# inspect outcomes
res_attempt
