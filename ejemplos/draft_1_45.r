remotes::install_github("metapsy-project/metapsyTools")

non_response <- read_excel("non_response.xlsx")


#Libraries needed
library(tidyverse)
library(metapsyTools)
library(readxl)
library(magrittr)

#Reading dataset into workspace
data <- read_excel("non_response.xlsx")


# for analysis of non_response
data %>% 
  #filter(non_response_arm1 >= 0) %>%
  # mutate(event_arm1 = non_response_arm1,
  #        event_arm2 = non_response_arm2,
  #        totaln_arm1 = n_arm1,
  #        totaln_arm2 = n_arm2) %>%
  # commented these lines out because checkDataFormat() expects the original names
  select(rating, study, n_arm1, mean_arm1, sd_arm1, n_arm2, mean_arm2, sd_arm2,
         direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, 
         instrument, time_weeks, event_arm1, event_arm2,totaln_arm1,totaln_arm2, time) %>% 
  mutate(across (c(time_weeks, event_arm1, event_arm2, n_arm1, 
                   n_arm2), as.numeric)) -> non_response
non_response$outcome_type <- "RR"


#for analysis of attempts#
data %>% filter(attempts_arm1 >= 0) %>%
  mutate(event_arm1 = non_response_arm1,
         event_arm2 = non_response_arm2,
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

non_response <- checkDataFormat(non_response)
checkConflicts(non_response)
non_response <- calculateEffectSizes(non_response)
attempts <- checkDataFormat(attempts)
attempts <- calculateEffectSizes(attempts)
# meta-analysis
res_non_response <- runMetaAnalysis(non_response, es.measure = "RR", 
                                    which.run = c("overall", "combined", 
                                                  "lowest.highest", "outliers", 
                                                  "influence", "threelevel", 
                                                  "threelevel.che"),
                                    which.outliers = "combined",
                                    which.influence = "combined",
                                    which.rob = "threelevel.che",
                                    vcov = "complex")
res_attempt <- runMetaAnalysis(attempts, es.measure = "RR", which.run = 
                                 c("overall", "combined", "lowest.highest", 
                                   "outliers", "influence", "threelevel", 
                                   "threelevel.che"),
                               which.outliers = "combined",
                               which.influence = "combined",
                               which.rob = "threelevel.che",
                               vcov = "complex")
# inspect outcomes
res_non_response
res_attempt
