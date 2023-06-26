#Libraries needed
library(tidyverse)
library(metapsyTools)
library(readxl)
library(meta)
library(magrittr)
library(dmetar)


#Reading dataset into workspace
#data <- read_excel("./data/non_response.xlsx")
data <- read_excel("./data/metapsydate_1.xlsx") %>% 
  rename(therapy_type=29)

#for analysis of attempts#
data %>% filter(attempters_arm1 >= 0) %>%
  mutate(event_arm1 = n_resp_arm1,
         event_arm2 = n_resp_arm2,
         totaln_arm1 = n_arm1,
         totaln_arm2 = n_arm2) %>%
  # commented these lines out because checkDataFormat() expects the original names
  select(rating, study, n_arm1, mean_arm1, sd_arm1, n_arm2, mean_arm2, sd_arm2,
         direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, 
         format, country, recruitment_setting, delivery_setting, target_group,
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

plot(res_attempt, "overall", 
     col.predict = "red", 
     col.square = "purple",
     fontfamily = "Times New Roman",
     width = 10,
     height = 12)

correctPublicationBias(res_attempt, which.run = "overall")

funnel(res_attempt$model.overall, 
       studlab = TRUE, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("purple", "grey", "lightblue"))

# Subgrupos
sg_country <- subgroupAnalysis(res_attempt, country)
sg_format <- subgroupAnalysis(res_attempt, format)
sg_target_group <- subgroupAnalysis(res_attempt, target_group)
sg_recruitment_setting <- subgroupAnalysis(res_attempt, recruitment_setting)
sg_delivery_setting <- subgroupAnalysis(res_attempt, delivery_setting)

# sg_therapy_type <- subgroupAnalysis(res_attempt, therapy_type)


# eggers.test(res_attempt$model.overall)
