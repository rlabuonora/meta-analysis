#Libraries needed
library(tidyverse)
library(metapsyTools)
library(readxl)
library(meta)
library(dmetar)
library(magrittr)

#Reading dataset into workspace
# data <- read_excel("non_response.xlsx")
data <- read_excel("./data/metapsydate_1.xlsx")

data$time <- "post"
data$rating <- "NA"
# data$rob <- "NA"

# for analysis of non_response
data %>% 
  filter(n_resp_arm1 >= 0) %>%
  mutate(event_arm1 = n_resp_arm1,
         event_arm2 = n_resp_arm2,
         totaln_arm1 = n_arm1,
         totaln_arm2 = n_arm2) %>%
  # commented these lines out because checkDataFormat() expects the original names
  select(rating, study, n_arm1, mean_arm1, sd_arm1, n_arm2, mean_arm2, sd_arm2,
         direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, 
         format, country, recruitment_setting, delivery_setting, target_group,
         instrument, time_weeks, event_arm1, event_arm2,totaln_arm1,totaln_arm2, time) %>% 
  mutate(across (c(time_weeks, event_arm1, event_arm2, n_arm1, 
                   n_arm2), as.numeric)) -> non_response
non_response$outcome_type <- "RR"


non_response <- checkDataFormat(non_response)
checkConflicts(non_response)
non_response <- calculateEffectSizes(non_response)

res_non_response <- runMetaAnalysis(non_response, es.measure = "RR", 
                                    which.run = c("overall", "combined", 
                                                  "lowest.highest", "outliers", 
                                                  "influence", "threelevel", 
                                                  "threelevel.che"),
                                    which.outliers = "combined",
                                    which.influence = "combined",
                                    which.rob = "threelevel.che",
                                    vcov = "complex")
res_non_response
plot(res_non_response, "overall", 
     col.predict = "red", 
     col.square = "purple",
     fontfamily = "Times New Roman",
     width = 10,
     height = 12)

correctPublicationBias(res_non_response, which.run = "overall")

funnel(res_non_response$model.overall, 
       studlab = TRUE, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("purple", "grey", "lightblue"))

sg_country <- subgroupAnalysis(res_non_response, country)
sg_format <- subgroupAnalysis(res_non_response, format)
sg_target_group <- subgroupAnalysis(res_non_response, target_group)
sg_recruitment_setting <- subgroupAnalysis(res_non_response, recruitment_setting)
sg_delivery_setting <- subgroupAnalysis(res_non_response, delivery_setting)


# eggers.test(res_non_response$model.overall)
