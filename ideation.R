library(tidyverse)
library(meta)
library(metapsyTools)
library(dmetar)


library(readxl)
data <- read_excel("./data/metapsydate_2.xlsx") %>% 
  mutate(format=forcats::fct_recode(format,
                                    "Family"="fam",
                                    "Group"="grp",
                                    "Other"="oth",
                                    "Individual"="ind",
                                    "Face to Face"="ftf"))

data <- read_excel("./data/metapsydate_1.xlsx")
# choose proper control groups
table(data$condition_arm2, useNA = "always")
# target <- c("medication", "wl", "cau", "ecau", "other ctr", "supp", "placebo", "mixed", "supp or ecau", "GHE: group HIV education")
#target <- c("ecau", "etau", "tau")

# data$rob <- "NA"
data$instrument <- "xx"

# For analysis of ideation
data %>%
  #filter (mean_arm1 > 0) %>%
  select(study, 
         format, country, recruitment_setting, delivery_setting, target_group,
         condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, 
         n_arm1, n_arm2) %>% 
         
         #si_event_arm1,           
         #si_event_arm2, 	
         #si_n_arm1,    	
         #si_n_arm2) %>%
  # rename(event_arm1 = si_event_arm1,
  #        event_arm2 = si_event_arm2,
  #        totaln_arm1 = si_n_arm1,
  #        totaln_arm2 = si_n_arm2) %>%
  mutate(across(c(time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2),
                   #event_arm1, event_arm2, 
                  #totaln_arm1, totaln_arm2), 
                as.numeric)) -> ideation_cont
ideation_cont$outcome_type <- "msd"

data %>%
  #filter (si_n_arm1 > 0) %>%
  select(study, 
         condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2 , 
         event_arm1, event_arm2,
         format, country, recruitment_setting, delivery_setting, target_group,
         totaln_arm1,           totaln_arm2) %>%
#si_n_arm1,    	si_n_arm2) %>%
  # rename(event_arm1 = si_event_arm1,
  #        event_arm2 = si_event_arm2,
  #        totaln_arm1 = si_n_arm1,
  #        totaln_arm2 = si_n_arm2) %>%
  mutate(across(c(time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2),
                   #event_arm1, event_arm2, totaln_arm1, totaln_arm2), 
                as.numeric)) -> ideation_dich
ideation_dich$outcome_type <- "deterioration"

# Sign of g based on binary outcomes is flipped!

ideation_dich$event_arm1 <- ideation_dich$totaln_arm1 - ideation_dich$event_arm1
ideation_dich$event_arm2 <- ideation_dich$totaln_arm2 - ideation_dich$event_arm2

ideation_cont %>% bind_rows(ideation_dich) -> ideation


# for analysis of attempts

ideation$time <- "post"
ideation$rating <- "NA"
# check with MetaPsyTools
ideation <- checkDataFormat(ideation)
checkConflicts(ideation)
ideation <- calculateEffectSizes(ideation)


# check whether meta-analysis runs
resIdeation <- runMetaAnalysis(ideation, which.run = c("overall", "combined",
                                                       "lowest.highest", "outliers",
                                                       "influence", "threelevel", "threelevel.che"),
                               which.outliers = "combined",
                               which.influence = "combined",
                               which.rob = "threelevel.che",
                               vcov = "complex")


# inspect outcomes
resIdeation

plot(resIdeation, "overall", 
     col.predict = "red", 
     col.square = "purple",
     fontfamily = "Times New Roman",
     width = 10,
     height = 12)

correctPublicationBias(resIdeation, which.run = "overall")

funnel(resIdeation$model.overall, 
       studlab = TRUE, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("purple", "grey", "lightblue"))

sg_country <- subgroupAnalysis(resIdeation, country)
sg_format <- subgroupAnalysis(resIdeation, format)
sg_target_group <- subgroupAnalysis(resIdeation, target_group)
sg_recruitment_setting <- subgroupAnalysis(resIdeation, recruitment_setting)
sg_delivery_setting <- subgroupAnalysis(resIdeation, delivery_setting)

# eggers.test(ideation$model.overall)