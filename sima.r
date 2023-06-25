#Script for ensuring MA running of continuous outcome
#basic#
#setwd("C:/Users/pgpau/Desktop/r studio thesis/metapsydata.xlsx"/")

#call the data#
library(readxl)
library(tidyverse)
library(metapsyTools)


metapsydate <- read_excel("metapsydate.xlsx")
data <- read_excel("metapsydate.xlsx")
#proper control groups#


target <- c("tau", "etau", "other")
data %>% filter(condition_arm2 %in% target) -> data
table(data$condition_arm2, useNA = "always")


# data$time <- "post"
# data$rating <- "NA"
# data$rob <- "NA"
data$instrument <- "xx"

#run ma ensuring#
library(metapsyTools)
res <- runMetaAnalysis(metapsydate, 
                         which.run = c("overall", "combined", "lowest.highest", "outliers",
                                                                  "influence", "threelevel", "threelevel.che"),
                         which.outliers = "combined",
                         which.influence = "combined",
                         which.rob = "threelevel.che",
                         vcov = "complex")
summary(res)

#plot-inspect outcomes
res

#foresplot overall ma
plot(res, "overall", 
     col.predict = "red", 
     col.square = "purple",
     fontfamily = "Times New Roman",
     width = 10,
     height = 12)
#funnel plot 
library(meta)
funnel(res$model.overall, 
       studlab = TRUE, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("purple", "grey", "lightblue"))
# Egger's regression test applied to the "overall" model:
library(dmetar)
eggers.test(res$model.overall)

#run ma ensuring#
library(meta)
res <- runMetaAnalysis(metapsydate,
                       
                       which.run = c("overall", "combined", "lowest.highest", "outliers",
                                     "influence", "threelevel", "threelevel.che"),
                       which.outliers = "combined",
                       which.influence = "combined",
                       which.rob = "threelevel.che",
                       vcov = "complex")

#forestplot#


correctPublicationBias(res, which.run = "overall")
sg <- subgroupAnalysis(res, format)
#plot each sg
plot(sg)

plot(res, "overall")        # Overall model (ES assumed independent)
plot(res, "combined")       # ES combined within studies before pooling
plot(res, "lowest.highest") # Lowest and highest ES removed (creates 2 plots)
plot(res, "outliers")       # Outliers-removed model
plot(res, "influence")      # Influential cases-removed model
plot(res, "threelevel")     # Three-level model
plot(res, "threelevel.che") # Three-level CHE model                    


# For analysis of ideation
data %>%
  filter(mean_arm1 > 0) %>%
  mutate(si_event_arm1 = event_arm1,
         si_event_arm2 = event_arm2,
         si_n_arm1 = totaln_arm1,
         si_n_arm2 = totaln_arm2) %>%
  select(study,time, rating,direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2, si_event_arm1,
         si_event_arm2, si_n_arm1, si_n_arm2,event_arm1,event_arm2,totaln_arm1,totaln_arm2) %>%
  mutate(across(c(time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2,
                  event_arm1, event_arm2, totaln_arm1, totaln_arm2), as.numeric)) -> ideation_cont
ideation_cont$outcome_type <- "msd"

data %>%
  mutate(si_event_arm1 = event_arm1,
         si_event_arm2 = event_arm2,
         si_n_arm1 = totaln_arm1,
         si_n_arm2 = totaln_arm2) %>%
  filter(si_n_arm1 > 0) %>%
  select(study, time, rating, direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2, si_event_arm1,
         si_event_arm2, si_n_arm1, si_n_arm2,event_arm1,event_arm2,totaln_arm1,totaln_arm2) %>%
  mutate(across(c(time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2,
                  event_arm1, event_arm2, totaln_arm1, totaln_arm2), as.numeric)) -> ideation_dich
ideation_dich$outcome_type <- "deterioration"

# Sign of g based on binary outcomes is flipped!
ideation_dich$event_arm1 <- ideation_dich$totaln_arm1 - ideation_dich$event_arm1
ideation_dich$event_arm2 <- ideation_dich$totaln_arm2 - ideation_dich$event_arm2

ideation <- rbind(ideation_cont, ideation_dich)


# for analysis of attempts
data %>%
  filter(event_arm1 >= 0) %>%
  select(rating, study, n_arm1, mean_arm1, sd_arm1, n_arm2, mean_arm2, sd_arm2,
         direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2,
         instrument, time_weeks, event_arm1, event_arm2, totaln_arm1, totaln_arm2, time) -> attempts


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


resAttempt <- runMetaAnalysis(attempts, es.measure = "RR", 
                              which.run = c("overall", "combined", "lowest.highest", "outliers",
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
