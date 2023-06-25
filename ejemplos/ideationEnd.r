#MA ideation script list of contents#
#1. Ensure Basic structure from original code and correct data formatting#
library(tidyverse)
library(metapsyTools)
library(readxl)
data <- read_excel("metapsydate.xlsx")

# choose proper control groups
table(data$condition_arm2, useNA = "always")
target <- c("ecau", "etau", "tau")
data %>% filter(condition_arm2 %in% target) -> data

data$time <- "post"
data$rating <- "NA"
# data$rob <- "NA"
data$instrument <- "xx"

# For analysis of ideation
data %>%
  filter (mean_arm1 > 0) %>%
  select(study, direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2, 
         time, rating,
         event_arm1, event_arm2, 	totaln_arm1, totaln_arm2) %>%
  # rename(event_arm1 = si_event_arm1,
  #        event_arm2 = si_event_arm2,
  #        totaln_arm1 = si_n_arm1,
  #        totaln_arm2 = si_n_arm2) %>%
  mutate(across(c(time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2,
                   event_arm1, event_arm2, totaln_arm1, totaln_arm2), as.numeric)) -> ideation_cont
ideation_cont$outcome_type <- "msd"

data %>%
  filter (totaln_arm1 > 0) %>%
  select(study, direct, condition_arm1, condition_arm2, multi_arm1, multi_arm2, instrument,
         time_weeks, mean_arm1, mean_arm2, sd_arm1, sd_arm2, n_arm1, n_arm2,
         time, rating,
         event_arm1, event_arm2, totaln_arm1, totaln_arm2) %>%
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

# for analysis of attempts
data %>%
  filter(event_arm1 >= 0) %>%
  select(study, direct, condition_arm1, condition_arm2, 
         multi_arm1, multi_arm2, 
         time, rating,
         instrument, time_weeks, event_arm1,
         event_arm2, totaln_arm1, totaln_arm2) -> attempts
attempts$outcome_type <- "RR"

# check with MetaPsyTools
ideation <- checkDataFormat(ideation)
checkConflicts(ideation)
ideation <- calculateEffectSizes(ideation)

attempts <- checkDataFormat(attempts)
attempts <- calculateEffectSizes(attempts)

#2. Metanaalyses: res, ideation, attempts

#MA RES (do we need to add "es.measure =msd" here for res?)
res <- runMetaAnalysis(metapsydate, 
                       which.run = c("overall", "combined", "lowest.highest", "outliers",
                                     "influence", "threelevel", "threelevel.che"),
                       which.outliers = "combined",
                       which.influence = "combined",
                       which.rob = "threelevel.che",
                       vcov = "complex")

#MA ideation (do we need to add "es.measure =msd" here for ideation?)
resIdeation <- runMetaAnalysis(ideation, which.run = c("overall", "combined",
                                                       "lowest.highest", "outliers",
                                                       "influence", "threelevel", "threelevel.che"),
                               which.outliers = "combined",
                               which.influence = "combined",
                               which.rob = "threelevel.che",
                               vcov = "complex")
#MA attempts
resAttempt <- runMetaAnalysis(attempts, es.measure = "RR", which.run = c("overall", "combined",
                                                                         "lowest.highest", "outliers",
                                                                         "influence", "threelevel", "threelevel.che"),
                              which.outliers = "combined",
                              which.influence = "combined",
                              which.rob = "threelevel.che",
                              vcov = "complex")

#3. Plots and table output of MA res,ideation and attempts. 

# inspect outcomes (get output of it in tables)
res
resIdeation
resAttempt
# make forest plots
#foresplot overall ma ideation
plot(res)
plot(ideation, "overall", 
     col.predict = "red", 
     col.square = "purple",
     fontfamily = "Times New Roman",
     width = 10,
     height = 12)
plot(resAttempt)

#4. Egger's test + corrected publication bias + funnel plot after corrected 
#egger's test (get output of it)
library(dmetar)
eggers.test(ideation$model.overall)
#corrected publication bias (get output of it)
correctPublicationBias(ideation, which.run = "overall")
#funnel plot (adjusting plot to right sizes, and with display of studies ID)
library(meta)
funnel(ideation$model.overall, 
       studlab = TRUE, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("purple", "grey", "lightblue"))

#5. Subgroup analyses using the following variables: 
#number of sessions, 
#country (EU, U.S., U.K., AU, EAS, Others)
#format (Face to Face, Individual ),
#therapy type (condition_arm1)=(CBT, DBT, DYN, CAMS, ASSIP, ASSI, IMG, FAM, MBCT,IPT, PST, others)
#target group (Adults, Military, Students, Veterans, and Old adults)
#recruitment setting (Clinical, Community, Other)
#delivery setting= (Inpatient vs Outpatient)
#Number of sessions = (mayor que 6, menor o igual que 6)
#Example structure of MA with subgroup analyses used with names as example: 
non_response %>%
  filter(direct == "yes") -> non_response_direct

non_response %>%
  filter(direct == "no") -> non_response_indirect

res_non_response_direct <- runMetaAnalysis(non_response_direct, es.measure = "RR", which.run = c("overall", "combined",
                                                                                                 "lowest.highest", "outliers",
                                                                                                 "influence", "threelevel", "threelevel.che"),
                                           which.outliers = "combined",
                                           which.influence = "combined",
                                           which.rob = "threelevel.che",
                                           vcov = "complex")

# inspect outcomes of each subgroup(table per subgroup)
country
format
targetgroup
recruitmentsetting
deliverysetting

numberofsessions
therapytype
#End MA ideation list of contents#
