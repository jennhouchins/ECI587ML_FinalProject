# ASSIGNMENT DESCRIPTION #####################################
# File:         wrangleData.R
# Project:      Final Project 
# Author:       Jennifer Houchins
#
# Purpose:      Wrangle the data for final projects
#


# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,readr, dplyr, tidyr, stringr, RWeka, tidytext)

eoc_data <- read_csv("data/eoc_survey_cleancolnames.csv") %>% 
  select(response,
         EffectivenessRating,
         IntroDiscussionRating,
         CourseContentRating,
         PuttingItTogetherRating,
         ActionPlanRating,
         ResourcesRating,
         MostValuableAspects,
         ImprovementEstablishingNorms,
         ImprovementBringingSEL,
         ImprovementMaintainingConnection,
         ImprovementSelectDigResources,
         ImprovementSupportSpecialPop,
         ImprovementProvidingFeedback,
         PositiveChangesEffectiveness,
         HasAttemptedChanges,
         AnticipatedApplicationPractice,
         ChangesToPractice,
         CourseRecommendations,
         DesiredActivityCompletion,
         CourseHoursEstimate) %>%
  mutate(HasAttemptedChanges = as.numeric(gsub(" .*$","", HasAttemptedChanges))) %>% 
  mutate(CourseHoursEstimate = as.numeric(sub(" .*$", "", CourseHoursEstimate))) %>%
  mutate(MostValuableAspects = replace_na(MostValuableAspects, ""),
         ChangesToPractice = replace_na(ChangesToPractice, ""),
         CourseRecommendations = replace_na(CourseRecommendations, ""),
         AnticipatedApplicationPractice = replace_na(AnticipatedApplicationPractice, ""),
         CourseHoursEstimate = replace_na(CourseHoursEstimate, 0)) %>% 
  mutate(class = ifelse(EffectivenessRating == 5, "veryineffective",
                                      ifelse(EffectivenessRating == 4, "ineffective",
                                             ifelse(EffectivenessRating == 3, "neither",
                                                    ifelse(EffectivenessRating == 2, "effective", "veryeffective"))))) %>%

  mutate(text = paste0(MostValuableAspects, ChangesToPractice, CourseRecommendations)) %>% 
  drop_na() %>% 
  subset(text != "")


head(eoc_data,1)
glimpse(eoc_data)

write.csv(eoc_data, "data/eoc_surveydata_wrangled.csv")

eoc_data_singletext <- eoc_data %>% 
  select(response,
         EffectivenessRating,
         IntroDiscussionRating,
         CourseContentRating,
         PuttingItTogetherRating,
         ActionPlanRating,
         ResourcesRating,
         ImprovementEstablishingNorms,
         ImprovementBringingSEL,
         ImprovementMaintainingConnection,
         ImprovementSelectDigResources,
         ImprovementSupportSpecialPop,
         ImprovementProvidingFeedback,
         PositiveChangesEffectiveness,
         HasAttemptedChanges,
         DesiredActivityCompletion,
         CourseHoursEstimate,
         class,
         text)

set.seed(42)

rows <- sample(nrow(eoc_data_singletext))
responses <- eoc_data_singletext[rows,]

write_csv(responses, "data/eoc_surveydata_randomized.csv")

cutoff1 <- ceiling(nrow(responses)*.20)
cutoff2 <- ceiling(cutoff1 + nrow(responses)*.70)

responses_dev <- responses[1:cutoff1,]
responses_crossval <- responses[(cutoff1+1):cutoff2,]
responses_test <- responses[(cutoff2+1):nrow(responses),]

write_csv(responses_dev, "data/eoc_surveydata_dev.csv")
write_csv(responses_crossval, "data/eoc_surveydata_crossval.csv")
write_csv(responses_test, "data/eoc_surveydata_test.csv")

# for assignment 4 (kmeans and clustering)

# responses_weka <- responses_dev %>%
#   select(response,
#          IntroDiscussionRating,
#          CourseContentRating,
#          PuttingItTogetherRating,
#          ActionPlanRating,
#          ResourcesRating,
#          ImprovementEstablishingNorms,
#          ImprovementBringingSEL,
#          ImprovementMaintainingConnection,
#          ImprovementSelectDigResources,
#          ImprovementSupportSpecialPop,
#          ImprovementProvidingFeedback,
#          PositiveChangesEffectiveness,
#          HasAttemptedChanges,
#          DesiredActivityCompletion,
#          CourseHoursEstimate,
#          class)
# 
# write.arff(responses_weka, "data/eoc_survey_data.arff")

