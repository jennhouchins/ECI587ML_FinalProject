# ASSIGNMENT DESCRIPTION #####################################
# File:         ML_FinalProject_multiclass.R
# Project:      Assignment 5
# Author:       Jennifer Houchins
#
# Purpose:      This project nugget provide the first update
#               for my final project for the ECI 587 Machine Learning 
#               course


# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
# RTextTools is a supervised learning package for text classification and 
# the e1071 package provides SVM in r
pacman::p_load(pacman,readr, dplyr, tidyr, stringr, RWeka, tidytext, rstudioapi,
               RTextTools, e1071)

# the following chunk deals with setting the working directory and creating
setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file location
datapath <- file.path(getwd(), "data")
if (!file.exists(datapath)){
  dir.create(datapath)
  print("The data sub directory has been created in the working directory.")
}
# 1 GET THE WRANGLED DEV DATA ######################

dev_data <- read.csv(file.path(datapath,"eoc_surveydata_dev.csv"))

test_data <- read.csv(file.path(datapath, "eoc_surveydata_test.csv"))

# from Dr. Jiang's example
eoc_dtm_dev <- create_matrix(dev_data[,"text"], language="english",
                         removeStopwords=FALSE, removeNumbers=TRUE,
                         stemWords=FALSE)

eoc_dtm_test <- create_matrix(test_data[,"text"], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 

# consider tidytext instead; would look something like...

# eoc_tidy_dtm <- dev_data %>% 
#   unnest_tokens(output = word, input = text) %>% 
#   anti_join(stopwords, by="word") %>% 
#   count(response, word) %>% 
#   cast_dtm(response, word, n)

eoc_dtm_dev_matrix <- as.matrix(eoc_dtm_dev)
eoc_dtm_test_matrix <- as.matrix(eoc_dtm_test)

eoc_numeric_devdata <- dev_data %>%
  select(IntroDiscussionRating,
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
         CourseHoursEstimate)

eoc_numeric_testdata <- test_data %>%
  select(IntroDiscussionRating,
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
         CourseHoursEstimate)

devmat <- cbind(eoc_dtm_dev_matrix, eoc_numeric_devdata)
testmat <- cbind(eoc_dtm_test_matrix, eoc_numeric_testdata)

# train a default naiveBayes model using the created dev data matrix

classifier <- naiveBayes(devmat, as.factor(dev_data$class))

# test the validity
predicted <- predict(classifier, testmat)
predicted
table(test_data$class, predicted, dnn=c("Actual", "Prediction"))
recall_accuracy(test_data$class, predicted)
