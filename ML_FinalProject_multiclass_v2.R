# ASSIGNMENT DESCRIPTION #####################################
# File:         ML_FinalProject_multiclass_v2.R
# Project:      Assignment 6
# Author:       Jennifer Houchins
#
# Purpose:      This project nugget provide the second update
#               for my final project for the ECI 587 Machine Learning 
#               course. It includes migrating my modeling from the e1071
#               package to the tidymodels package


# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, readr, dplyr, tidyr, stringr, tidyverse, 
               tidytext, rstudioapi, tm, tidymodels, ggplot2, stringi,
               recipes, textrecipes, modeldata, hardhat, discrim,
               themis, BBmisc, vip, dials, textfeatures)

# the following chunk deals with setting the working directory and creating
setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file location
datapath <- file.path(getwd(), "data")
if (!file.exists(datapath)){
  dir.create(datapath)
  print("The data sub directory has been created in the working directory.")
}
# 1 GET THE WRANGLED DEV DATA ######################

eoc_surveydata <- read.csv(file.path(datapath,"eoc_surveydata_preprocessed.csv"),
                           row.names = 1) %>% 
  mutate(class = as.factor(class)) %>% 
  mutate(text = gsub('&nbsp;',' ', text)) %>% 
  mutate(IntroDiscussionRating = normalize(IntroDiscussionRating, method = "range",
                                           range = c(0,1))) %>%
  mutate(CourseContentRating = normalize(CourseContentRating, method = "range",
                                           range = c(0,1))) %>%
  mutate(PuttingItTogetherRating = normalize(PuttingItTogetherRating, method = "range",
                                           range = c(0,1))) %>%
  mutate(ActionPlanRating = normalize(ActionPlanRating, method = "range",
                                           range = c(0,1))) %>%
  mutate(ResourcesRating = normalize(ResourcesRating, method = "range",
                                           range = c(0,1))) %>%
  mutate(ImprovementEstablishingNorms = normalize(ImprovementEstablishingNorms, method = "range",
                                     range = c(0,1))) %>%
  mutate(ImprovementBringingSEL = normalize(ImprovementBringingSEL, method = "range",
                                     range = c(0,1))) %>%
  mutate(ImprovementMaintainingConnection = normalize(ImprovementMaintainingConnection, method = "range",
                                     range = c(0,1))) %>%
  mutate(ImprovementSelectDigResources = normalize(ImprovementSelectDigResources, method = "range",
                                     range = c(0,1))) %>%
  mutate(ImprovementSupportSpecialPop = normalize(ImprovementSupportSpecialPop, method = "range",
                                     range = c(0,1))) %>%
  mutate(ImprovementProvidingFeedback = normalize(ImprovementProvidingFeedback, method = "range",
                                                  range = c(0,1))) %>%
  mutate(PositiveChangesEffectiveness = normalize(PositiveChangesEffectiveness, method = "range",
                                                  range = c(0,1))) %>%
  mutate(HasAttemptedChanges = normalize(HasAttemptedChanges, method = "range",
                                                  range = c(0,1))) %>%
  mutate(DesiredActivityCompletion = normalize(DesiredActivityCompletion, method = "range",
                                                  range = c(0,1))) %>%
  mutate(CourseHoursEstimate = normalize(CourseHoursEstimate, method = "range",
                                                  range = c(0,1)))


# 2 PREPARE FOR MODELING ###########################

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(587)

# Put 3/4 of the data into the training set 
data_split <- initial_split(eoc_surveydata, prop = 3/4)

# Create data frames for the two sets:
eoc_train <- training(data_split)
eoc_test  <- testing(data_split)

# Check to see if the classifications are distributed in similar percentages
# RESULTS are that both training and test data sets have label distributions of
# ~97% effective, ~.5% ineffective, and ~2% neither
table(eoc_train$class)
table(eoc_test$class)

# duplicates for naive bayes

eoc_train_nb <- eoc_train %>% 
  mutate(class = factor(if_else(class=="effective", "effective", "ineffective")))

eoc_test_nb <- eoc_test %>% 
  mutate(class = factor(if_else(class=="effective", "effective", "ineffective")))

table(eoc_train_nb$class)
table(eoc_test_nb$class)

# 4 TRY LASSO MULTICLASSIFICATION #####################

# To move forward use Julia Silge's tutorial post here
# https://juliasilge.com/blog/tidy-text-classification/

survey_recipe <- recipe(class ~ response + IntroDiscussionRating + 
                          ActionPlanRating + 
                          PositiveChangesEffectiveness +
                          text, data = eoc_train) %>% 
  update_role(response, new_role = "ID") %>% 
  step_tokenize(text) %>%
  step_ngram(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>% 
  step_tfidf(text) %>%
  step_downsample(class)

# survey_prep <- prep(survey_recipe)

survey_wf <- workflow() %>% 
  add_recipe(survey_recipe)

survey_folds <- vfold_cv(eoc_train)

multi_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

multi_spec

##sparse bp
sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

multi_lasso_wf <- workflow() %>%
  add_recipe(survey_recipe, blueprint = sparse_bp) %>%
  add_model(multi_spec)

multi_lasso_wf

multi_lasso_rs <- tune_grid(
  multi_lasso_wf,
  survey_folds,
  grid = 10,
  control = control_resamples(save_pred = TRUE),
  metrics = metric_set(accuracy, sensitivity, specificity)
)

multi_lasso_rs

best_acc <- multi_lasso_rs %>%
  show_best("accuracy")
best_acc
# confusion matrix
multi_lasso_rs %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  filter(id == "Fold09") %>%
  conf_mat(class, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20))

multi_lasso_fit <- multi_lasso_wf %>% 
  fit(data = eoc_train)

multi_lasso_fit

multi_lasso_fit %>%
  pull_workflow_fit() %>% 
  vip(num_features = 50, geom = "point")

autoplot(multi_lasso_rs) +
  labs(
    color = "Number of tokens",
    title = "Model performance across regularization penalties and tokens",
    subtitle = paste("We can choose a simpler model with higher regularization")
  )

choose_acc <- multi_lasso_rs %>%
  select_by_pct_loss(metric = "accuracy", -penalty)

choose_acc

final_wf <- finalize_workflow(multi_lasso_wf, choose_acc)
final_wf

final_fitted <- last_fit(final_wf, data_split)

collect_metrics(final_fitted)

collect_predictions(final_fitted) %>%
  conf_mat(truth = class, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
