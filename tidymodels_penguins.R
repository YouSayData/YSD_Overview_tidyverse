library(tidymodels)
library(palmerpenguins)
library(ranger)
library(randomForest)

penguins_sml <- penguins %>%
  select(3:6, species) %>%
  drop_na

penguins_sml %>% ggplot() +
  geom_point(aes(bill_length_mm, flipper_length_mm, col = species))


# Preprocess --------------------------------------------------------------

# Sampling ----------------------------------------------------------------

penguins_split <- initial_split(penguins_sml, prop = .6)
penguins_split

penguins_split %>%
  training %>%
  glimpse

penguins_split %>%
  testing %>%
  glimpse

penguins_split %>%
  testing %>%
  class


# Pre-Process interface ---------------------------------------------------

# recipe() takes the models formula and starts a set of transformations
# transformations in tidymodels start with step_ . There quite a few.
# prep() executes them

penguins_recipe <- penguins_split %>%
  training %>%
  # start recipe for model
  recipe(species ~.) %>%
  # removes variables that have large correlations with other variables
  step_corr(all_predictors()) %>% 
  # numeric data is centered to a mean of 0
  step_center(all_predictors(), -all_outcomes()) %>%
  # numeric data is scaled to a sd of 1
  step_scale(all_predictors(), -all_outcomes()) %>%
  # execute the transformations
  prep


# transform the test data -------------------------------------------------

# we bake the testing data with the recipe derived from the training data
penguins_testing <- penguins_recipe %>%
  bake(testing(penguins_split)) 

penguins_testing

# for the training data we don't need an extra preprocessing step,
# because it has already been done during the recipe creation
# we only need to extract aka juice it

penguins_training <- juice(penguins_recipe)
penguins_training


# Model training ----------------------------------------------------------

# before tidymodels one needed to know all kind of parameters for all kind of libraries
# e.g. ranger and randomforest do similar things, but have differently named parameters
# tidymodels give easy access to that

penguins_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(species ~ ., data = penguins_training)

penguins_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(species ~ ., data = penguins_training)


# Predictions -------------------------------------------------------------

predict(penguins_ranger, penguins_testing)

penguins_ranger %>%
  predict(penguins_testing) %>%
  bind_cols(penguins_testing)


# Validations -------------------------------------------------------------

penguins_ranger %>%
  predict(penguins_testing) %>%
  bind_cols(penguins_testing) %>%
  metrics(truth = species, estimate = .pred_class)

penguins_rf %>%
  predict(penguins_testing) %>%
  bind_cols(penguins_testing) %>%
  metrics(truth = species, estimate = .pred_class)


# Probabilities -----------------------------------------------------------

penguins_ranger %>%
  predict(penguins_testing, type = "prob")

penguis_probs <- penguins_ranger %>%
  predict(penguins_testing, type = "prob") %>%
  bind_cols(penguins_testing)

penguis_probs %>%
  roc_curve(species, .pred_Adelie:.pred_Gentoo) %>%
  autoplot()


# smote -------------------------------------------------------------------

penguins_sml %>% 
  count(species)

library(themis)

penguins_recipe_smote <- penguins_split %>%
  training %>%
  # start recipe for model
  recipe(species ~.) %>%
  # oversample minority classes 
  step_smote(species, skip = T) %>%
  # removes variables that have large correlations with other variables
  step_corr(all_predictors()) %>% 
  # numeric data is centered to a mean of 0
  step_center(all_predictors(), -all_outcomes()) %>%
  # numeric data is scaled to a sd of 1
  step_scale(all_predictors(), -all_outcomes()) %>%
  # execute the transformations
  prep

# transform the test data -------------------------------------------------

# we bake the testing data with the recipe derived from the training data
penguins_testing <- penguins_recipe_smote %>%
  bake(testing(penguins_split)) 

penguins_testing %>% count(species)

# for the training data we don't need an extra preprocessing step,
# because it has already been done during the recipe creation
# we only need to extract aka juice it

penguins_training <- juice(penguins_recipe_smote)
penguins_training %>% count(species)


# Model training ----------------------------------------------------------

penguins_rf_smote <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(species ~ ., data = penguins_training)

penguins_rf_smote %>%
  predict(penguins_testing) %>%
  bind_cols(penguins_testing) %>%
  metrics(truth = species, estimate = .pred_class)

penguins_rf %>%
  predict(penguins_testing) %>%
  bind_cols(penguins_testing) %>%
  metrics(truth = species, estimate = .pred_class)


# mlp ---------------------------------------------------------------------

library(keras)

penguins_mlp_fit <-
  mlp(epochs = 150L, hidden_units = 20L, dropout = 0.1) %>%
  set_mode("classification") %>% 
  set_engine("keras") %>%
  fit(species ~ ., data = penguins_training)

penguins_mlp_result <- penguins_testing %>%
  select(species) %>%
  bind_cols(
    predict(penguins_mlp_fit, new_data = penguins_testing),
    predict(penguins_mlp_fit, new_data = penguins_testing, type = "prob")
  )
penguins_mlp_result

penguins_mlp_fit %>%
  predict(penguins_testing) %>%
  bind_cols(penguins_testing) %>%
  metrics(truth = species, estimate = .pred_class)


# xg boost ----------------------------------------------------------------

library(xgboost)

xg_spec <- boost_tree(tree_depth = 10, trees = 100) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

penguins_xg_fit <- xg_spec %>% 
  fit(species ~ ., data = penguins_training)
penguins_xg_fit

penguins_xg_fit %>%
  predict(penguins_testing) %>%
  bind_cols(penguins_testing) %>%
  metrics(truth = species, estimate = .pred_class)

