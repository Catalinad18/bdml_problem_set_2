##############################################################
#       Big Data y Machine Learning                          #
#  Taller 2: Árboles, Bagging, Random Forests y  Boosting    #
##############################################################

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------
require(pacman)

# Clean environment
rm(list = ls())
#-------------------------------------------

p_load(tidymodels, 
       caret, 
       rpart, 
       modeldata, 
       spatialsample,
       tidyverse,
       rio,
       leaflet,
       randomForest,
       rattle, 
       spatialsample)

source("variablesJ.R")

# Load data ---------------------------------
test_data <- read_csv("base_datos_limpia_test.csv")
train_data <- read_csv("base_datos_limpia_train.csv")

#Append data
train_data <- train_data %>% mutate(type = "train")
test_data <- test_data %>% mutate(type = "test")

data <- rbind(train_data, test_data)

new_data <- sp_data %>%
  mutate(log_price=log(price)) %>%
  select(-c(price, city, month, year, title, description, piso_info,
            precio_por_mt2, area, property_type_2,
            geometry, operation_type, parqueadero, balcon, terraza)) %>%
  mutate(across(
    .cols = starts_with('dist'),
    .fns = list(sq = function(x) {x^2},
                cube = function(x) {x^3}),
    .names = "{.col}_{.fn}")) %>%
  mutate(across(c(surface_covered, bedrooms, bathrooms),
                .fns = list(sq = function(x) {x^2},
                            cube = function(x) {x^3}),
                .names = "{.col}_{.fn}"))

new_train<- new_data %>%
  filter(type=="train") %>%
  select(-c(type, property_id)) %>%
  bind_cols(dense_dtm_train)

new_test<- new_data %>%
  filter(type=="test") %>%
  select(-c(type, property_id)) %>%
  bind_cols(dense_dtm_test)

# Folds -------------------------------------------------------------------

#Para la parametrizacion necesitamos folds espaciales.

spatial_folds_trees <- group_vfold_cv(new_train, group = "NOMBRE")

#RECETA PARA LOS MODELOS --------------------------------------------------

#Recipe

rec_trees_forests_boost <-
  recipe(log_price ~ ., data = new_train) %>% 
  step_rm(NOMBRE) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())


# CART --------------------------------------------------------------------

#Model
tree <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode("regression")

tune_grid_tree <- grid_random(
  tree_depth(range = c(1, 10)),
  min_n(range = c(1, 20)),
  size = 100
)

#Workflow

workflow_tree <- workflow() %>%
  add_recipe(rec_tree) %>%
  add_model(tree)

#Tune grid

tune_tree <- workflow_tree %>% 
  tune_grid(resamples = spatial_folds_trees,
            grid = tune_grid_tree,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

results_tuning_tree <- tune_tree %>%
  collect_metrics()

tune_tree %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best Fit, Training and K-validation

df_fold <- vfold_cv(train, v = 3)

best_tree <- select_best(tune_tree, metric = "mae")

#Predict

tree_final <- finalize_workflow(workflow_tree, best_tree)

tree_final_fit <- fit(tree_final, data = new_test)

## Exportar

write_csv(tree_final_fit, "modeloArbol.csv")

# RANDOM FORESTS--------------------------------------------------------------

#Model
random_forest <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

random_forest_grid <- grid_random(  mtry(range = c(2, 4)),
                                min_n(range = c(1, 10)),
                                trees(range = c(100, 300)), size = 4)

#Workflow

workflow_random_forest <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(random_forest)

#Tune grid

tune_random_forest <- workflow_random_forest %>% 
  tune_grid(resamples = spatial_folds_trees,
            grid = random_forest_tree,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

results_tuning_random_forest <- tune_random_forest %>%
  collect_metrics()

tune_random_forest %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best Fit, Training and K-validation

df_fold <- vfold_cv(train, v = 3)

best_random_forest <- select_best(tune_random_forest, metric = "mae")

#Predict

random_forest_final <- finalize_workflow(workflow_random_forest, best_random_forest)

random_forest_final_fit <- fit(random_forest_final, data = new_test)

## Exportar

write_csv(random_forest_final_fit, "submissions/modeloBosqueAleatorio.csv")

# BOOSTING  --------------------------------------------------------------------

#Model

boost <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  # Cambiar a modo de regresióntree <- decision_tree(

tune_grid_boost <- grid_random(
  trees(range = c(400, 600)),
  min_n(range = c(1, 3)),
  learn_rate(range = c(0.001, 0.01)), size = 150
)

#Workflow

workflow_boost <- workflow() %>%
  add_recipe(rec_trees_forests_boost) %>%
  add_model(boost)

#Tune grid

tune_boost <- workflow_tree %>% 
  tune_grid(resamples = spatial_folds_trees,
            grid = tune_grid_boost,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

results_tuning_boost <- tune_boost %>%
  collect_metrics()

tune_boost %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best Fit, Training and K-validation

df_fold <- vfold_cv(train, v = 3)

best_boost <- select_best(tune_boost, metric = "mae")

#Predict

boost_final <- finalize_workflow(workflow_boost, best_boost)

boost_final_fit <- fit(boost_final, data = new_test)

## Exportar

write_csv(boost_final_fit, "modeloAumentando.csv")