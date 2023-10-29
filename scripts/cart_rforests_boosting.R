##############################################################
#       Big Data y Machine Learning                          #
#  Taller 2: Árboles, Bagging, Random Forests y  Boosting    #
##############################################################

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------

# Clean environment
rm(list = ls())
#-------------------------------------------
require(pacman)
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
       spatialsample, 
       doParallel, 
       ranger)


# Datos -------------------------------------------------------------------

sp_data <- readRDS("stores/Bases Finales/sp_data_final.rds")
dense_dtm_train <- readRDS("stores/Bases Finales/DTM_train.rds")
dense_dtm_test <- readRDS("stores/Bases Finales/DTM_test.rds")


new_data <- sp_data %>%
  mutate(log_price=log(price)) %>%
  select(-c(price, city, month, year, title, description, piso_info, area, 
            precio_por_mt2, area, property_type_2,
            geometry, operation_type)) %>%
  mutate(across(
    .cols = starts_with('dist'),
    .fns = list(sq = function(x) {x^2},
                cube = function(x) {x^3}),
    .names = "{.col}_{.fn}")) %>%
  mutate(across(c(surface_covered, bedrooms, bathrooms),
                .fns = list(sq = function(x) {x^2},
                            cube = function(x) {x^3}),
                .names = "{.col}_{.fn}")) %>%
  mutate(ESTRATO=as.character(ESTRATO))

new_train<- new_data %>%
  filter(type=="train") %>%
  select(-c(type, property_id)) %>%
  bind_cols(dense_dtm_train)

new_test<- new_data %>%
  filter(type=="test") %>%
  select(-c(type, property_id)) %>%
  bind_cols(dense_dtm_test)

#Coefs Lasso para seleccionar variables
coefs_lasso <- read_csv("stores/Bases Finales/coefs_lasso.csv")
selected_vars <- coefs_lasso$term[coefs_lasso$estimate==0]

# Folds -------------------------------------------------------------------

set.seed(123)
block_folds <- spatial_block_cv(new_train, v = 5)
autoplot(block_folds)

#RECETA PARA LOS MODELOS --------------------------------------------------

#Recipe

#Con todas las variables
rec_trees_forests_boost <-
  recipe(log_price ~ ., data = select(as_tibble(new_train), -geometry)) %>% 
  step_dummy(ESTRATO) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Quitando lasso
rec_trees_forests_boost_select <-
  recipe(log_price ~ ., data = select(as_tibble(new_train), -geometry)) %>% 
  step_dummy(ESTRATO) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes()) %>%
  step_rm(all_of(selected_vars))

# CART --------------------------------------------------------------------

#Model
tree <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode("regression")

tune_grid_tree <- grid_regular(
  tree_depth(),
  min_n(),
  levels = c(10, 5)
)

#Workflows

workflow_tree <- workflow() %>%
  add_recipe(rec_trees_forests_boost) %>%
  add_model(tree)

workflow_tree_lasso <- workflow() %>%
  add_recipe(rec_trees_forests_boost_select) %>%
  add_model(tree)

#Tune grid

tune_tree <- workflow_tree %>% 
  tune_grid(resamples = block_folds,
            grid = tune_grid_tree,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

tune_tree_lasso <- workflow_tree_lasso %>% 
  tune_grid(resamples = block_folds,
            grid = tune_grid_tree,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))


#Best Fit, Training and K-validation
best_tree <- select_best(tune_tree, metric = "mae")
best_tree_lasso <- select_best(tune_tree_lasso, metric = "mae")

#Predict
tree_final <- finalize_workflow(workflow_tree, best_tree)
tree_final_fit <- fit(tree_final, data = new_train)

tree_lasso_final <- finalize_workflow(workflow_tree_lasso, best_tree_lasso)
tree_lasso_final_fit <- fit(tree_lasso_final, data = new_train)

tree_predictions <- predict(tree_final_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

tree_lasso_predictions <- predict(tree_lasso_final_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar

write_csv(tree_predictions, "submissions/tree.csv")
write_csv(tree_lasso_predictions, "submissions/tree_lasso.csv")

# RANDOM FORESTS--------------------------------------------------------------

set.seed(123)
#Model
random_forest <- rand_forest(
  mtry = tune(), #default para probar. Usualmente lleva al optimo
  min_n = tune(),
  trees = tune(),
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

random_forest_grid <- grid_regular(mtry(range = c(26, 500)),
                                   min_n(),
                                   trees(range = c(1000, 2000)),
                                   levels = c(10,5,5))

#Workflow
workflow_random_forest <- workflow() %>%
  add_recipe(rec_trees_forests_boost_select) %>%
  add_model(random_forest)

#Tune grid
registerDoParallel(cores = 8)

tune_random_forest <- workflow_random_forest %>% 
  tune_grid(resamples = block_folds,
            grid = random_forest_grid,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

stopImplicitCluster()

#tune_random_forest %>%
# collect_metrics() %>%
#ggplot(aes(penalty, mean, color = .metric)) +
#geom_line(size = 1.5) +
#scale_x_log10() +
#theme(legend.position = "none") +
#labs(title = "RMSE")

#Best Fit, Training and K-validation
best_random_forest <- select_best(tune_random_forest, metric = "mae")

#Predict
random_forest_final <- finalize_workflow(workflow_random_forest, best_random_forest)
random_forest_final_fit <- fit(random_forest_final, data = new_train)

random_forest_predictions <- predict(random_forest_final_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportars

write_csv(random_forest_predictions, "submissions/RandomForest.csv")

# BOOSTING  --------------------------------------------------------------------

#Model

boost <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")

tune_grid_boost <- grid_random(
  trees(),
  min_n(),
  learn_rate(), 
  levels = c(5,5,20)
)

#Workflow

workflow_boost <- workflow() %>%
  add_recipe(rec_trees_forests_boost_select) %>%
  add_model(boost)

#Tune grid
registerDoParallel(cores = 8)

tune_boost <- workflow_tree %>% 
  tune_grid(resamples = block_folds,
            grid = tune_grid_boost,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

stopImplicitCluster()

#Best Fit, Training 

best_boost <- select_best(tune_boost, metric = "mae")
boost_final <- finalize_workflow(workflow_boost, best_boost)
boost_final_fit <- fit(boost_final, data = new_train)

#Predict
boost_predictions <- predict(boost_final_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))


## Exportar

write_csv(boost_final_fit, "submissions/Boost_Tree.csv")