##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2: Lasso/Ridge/Elastic Net.                   #
##############################################################

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------
require(pacman)
p_load("tidymodels", "glmnet", "spatialsample")
#source("scripts/variables.R")

new_data <- sp_data %>%
  mutate(log_price=log(price)) %>% #Mejor log
  select(-c(price, city, month, year, title, description, area, property_type, precio_por_mt2, piso_info,
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

spatial_folds <- group_vfold_cv(new_train, group = "NOMBRE")

# Lasso -------------------------------------------------------------------

#Model
lasso <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lambda_grid_lasso <- grid_regular(penalty(), levels = 50) #Spatial cross validation

#Recipe
lasso_rec <-
  recipe(log_price ~ ., data = new_train) %>% 
  step_rm(NOMBRE) %>% #NOMBRE es la variable para el CV ahora, entonces no funcionan bien los efectos fijos. Vamos a ver. 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
lasso_wflow <- 
  workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso) 

#Tune grid

tune_result <- lasso_wflow %>% 
  tune_grid(resamples = spatial_folds,
            grid = lambda_grid_lasso,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

results_tuning_lasso <- tune_result %>%
  collect_metrics()

tune_result %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "mae")

best_lasso <- linear_reg(penalty = tune_best$penalty, mixture = 1) %>%
  set_engine("glmnet")

best_lasso_wflow <- 
  workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(best_lasso) 

best_lasso_fit <- 
  best_lasso_wflow %>% 
  fit(new_train)

## Es interesante ver las variables que quedaron seleccionadas
coefs_lasso <- best_lasso_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Predict
lasso_predictions <- predict(best_lasso_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar
write_csv(lasso_predictions, "submissions/03_Lasso.csv")


#### Revision: Los resultados de Kaggle fueron muy malos, overfitting. Removamos los cuadrados y cubos y estimemos las palabras importantes ####

new_data_corrected <- sp_data %>%
  mutate(log_price=log(price)) %>% #Mejor log
  select(-c(price, rooms, city, month, year, surface_total, title, description,
            geometry, operation_type, parqueadero, balcon, terraza))

new_train_corrected <- new_data_corrected %>%
  filter(type=="train") %>%
  select(-c(type, property_id)) %>%
  bind_cols(dense_dtm_train)

new_test_corrected<- new_data_corrected %>%
  filter(type=="test") %>%
  select(-c(type, property_id)) %>%
  bind_cols(dense_dtm_test)

spatial_folds_corrected <- group_vfold_cv(new_train_corrected, group = "NOMBRE")

#Model
lasso <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lambda_grid_lasso <- grid_regular(penalty(), levels = 50) #Spatial cross validation

#Recipe
lasso_rec <-
  recipe(log_price ~ ., data = new_train_corrected) %>% 
  step_rm(NOMBRE) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
lasso_wflow <- 
  workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso) 

#Tune grid
tune_result <- lasso_wflow %>% 
  tune_grid(resamples = spatial_folds_corrected,
            grid = lambda_grid_lasso,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

results_tuning_lasso <- tune_result %>%
  collect_metrics()

tune_result %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "mae")

best_lasso <- linear_reg(penalty = tune_best$penalty, mixture = 1) %>%
  set_engine("glmnet")

best_lasso_wflow <- 
  workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(best_lasso) 

best_lasso_fit <- 
  best_lasso_wflow %>% 
  fit(new_train_corrected)

## Es interesante ver las variables que quedaron seleccionadas
coefs_lasso_corrected <- best_lasso_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Predict
lasso_predictions_corrected <- predict(best_lasso_fit, new_test_corrected) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar
write_csv(lasso_predictions_corrected, "submissions/03_Lasso(V2).csv")



# Ridge -------------------------------------------------------------------

#Sólo para probar, usamos un tipo diferente de splits espaciales (k means)
set.seed(123)
block_folds <- spatial_block_cv(new_train, v = 15)
block_folds_corrected <- spatial_block_cv(new_train_corrected, v = 15)
autoplot(block_folds)

#Model
ridge <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

lambda_grid_ridge <- grid_regular(penalty(), levels = 50) #Spatial cross validation

#Recipe
ridge_rec <-
  recipe(log_price ~ ., data = select(as_tibble(new_train), -geometry)) %>% 
  #step_rm(NOMBRE) %>% #NOMBRE es la variable para el CV ahora, entonces no funcionan bien los efectos fijos. Vamos a ver. 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(ridge) 

#Tune grid

tune_result <- ridge_wflow %>% 
  tune_grid(resamples = block_folds,
            grid = lambda_grid_ridge,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

results_tuning_ridge <- tune_result %>%
  collect_metrics()

tune_result %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "mae")

best_ridge <- linear_reg(penalty = tune_best$penalty, mixture = 0) %>%
  set_engine("glmnet")

best_ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(best_ridge) 

best_ridge_fit <- 
  best_ridge_wflow %>% 
  fit(new_train)

## Es interesante ver las variables que quedaron seleccionadas
coefs_ridge <- best_ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Predict
ridge_predictions <- predict(best_ridge_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar
write_csv(ridge_predictions, "submissions/04_Ridge.csv")


#### Revision: Tal como con Lasso ####

#Model
ridge <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

lambda_grid_ridge <- grid_regular(penalty(), levels = 50) #Spatial cross validation

#Recipe
ridge_rec <-
  recipe(log_price ~ ., data = select(as_tibble(new_train_corrected), -geometry)) %>% 
  #step_rm(NOMBRE) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(ridge) 

#Tune grid
tune_result <- ridge_wflow %>% 
  tune_grid(resamples = block_folds_corrected,
            grid = lambda_grid_ridge,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

results_tuning_ridge <- tune_result %>%
  collect_metrics()

tune_result %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "mae")

best_ridge <- linear_reg(penalty = tune_best$penalty, mixture = 0) %>%
  set_engine("glmnet")

best_ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(best_ridge) 

best_ridge_fit <- 
  best_ridge_wflow %>% 
  fit(new_train_corrected)

## Es interesante ver las variables que quedaron seleccionadas
coefs_ridge_corrected <- best_ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Predict
ridge_predictions_corrected <- predict(best_ridge_fit, new_test_corrected) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar
write_csv(ridge_predictions_corrected, "submissions/04_Ridge(V2).csv")



# Elastic Net -------------------------------------------------------------

#Model
elasticnet <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

lambda_grid_elasticnet <- grid_regular(penalty(), mixture(), levels = 50) #Spatial cross validation

#Recipe
elasticnet_rec <-
  recipe(log_price ~ ., data = select(as_tibble(new_train), -geometry)) %>% 
  #step_rm(NOMBRE) %>% #NOMBRE es la variable para el CV ahora, entonces no funcionan bien los efectos fijos. Vamos a ver. 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
elasticnet_wflow <- 
  workflow() %>% 
  add_recipe(elasticnet_rec) %>% 
  add_model(elasticnet) 

#Tune grid

tune_result <- elasticnet_wflow %>% 
  tune_grid(resamples = block_folds,
            grid = lambda_grid_elasticnet,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE, parallel_over = "everything"))

results_tuning_elasticnet <- tune_result %>%
  collect_metrics()

tune_result %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "MAE")

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "mae")

best_elasticnet <- linear_reg(penalty = tune_best$penalty, mixture = tune_best$mixture) %>%
  set_engine("glmnet")

best_elasticnet_wflow <- 
  workflow() %>% 
  add_recipe(elasticnet_rec) %>% 
  add_model(best_elasticnet) 

best_elasticnet_fit <- 
  best_elasticnet_wflow %>% 
  fit(new_train)

## Es interesante ver las variables que quedaron seleccionadas
coefs_elasticnet <- best_elasticnet_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Predict
elasticnet_predictions <- predict(best_elasticnet_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar
write_csv(elasticnet_predictions, "submissions/05_ElasticNet.csv")


 














