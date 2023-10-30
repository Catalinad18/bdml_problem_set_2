##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2: Lasso/Ridge/Elastic Net.                   #
##############################################################
require(pacman)
p_load("dplyr", "tidyverse", "sf", "leaflet", "Rcpp", "rio", "plotly", "tmaptools", "osmdata",
       "tidymodels", "ggmap", "tm", "udpipe", "stringi", "gdata", "spatialsample", "glmnet", "doParallel")

sp_data <- readRDS("stores/Bases Finales/sp_data_final.rds")
dense_dtm_train <- readRDS("stores/Bases Finales/DTM_train.rds")
dense_dtm_test <- readRDS("stores/Bases Finales/DTM_test.rds")
test_data <- read.csv("scripts/base_datos_limpia_test.csv")


new_data <- sp_data %>%
  mutate(log_price=log(price)) %>% #Mejor log
  select(-c(price, city, month, year, title, description, area, property_type, precio_por_mt2, piso_info,
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

# Folds -------------------------------------------------------------------

#Sólo para probar, usamos un tipo diferente de splits espaciales (k means)
set.seed(123)
block_folds <- spatial_block_cv(new_train, v = 5)
autoplot(block_folds)

# Lasso -------------------------------------------------------------------

#Model
lasso <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lambda_grid_lasso <- grid_regular(penalty(), levels = 50) #Spatial cross validation

#Recipe
lasso_rec <-
  recipe(log_price ~ ., data = select(as_tibble(new_train), -geometry)) %>% 
  #step_rm(NOMBRE) %>% #NOMBRE es la variable para el CV ahora, entonces no funcionan bien los efectos fijos. Vamos a ver. 
  step_dummy(ESTRATO) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
lasso_wflow <- 
  workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso) 

#Tune grid
registerDoParallel()
tune_result_lasso <- lasso_wflow %>% 
  tune_grid(resamples = block_folds,
            grid = lambda_grid_lasso,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))
stopImplicitCluster()
stopImplicitCluster()

results_tuning_lasso <- tune_result_lasso %>%
  collect_metrics()

plot_lasso_penalty<- results_tuning_lasso %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(linewidth = 1) +
  ylab("MAE") +
  xlab("Penalidad (lambda)") + 
  theme_bw() +
  theme(legend.position = "none") 

ggsave("views/plot_lasso_penalty.png", plot_lasso_penalty, dpi=300)

#Best fit 
tune_best_lasso <- tune_result_lasso %>% 
  select_best(metric = "mae")

best_lasso <- linear_reg(penalty = tune_best_lasso$penalty, mixture = 1) %>%
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
write_csv(lasso_predictions, "submissions/03_Lasso_VF.csv")

write_csv(coefs_lasso, "stores/Bases Finales/coefs_lasso.csv")

# Ridge -------------------------------------------------------------------


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
registerDoParallel()
tune_result_ridge <- ridge_wflow %>% 
  tune_grid(resamples = block_folds,
            grid = lambda_grid_ridge,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))

stopImplicitCluster()
stopImplicitCluster()

results_tuning_ridge <- tune_result_ridge %>%
  collect_metrics()

tune_result_ridge %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "RMSE")

#Best fit 
tune_best_ridge <- tune_result_ridge %>% 
  select_best(metric = "mae")

best_ridge <- linear_reg(penalty = tune_best_ridge$penalty, mixture = 0) %>%
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
write_csv(ridge_predictions, "submissions/04_RidgevF.csv")


# Elastic Net -------------------------------------------------------------

#Model
elasticnet <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

lambda_grid_elasticnet <- grid_regular(penalty(), mixture(), levels = c(30, 30)) #Spatial cross validation

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
registerDoParallel()
tune_result <- elasticnet_wflow %>% 
  tune_grid(resamples = block_folds,
            grid = lambda_grid_elasticnet,
            metrics = metric_set(mae),
            control=control_grid(verbose=TRUE))
stopImplicitCluster()
results_tuning_elasticnet <- tune_result %>%
  collect_metrics()

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
write_csv(elasticnet_predictions, "submissions/ElasticNetBest.csv")