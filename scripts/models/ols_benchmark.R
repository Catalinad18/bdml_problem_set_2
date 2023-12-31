##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2: OLS Benchmark.                             #
##############################################################


# Libraries ---------------------------------------------------------------
require(pacman)
p_load(sf, tidymodels, tidyverse)

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------
source("scripts/variables.R")

new_data <- sp_data %>%
  mutate(log_price=log(price)) %>% #Mejor log
  as_tibble %>%
  select(-c(price, city, month, year, surface_total, title, description, geometry, operation_type))

new_train<- new_data %>%
  filter(type=="train") %>%
  select(-c(type, property_id))

new_test<- new_data %>%
  filter(type=="test") %>%
  select(-c(type, property_id))

# 1) OLS ------------------------------------------------------------------

#Recipe
initial_rec <- 
  recipe(log_price ~ ., data = new_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

#Modelo
initial_model <- linear_reg()

#Workflow
initial_wflow <- 
  workflow() %>% 
  add_model(initial_model) %>% 
  add_recipe(initial_rec)

#Fit
initial_fit <- initial_wflow %>%
  fit(new_train)

#Predict
initial_predictions <- predict(initial_fit, new_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar
write_csv(initial_predictions, "submissions/01_OLS.csv")


# OLS con datos de texto --------------------------------------------------

olstext_train <- bind_cols(select(new_train, -c(parqueadero, balcon, terraza)), dense_dtm_train)
olstext_test <- bind_cols(select(new_test, -c(parqueadero, balcon, terraza)), dense_dtm_test)

#Recipe
olstext_rec <- 
  recipe(log_price ~ ., data = olstext_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

#Workflow
olstext_wflow <- 
  workflow() %>% 
  add_model(initial_model) %>% 
  add_recipe(olstext_rec)

#Fit
olstext_fit <- olstext_wflow %>%
  fit(olstext_train)

#Predict
olstext_predictions <- predict(olstext_fit, olstext_test) %>%
  bind_cols(test_data$property_id) %>%
  mutate(price=exp(.pred)) %>%
  select(-.pred) %>%
  rename(c("property_id"="...2"))

## Exportar
write_csv(olstext_predictions, "submissions/02_OLS(text).csv")







