#Problem Set 2

#Cargamos paquetes requeridos

library(pacman)
p_load(tidyverse, rio, tidymodels, glmnet, stargazer, dplyr)

#Cargamos data-frames

file_dir <- this.path::here()
setwd(file_dir)
df <- read_csv("../stores/train.csv")
df_test <- read_csv(("../stores/test.csv"))

#Visualizamos resumen de data-frames para ver si hay NaN's y dónde los hay

glimpse(df)
glimpse(df_test)
sapply(df, function(x) sum(is.na(x)))
sapply(df_test, function(x) sum(is.na(x)))

#Revisamos distribución de datos en variables que tienen NaN's

#train

distributionSurfaceTotal <- function (surface_totall) {
  N = length(df$surface_total)
  surface_totall <- na.omit(df$surface_total)
  hist( df$surface_total,col = "light blue")
}

distributionSurfaceTotal()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con mediana.

distributionSurfaceCovered <- function (surface_coveredd) {
  N = length(df$surface_covered)
  surface_coveredd <- na.omit(df$surface_covered)
  hist( df$surface_covered,col = "light blue")
}

distributionSurfaceCovered()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con mediana.

distributionRooms <- function (rooms) {
  N = length(df$rooms)
  rooms <- na.omit(df$rooms)
  hist( df$rooms,col = "light blue")
}

distributionRooms()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con mediana.

distributionBathrooms <- function (bathrooms) {
  N = length(df$bathrooms)
  bathrooms <- na.omit(df$bathrooms)
  hist( df$bathrooms,col = "light blue")
}

distributionBathrooms()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con mediana.

##Distribución variable dependiente

distributionPrice <- function (pricee) {
  N = length(df$price)
  pricee <- na.omit(df$price)
  hist( df$price,col = "light blue")
}

distributionPrice()

#Posee una distribución asimétrica hacia la izquierda, por lo que haremos una transformación logarítmica

#df <- df %>% mutate(price = log(price))

#test

distributionSurfaceTotalTest <- function (surface_totall) {
  N = length(df_test$surface_total)
  surface_totall <- na.omit(df_test$surface_total)
  hist( df_test$surface_total,col = "light blue")
}

distributionSurfaceTotalTest()

#Vemos que hay una distribución asimétrica, echada a la izquierda, con un valor atípico. Para evitar leverage, imputaremos con mediana.

distributionSurfaceCoveredTest <- function (surface_coveredd) {
  N = length(df_test$surface_covered)
  surface_coveredd <- na.omit(df_test$surface_covered)
  hist( df_test$surface_covered,col = "light blue")
}

distributionSurfaceCoveredTest()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con mediana.

distributionRoomsTest <- function (rooms) {
  N = length(df_test$rooms)
  rooms <- na.omit(df_test$rooms)
  hist( df_test$rooms,col = "light blue")
}

distributionRoomsTest()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con mediana.

distributionBathroomsTest <- function (bathrooms) {
  N = length(df_test$bathrooms)
  bathrooms <- na.omit(df_test$bathrooms)
  hist( df_test$bathrooms,col = "light blue")
}

distributionBathroomsTest()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con mediana.

##Imputación

#Cálculos de medianas

#train

mediana_sup_cubierta <- median(df$surface_covered, na.rm = TRUE) #108
mediana_sup_total <- median(df$surface_total, na.rm = TRUE) #119
mediana_bathrooms <- median(df$bathrooms, na.rm = TRUE) #3
mediana_rooms <- median(df$rooms, na.rm = TRUE) #2

#test

mediana_sup_cubierta_test <- median(df_test$surface_covered, na.rm = TRUE) #118
mediana_sup_total_test <- median(df_test$surface_total, na.rm = TRUE) #120
mediana_bathrooms_test <- median(df_test$bathrooms, na.rm = TRUE) #3
mediana_rooms_test <- median(df_test$rooms, na.rm = TRUE) #2

#Reemplazo de datos faltantes

#train

df <- df %>%
  mutate(rooms = replace_na(rooms, 2),
         bathrooms = replace_na(bathrooms, 3),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
         surface_total = replace_na(surface_total, mediana_sup_total),)

#test

df_test <- df_test %>%
  mutate(rooms = replace_na(rooms, 2),
         bathrooms = replace_na(bathrooms, 3),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta_test),
         surface_total = replace_na(surface_total, mediana_sup_total_test),)

#Tratamiento variable dicótoma property_type

#train

#df <- df %>%
  #mutate(property_typed = ifelse(df$property_type == "Casa", 1, 0))

df <- df %>%
  mutate(property_type = case_when(property_type == "Apartamento" ~ 1,
                                   property_type == "Casa" ~ 0)
         )

#test

df_test <- df_test %>%
  mutate(property_type = case_when(property_type == "Apartamento" ~ 1,
                                   property_type == "Casa" ~ 0)
  )

##Receta 1, OLS

rec1 <- recipe(price ~ surface_total + surface_covered + rooms + bedrooms + bathrooms + property_type, data = df)

lm_mod <- linear_reg()

##Workflow 1, OLS

wfl1 <- workflow() %>%
  add_recipe(rec1) %>%
  add_model(lm_mod)

#K-fold CV

set.seed(666)

data_split <- initial_split(df, prop = .7)
train <- training(data_split)
test <- testing(data_split)

##Fit de datos

fit1.1 <- wfl1 %>%
  fit(data=train)

##Predicción

test_predict <- predict(fit1.1, new_data = test) %>%
  bind_cols(test)

##RMSE promedio K-fold

folds <- vfold_cv(df, v = 5)

fit_res2.1 <- fit_resamples(
  wfl1,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold

individual_rmse2.1 <- fit_res2.1 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)

individual_rmse2.1

mean(individual_rmse2.1$.estimate)

#Ridge y Lasso para selección de variables

##Ridge

ridge_spec <- linear_reg(mixture = 0, penalty = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

ridge_fit <- fit(ridge_spec, price ~ surface_total + surface_covered + rooms + bedrooms + bathrooms + property_type, data = df)

#Visualizar

tidy(ridge_fit)

tidy(ridge_fit, penalty = 28000000000000000000)

ridge_fit %>%
  autoplot()

#Predicción Ridge

predict(ridge_fit, new_data = df)
predict(ridge_fit, new_data = df, penalty = 5000000000000000000000000000)

#K-fold para hallar el valor óptimo del lambda

#El objeto folds ya está creado

ridge_recipe <- 
  recipe(formula = price ~ surface_total + surface_covered + rooms + bedrooms + bathrooms + property_type, data = df) %>%
  step_dummy(all_nominal_predictors()) %>%
  #step_interact(terms = ~ property_type:surface_total + property_type:surface_covered + property_type:rooms + property_type:bedrooms + property_type:bathrooms) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

ridge_spec2 <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

ridge_workflow <- workflow() %>%
  add_recipe(ridge_recipe) %>%
  add_model(ridge_spec2)

penalty_grid <- grid_regular(penalty(range = c(-4, 1)), levels = 30)

penalty_grid

tune_res <- tune_grid(
  ridge_workflow,
  resamples = folds,
  grid = penalty_grid,
  metrics = metric_set(rmse)
)

tune_res

autoplot(tune_res)

collect_metrics(tune_res)

best_penalty <- select_best(tune_res, metric = "rmse")

best_penalty

ridge_final <- finalize_workflow(ridge_workflow, best_penalty)

ridge_final_fit <- fit(ridge_final, data = df)

augment(ridge_final_fit, new_data = df) %>%
  rmse(truth = price, estimate = .pred)