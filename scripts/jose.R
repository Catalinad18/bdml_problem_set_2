#Problem Set 2

#Cargamos paquetes requeridos

library(pacman)
p_load(tidyverse, 
       rio, 
       tidymodels, 
       glmnet, 
       stargazer, 
       dplyr, 
       plotly, 
       leaflet, 
       rgeos, 
       tmaptools, 
       sf, 
       osmdata)

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

#Vemos que hay una distribución asimétrica, y aunque más normalizada, sigue estando echada a la izquierda. Imputaremos con moda, al tomar valores enteros.

distributionBathrooms <- function (bathrooms) {
  N = length(df$bathrooms)
  bathrooms <- na.omit(df$bathrooms)
  hist( df$bathrooms,col = "light blue")
}

distributionBathrooms()

#Vemos que hay una distribución asimétrica y echada a la izquierda. Imputaremos con moda, al tomar valores enteros.


##Distribución variable dependiente

distributionPrice <- function (pricee) {
  N = length(df$price)
  pricee <- na.omit(df$price)
  hist( df$price,col = "light blue")
}

distributionPrice()

#Posee una distribución asimétrica hacia la izquierda, por lo que haremos una transformación logarítmica
#Revisamos posibles outliers

summary(df$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

#V1
#Min.      $300,000,000
#1st Qu.   $415,000,000
#Median    $559,990,000
#Mean      $654,534,675
#3rd Qu.   $810,000,000
#Max.    $1,650,000,000

#Como vemos que el valor máximo casi que dupica el tercer cuantil, revisamos que no sean errores o datos sin sentido, viendo la distribución del precio del metro cuadrado

df <- df %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))

summary(df$precio_por_mt2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

#Vemos que hay un valor mínimo de $20,424 y uno máximo de $40,450,000 por metro cuadrado, lo cual, a todas luces es extraordinariamente alto.
#Estableceremos un rango mínimo de $600,000 máximo de $15,000,000 por metro cuadrado (según estimaciones, por barrio, el metro cuadrado más caro de Bogotá ronda alrededor de $7,145,435, y el más barato, alrededor de $873.138. Por ello dejaremos un poco de espacio para evitar sesgo, pero sí para eliminar valores irreales que puedan perjudicar las predicciones)

df <- df %>%
  filter(between(precio_por_mt2, 600000,  15e6))

#Visualizamos la nueva distribución de la variable dependiente.

p <- ggplot(df, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()

p

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

#Vemos que hay una distribución asimétrica, aunque algo más normalizada, pero echada a la izquierda. Imputaremos con mediana.

##Imputación

#Cálculos de modas y medianas

#train

df %>%count(rooms) %>% head() #La moda es 3
df %>%count(bathrooms) %>% head() #La moda es 2

mediana_sup_cubierta <- median(df$surface_covered, na.rm = TRUE) #108
mediana_sup_total <- median(df$surface_total, na.rm = TRUE) #119
mediana_bathrooms <- median(df$bathrooms, na.rm = TRUE) #3
mediana_rooms <- median(df$rooms, na.rm = TRUE) #3

#test

df_test %>%count(rooms) %>% head() #La moda es 3
df_test %>%count(bathrooms) %>% head() #La moda es 2

mediana_sup_cubierta_test <- median(df_test$surface_covered, na.rm = TRUE) #118
mediana_sup_total_test <- median(df_test$surface_total, na.rm = TRUE) #120
mediana_bathrooms_test <- median(df_test$bathrooms, na.rm = TRUE) #3
mediana_rooms_test <- median(df_test$rooms, na.rm = TRUE) #2

#Reemplazo de datos faltantes

#train

df <- df %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
         surface_total = replace_na(surface_total, mediana_sup_total),)

#test

df_test <- df_test %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta_test),
         surface_total = replace_na(surface_total, mediana_sup_total_test),)

##Tratamiento Datos Espaciales


#Filtramos observaciones que no tengan latitud ni longitud

df <- df %>%
  filter(!is.na(lat) & !is.na(lon))

df_test <- df_test %>%
  filter(!is.na(lat) & !is.na(lon))

##Visualización y tratamiento de datos espaciales

#Visualizamos los datos

leaflet() %>%
  addTiles() %>%
  addCircles(lng = df$lon,
             lat = df$lat)

leaflet() %>%
  addTiles() %>%
  addCircles(lng = df_test$lon,
             lat = df_test$lat)

#Establecemos los límites de los datos espaciales para que coincidan con los límites del área urbana de la ciudad de Bogotá, de acuerdo a como los establece OSM

limites <- getbb("Bogotá Colombia")

limites

df <- df %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )

df_test <- df_test %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )

#Escalamos la variable de precio de metro cuadrado para mejorar visualización

df <- df %>%
  mutate(precio_por_mt2_sc =( (precio_por_mt2 - min(precio_por_mt2)) / (max(precio_por_mt2) - min(precio_por_mt2))))

#Gráfica de Colores por precio

df <- df %>%
  mutate(color = case_when(precio_por_mt2 >= 13000000 ~ "#F32F0E",
                           precio_por_mt2 >= 10000000 & precio_por_mt2_sc <= 12999999 ~ "#F3870E",
                           precio_por_mt2 >= 8000000 & precio_por_mt2_sc <= 9999999 ~ "#F3E70E",
                           precio_por_mt2 >= 6000000 & precio_por_mt2_sc <= 7999999 ~ "#AFF30E",
                           precio_por_mt2 >= 4000000 & precio_por_mt2_sc <= 5999999 ~ "#0EF35E",
                           precio_por_mt2 >= 3000000 & precio_por_mt2_sc <= 3999999 ~ "#0EF3CF",
                           precio_por_mt2 >= 2000000 & precio_por_mt2_sc <= 2999999 ~ "#17B53E",
                           precio_por_mt2 >= 1000000 & precio_por_mt2_sc <= 1999999 ~ "#0EC7F3",
                           precio_por_mt2 >= 900000 & precio_por_mt2_sc <= 999999 ~ "#0E66F3",
                           precio_por_mt2 >= 800000 & precio_por_mt2_sc <= 899999 ~ "#1E0EF3",
                           precio_por_mt2 >= 7000000 & precio_por_mt2_sc <= 799999 ~ "#970EF3"))


leaflet() %>%
  addTiles() %>%
  addCircles(lng = df$lon, 
             lat = df$lat, 
             col = df$color,
             fillOpacity = 1,
             opacity = 1,
             radius = df$precio_por_mt2_sc*10)

##Tratamiento de texto de la variable "description"


#Normalización

  #Pasamos todo a minúscula

df <- df %>%
  mutate(description = str_to_lower(description))

df_test <- df_test %>%
  mutate(description = str_to_lower(description))

  #Eliminamos tildes

df <- df %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

df_test <- df_test %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

  #Eliminamos caracteres especiales que no sean alfanuméricos

df <- df %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

df_test <- df_test %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

  #Eliminamos espacios extras

df <- df %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

df_test <- df_test %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))


#Verificación variable property_type

#train

df <- df %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento|pent house", description), "apartamento", property_type))

df <- df %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "casa", property_type)) %>%
  select(-property_type)

#test

df_test <- df_test %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento|pent house", description), "apartamento", property_type))

df_test <- df_test %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "casa", property_type)) %>%
  select(-property_type)

#Ahora podemos convertir a Dummy la variable property_type_2

#Tratamiento variable dicótoma property_type

#train

#df <- df %>%
#mutate(property_typed = ifelse(df$property_type == "Casa", 1, 0))

df <- df %>%
  mutate(property_type = case_when(property_type_2 == "apartamento" ~ 0,
                                   property_type_2 == "Apartamento" ~ 0,
                                   property_type_2 == "Casa" ~ 1,
                                   property_type_2 == "casa" ~ 1)
  )

#test

df_test <- df_test %>%
  mutate(property_type = case_when(property_type_2 == "apartamento" ~ 0,
                                   property_type_2 == "Apartamento" ~ 0,
                                   property_type_2 == "Casa" ~ 1,
                                   property_type_2 == "casa" ~ 1)
  )

#Verificación variable bedrooms



#Variable Piso para apartamentos




##Algoritmo detección áreas:  #1. Eliminar todos los espacios. 
                              #2. Detectar todos los casos donde \d\d\d\d(mts) | \d\d\d(mts) | \d\d(mts) | \d\d\d\d(m2) | \d\d\d(m2) | \d\d(m2) | \d\d\d\d(metros) | \d\d\d(metros) | \d\d\(metros) | \d\d\d\d(metroscuadrados) | \d\d\d(metroscuadrados) | \d\d(metroscuadrados) | \d\d\d\d(metros2) | \d\d\d(metros2) | \d\d(metros2) 
                              #3. Crear una columna donde se almacenen esos datos, con letras. 
                              #4. Detectar en esa nueva columna solamente \d\d\d\d | \d\d\d | \d\d. 
                              #5. Almacenar esos datos numéricos en otra nueva columna. 
                              #6. Reemplazar datos en la columna de superficie (¿ambas superficie cubierta y total?), solamente si el valor es faltante.


#Algoritmo detección de baños: (¿La ñ se toma como caracter especial?)  
                              #1. Eliminar todos los espacios.
                              #2. Detectar todos los casos donde \d\d(baos) | \d(baos) | \d(bao) | \d\d(banos) | \d(banos) | \d(bano) | \w\w\w\w\w\w\w(banos) | \w\w\w\w\w\w(banos) | \w\w\w\w\w(banos) | \w\w\w\w(banos) |\w\w\w(banos) | \w\w(banos) | \w\w(bano) \w\w\w\w\w\w\w(banos) | \w\w\w\w\w\w(banos) | \w\w\w\w\w(banos) | \w\w\w\w(banos) |\w\w\w(banos) | \w\w(banos) | \w\w(bano)
                              #3. Crear una columna donde se almacenen esos datos, con letras.
                              #4. Para baños con números: Detectar en esa nueva columna solamente \d\d | \d
                              #5. Para baños con números:Almacenar esos datos numéricos en otra nueva columna.
                              #6. Para baños con números: Reemplazar datos en la columna de baños, solamente si el valor falta.
                              #7. Para baños con letras: Detectar en esa nueva columna solamente (baos) | (bao) | (banos) |(bao)
                              #8. Eliminar de toda la columna las expresiones anteriormente dichas.
                              #9. Transliterar cada palabra a número (reemplazar un = 1, dos = 2). O As.numeric?
                              #10. Reemplazar datos en la columna de baños.

#¿Qué hacer con rooms?

#Variable de pisos para apartamentos. ¿Relación cuadrática?
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

test_predict <- predict(fit1.1, new_data = df_test) %>%
  bind_cols(df_test)

test_predict1 <- test_predict[,c("property_id", ".pred")]
colnames(test_predict1) <- c("property_id", "price")

write_csv(test_predict1, "modelo1.csv")

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

fit1.1 <- wfl1 %>%
  fit(data=train)

##Predicción Ridge

test_predict2 <- predict(ridge_final_fit, new_data = df_test) %>%
  bind_cols(df_test)

test_predict3 <- test_predict2[,c("property_id", ".pred")]
colnames(test_predict3) <- c("property_id", "price")

write_csv(test_predict3, "modelo2_ridge.csv")

augment(ridge_final_fit, new_data = df) %>%
  rmse(truth = price, estimate = .pred)

##LASSO

lasso_recipe <- 
  recipe(formula = price ~ surface_total + surface_covered + rooms + bedrooms + bathrooms + property_type, data = df) %>% 
  #step_interact(terms = ~ property_type:surface_total + property_type:surface_covered + property_type:rooms + property_type:bedrooms + property_type:bathrooms) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

lasso_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

lasso_fit <- fit(lasso_spec, price ~ surface_total + surface_covered + rooms + bedrooms + bathrooms + property_type, data = df)

tidy(lasso_fit, penalty = 0)

tidy(lasso_fit, penalty = 28000000000000000000)

lasso_fit %>%
  autoplot()

lasso_workflow <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_spec)

penalty_grid_lasso <- grid_regular(penalty(range = c(-10, 10)), levels = 50)

tune_res_lasso <- tune_grid(
  lasso_workflow,
  resamples = folds, 
  grid = penalty_grid,
  metrics = metric_set(rmse)
)

autoplot(tune_res_lasso)

best_penalty_lasso <- select_best(tune_res_lasso, metric = "rmse")

lasso_final <- finalize_workflow(lasso_workflow, best_penalty_lasso)

lasso_final_fit <- fit(lasso_final, data = df)

augment(lasso_final_fit, new_data = df) %>%
  rmse(truth = price , estimate = .pred)

test_predict4 <- predict(lasso_final_fit, new_data = df_test) %>%
  bind_cols(df_test)

#Predicción LASSO

test_predict4 <- test_predict4[,c("property_id", ".pred")]
colnames(test_predict4) <- c("property_id", "price")

write_csv(test_predict4, "modelo3_lasso.csv")