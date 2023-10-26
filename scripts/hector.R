# Cargar pacman (contiene la función p_load)
library(pacman)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(
  tidyverse, # Manipular dataframes
  rio, # Import data easily
  plotly, # Gráficos interactivos
  leaflet, # Mapas interactivos
  rgeos, # Calcular centroides de un poligono
  tmaptools, # geocode_OSM()
  sf, # Leer/escribir/manipular datos espaciales
  osmdata, # Get OSM's data
  tidymodels, # para modelos de ML
  stopwords, # para eliminar palabras sin "valor"
  tokenizers,
  SnowballC
)

rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 2")

# Importación de las bases -------------------------------------------

train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Junte de las bases para creación de variables ----------------------

train$train <- 1
test$train <- 0

db <- rbind(train, test)

# Variables importantes ----------------------------------------------

db <- db %>%
  mutate(description = str_replace_all(description, "[^a-zA-Z0-9 ]", " "))

# db$description <- tokenize_words(db$description)

db <- db %>%
  mutate(habitaciones = str_extract(description, "\\w+(?=\\s*alcoba)"))

db$habitaciones <- ifelse(is.na(db$habitaciones), str_extract(db$description, "\\w+(?=\\s*alcobas)"), db$habitaciones)
db$habitaciones <- ifelse(is.na(db$habitaciones), str_extract(db$description, "\\w+(?=\\s*habitacion)"), db$habitaciones)
db$habitaciones <- ifelse(is.na(db$habitaciones), str_extract(db$description, "\\w+(?=\\s*habitaciones)"), db$habitaciones)
db$habitaciones <- ifelse(is.na(db$habitaciones), str_extract(db$description, "\\w+(?=\\s*dormitorio)"), db$habitaciones)
db$habitaciones <- ifelse(is.na(db$habitaciones), str_extract(db$description, "\\w+(?=\\s*dormitorios)"), db$habitaciones)

db <- db %>%
  mutate(habitaciones2 = str_extract(description, "(\\w+|\\d+) alcoba"))

db$habitaciones2 <- ifelse(is.na(db$habitaciones2), str_extract(db$description, "(\\w+|\\d+) alcobas"), db$habitaciones2)
db$habitaciones2 <- ifelse(is.na(db$habitaciones2), str_extract(db$description, "(\\w+|\\d+) habitacion"), db$habitaciones2)
db$habitaciones2 <- ifelse(is.na(db$habitaciones2), str_extract(db$description, "(\\w+|\\d+) habitaciones"), db$habitaciones2)
db$habitaciones2 <- ifelse(is.na(db$habitaciones2), str_extract(db$description, "(\\w+|\\d+) dormitorio"), db$habitaciones2)
db$habitaciones2 <- ifelse(is.na(db$habitaciones2), str_extract(db$description, "(\\w+|\\d+) dormitorios"), db$habitaciones2)

# Distancia al centro comercial :::check
# Facilidades de transporte: vias principales, paradas de bus
# Aeropuerto
# Terraza balcn :::check
# piso :::check, cosas raras
# moderno 1 si aparece "moderno" :::check
# estrato :::check, muchos missings!!!
# Parqueadero - numero de parqueaderos :::check

db <- db %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero|parqueaderos|garaje|garajes", db$description)))

db <- db %>%
  mutate(terraza = as.numeric(grepl("terraza|balcn", db$description)))

db <- db %>%
  mutate(estrato = str_extract(description, "estrato (\\w+|\\d+)")) # muchos missings, no funciona bien

db <- db %>%
  mutate(moderno = as.numeric(grepl("moderno|remodelado", db$description)))

db <- db %>%
  mutate(piso = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

# Extraemos la info sobre centros comerciales de Bogotá -----------------------------------------------

CC <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "shop", value = "mall")

# Cambiando el formato a simple features (sf)

CC_sf <- osmdata_sf(CC)

# Geomoetría y donde estan ubicados los centros comerciales -------------------------------------------

CC_geometria <- CC_sf$osm_polygons %>%
  select(osm_id, name)

centroidesCC <- gCentroid(as(CC_geometria$geometry, "Spatial"), byid = T)

# Distancia al CC más cercano

db_sf <- st_as_sf(db, coords = c("lon", "lat"))

# Especificando el sistema de coordenadas, para que todo sea comparable

st_crs(db_sf) <- 4326

# Centroides a formato sf

centroidesCC_sf <- st_as_sf(centroidesCC, coords = c("x", "y"))

# Calculando la distancia

dist_matrixCC <- st_distance(x = db_sf, y = centroidesCC_sf)

# Distancia mínima

dist_minCC <- apply(dist_matrixCC, 1, min)

db_sf <- db_sf %>% mutate(distancia_minCC = dist_minCC)

# Revisión de la áreas de los CC, hay muchos pequeños

CC_geometria$areasCC <- st_area(CC_geometria)
summary(CC_geometria$areasCC)

# Quedarse con los CC con áreas mayores a 3000m^2

CC_geometria$area <- as.numeric(gsub("[m^2]", "", CC_geometria$areasCC))

CC_geometria_grandes <- CC_geometria %>%
  filter(CC_geometria$area > 3000)

posicion <- apply(dist_matrixCC, 1, function(x) which(min(x) == x))
db_sf <- db_sf %>%
  mutate(area_CC = as.numeric(CC_geometria$areas[posicion]))

# Nuevo gráfico con los CC grandes

centroidesCCnuevos <- gCentroid(as(CC_geometria_grandes$geometry, "Spatial"), byid = T)

centroidesCCnuevos_sf <- st_as_sf(centroidesCCnuevos, coords = c("x", "y"))

# Calculando la distancia

dist_matrixCCgrandes <- st_distance(x = db_sf, y = centroidesCCnuevos_sf)

# Distancia mínima

dist_minCCgrandes <- apply(dist_matrixCCgrandes, 1, min)

db_sf <- db_sf %>% mutate(distancia_minCCgrandes = dist_minCCgrandes)

# Distancia vías principales ---------------------------------------------------

# Vías "trunk" autopistas etc.

Vias <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf()

Vias_geometria <- Vias$osm_lines %>%
  select(osm_id, name)

Vias_sf <- st_as_sf(Vias_geometria) %>%
  select(geometry)

dist_vias <- st_distance(x = db_sf, y = Vias_sf)

db_sf <- db_sf %>%
  mutate(Min_dist_vias = apply(dist_vias, 1, min))

# Vías principales

Principales <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "highway", value = "primary") %>%
  osmdata_sf()

Principales_geometria <- Principales$osm_lines %>%
  select(osm_id, name)

Principales_sf <- st_as_sf(Principales_geometria) %>%
  select(geometry)

dist_Principales <- st_distance(x = db_sf, y = Principales_sf)

db_sf <- db_sf %>%
  mutate(Min_dist_principales = apply(dist_Principales, 1, min))

# Gráficos ----------------------------------------------------------------------

latitud_central <- mean(db_sf$lat)
longitud_central <- mean(db_sf$lon)

# Gráfico centros comerciales (grandes)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(
    data = CC_geometria_grandes, col = "red", weight = 10,
    opacity = 0.8, popup = CC_geometria_grandes$name
  ) %>%
  addCircles(
    lng = centroidesCC$x,
    lat = centroidesCC$y,
    col = "darkblue", opacity = 0.5, radius = 1
  )

# Histograma de distancias a CC. grandes

p <- ggplot(db, aes(x = distancia_minCCgrandes)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(
    x = "Distancia mínima a un CC en metros", y = "Cantidad",
    title = "Distribución de la distancia a los CC"
  ) +
  theme_bw()
ggplotly(p)

# Gráfico vías "trunk"

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolylines(
    data = Vias_sf, col = "red", weight = 5,
    opacity = 0.8
  )

# Tratamiento de NAs ----------------------------------------------------------

missings_db <- colSums(is.na(db))
print(missings_db) 

# Los missings de price corresponde a la base de test 

# Bathrooms tiene 12.562 missings

moda_baños_casas <- db %>%
  filter(property_type == "Casa") %>%
  count(bathrooms) %>%
  filter(n == max(n)) %>%
  pull(bathrooms) # Moda = 3

moda_baños_aptos <- db %>%
  filter(property_type == "Apartamento") %>%
  count(bathrooms) %>%
  filter(n == max(n)) %>%
  pull(bathrooms) # Moda = 2

media_baños_casas <- mean(db$bathrooms[db$property_type == "Casa"], na.rm = TRUE) # = 3,57
media_baños_aptos <- mean(db$bathrooms[db$property_type == "Apartamento"], na.rm = TRUE) # = 2,65 

# Imputar datos faltantes

db_NAs <- db_sf %>%
  mutate(bathrooms = ifelse(property_type == "Casa" & is.na(bathrooms), 3, bathrooms),
         bathrooms = ifelse(property_type == "Apartamento" & is.na(bathrooms), 2, bathrooms))

# Estimaciones y modelos ------------------------------------------------------

test <- db_NAs %>% subset(train == 0)
test <- test %>%
  select(-price)

train <- db_NAs %>% subset(train == 1) # vamos a entrenar con el 79% de los datos

nrow(train)/nrow(db_NAs)

lambda <- .7
# Ridge
ridge_spec <- linear_reg(penalty = lambda, mixture = 1) %>%
  set_engine("glmnet")

# Lasso
lasso_spec <- linear_reg(penalty = lambda, mixture = 0) %>%
  set_engine("glmnet")

# Elastic Net (se especifica el parámetro de mixture entre 0 y 1)

elastic_net_spec <- linear_reg(penalty = lambda, mixture = .5) %>%
  set_engine("glmnet")

# Primera receta

rec_1 <- recipe(price ~ bedrooms + bathrooms + property_type + parqueadero + terraza + moderno + distancia_minCCgrandes + area_CC + Min_dist_vias + Min_dist_principales, data = db) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Segunda receta

rec_2 <- recipe(price ~ bedrooms + bathrooms + property_type + parqueadero + terraza + moderno + distancia_minCCgrandes + area_CC + Min_dist_vias + Min_dist_principales, data = db) %>%
  step_interact(terms = ~ Min_dist_principales:property_type + area_CC:property_type) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Tercera receta

rec_3 <- recipe(price ~ bedrooms + bathrooms + property_type + parqueadero + terraza + moderno + distancia_minCCgrandes + area_CC + Min_dist_vias + Min_dist_principales, data = db) %>%
  step_interact(terms = ~ Min_dist_principales:property_type + area_CC:property_type) %>%
  step_poly(area_CC, distancia_minCCgrandes, Min_dist_vias, Min_dist_principales, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Working flows de cada receta con su metodología

# Receta 1

workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(ridge_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(lasso_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)

# Receta 2

workflow_2.1 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(ridge_spec)

workflow_2.2 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(lasso_spec)

workflow_2.3 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(elastic_net_spec)

# Receta 3

workflow_3.1 <- workflow() %>%
  add_recipe(rec_3) %>%
  add_model(ridge_spec)

workflow_3.2 <- workflow() %>%
  add_recipe(rec_3) %>%
  add_model(lasso_spec)

workflow_3.3 <- workflow() %>%
  add_recipe(rec_3) %>%
  add_model(elastic_net_spec)

# Entrenamos el primer modelo con los datos de train

fit_1.1 <- workflow_1.1 %>%
  fit(data = train)

fit_1.2 <- workflow_1.2 %>%
  fit(data = train)

fit_1.3 <- workflow_1.3 %>%
  fit(data = train)


fit_2.1 <- workflow_2.1 %>%
  fit(data = train)

fit_2.2 <- workflow_2.2 %>%
  fit(data = train)

fit_2.3 <- workflow_2.3 %>%
  fit(data = train)


fit_3.1 <- workflow_3.1 %>%
  fit(data = train)

fit_3.2 <- workflow_3.2 %>%
  fit(data = train)

fit_3.3 <- workflow_3.3 %>%
  fit(data = train)

predictiones_1.1 <- predict(fit_1.1 , new_data = test)

predictiones_1.2 <- predict(fit_1.2 , new_data = test)

predictiones_1.3 <- predict(fit_1.3, new_data = test)

predictiones_2.1 <- predict(fit_2.1 , new_data = test)

predictiones_2.2 <- predict(fit_2.2, new_data = test)

predictiones_2.3 <- predict(fit_2.3, new_data = test)

predictiones_3.1 <- predict(fit_3.1 , new_data = test)

predictiones_3.2 <- predict(fit_3.2, new_data = test)

predictiones_3.3 <- predict(fit_3.3, new_data = test)

eg <- read.csv("submission_template.csv")

submission_1.1 <- test %>% 
  select(property_id) %>% 
  mutate(predict(fit_1.1, new_data = test)) %>% 
  rename(price = .pred)

submission_3.1 <- test %>% 
  select(property_id) %>% 
  mutate(predict(fit_3.1, new_data = test)) %>% 
  rename(price = .pred)


list_recipes <- list(rec_1, rec_2, rec_3)

#Lapply with workflows
fit_tidy_model <- function(x, df=train) {
  lm_model <- linear_reg() %>% 
    set_engine("lm")
  
  workflow <- workflow() %>% 
    add_model(lm_model) %>% 
    add_recipe(x)
  
  fitted_model <-  workflow %>% 
    fit(data = df)
  
  fitted_model
}

list_workflows <- lapply(list_recipes, function(x){fit_tidy_model(x, train)})

#Lapply with predictions

predict_from_workflow <- function(w, df_test=test) {
  predictions <- predict(w, new_data = df_test) %>% 
    bind_cols(df_test)
}

list_predictions <- lapply(list_workflows, function (w){predict_from_workflow(w, test)})

predictions_OLS1 <- data.frame(list_predictions[[1]]) 
predictions_OLS2 <- data.frame(list_predictions[[2]])
predictions_OLS3 <- data.frame(list_predictions[[3]])

for (i in 1:3) { 
  nombre_objeto <- paste("predictions_OLS", i, sep = "")
  assign(nombre_objeto, get(nombre_objeto) %>% 
  select(property_id, .pred) %>% 
  rename(price = .pred))
  
  nombre_archivo <- paste("", nombre_objeto, ".csv", sep = "")
  write.csv(get(nombre_objeto), file = nombre_archivo, row.names = F)
} 

#Estimaciones con OLS, desisto de Ridge, Lasso, Elastic por la cantidad de predictores. 

### Más variables OSM ----------------------------------------------------------------

#Para que funcione la base con la de los demás 

sp_data <- db_sf 

write_csv(sp_data, file = "sp_dataH.csv")

sp_data <- read.csv("/Users/hectorsegura/Documentos/Big Data & ML/sp_dataH.csv")
sp_data <- st_as_sf(sp_data, coords = c("lon", "lat"), crs = 4326)

setwd("/Users/hectorsegura/Documentos/GitHub/bdml_problem_set_2/")

#Primero el mapa de Bogotá y ubicar las UPL
upl_bog <- st_read("stores/unidadplaneamientolocal.gpkg") %>%
  st_transform(crs=4326) %>%
  select(c(NOMBRE, SECTOR, SHAPE)) 

sp_data <- sp_data %>%
  st_join(upl_bog, left = T, join=st_intersects) 

bbox_bog <- st_bbox(upl_bog)

source("scripts/functions_OSM.R")

#Zonas comerciales 
zcomer_bog_points <- retrieve_amenities(bbox_bog, "landuse", "commercial", "polygons")
sp_data$dist_zcomer <- nearest_amenity(sp_data, zcomer_bog_points)

#Zonas retail: "Commercial businesses which sell goods"  
zretail_bog_points <- retrieve_amenities(bbox_bog, "landuse", "retail", "polygons")
sp_data$dist_zretail <- nearest_amenity(sp_data, zretail_bog_points)

#Zonas industriales
zindus_bog_points <- retrieve_amenities(bbox_bog, "landuse", "industrial", "polygons")
sp_data$dist_zindus <- nearest_amenity(sp_data, zindus_bog_points)

#Zonas en construcción y desarrollo activo
zcons_bog_points <- retrieve_amenities(bbox_bog, "landuse", "construction", "polygons")
sp_data$dist_zcons <- nearest_amenity(sp_data, zcons_bog_points)

#Zonas institucional (como de gobierno)
zinstitu_bog_points <- retrieve_amenities(bbox_bog, "landuse", "institutional", "polygons")
sp_data$dist_zinstitutional <- nearest_amenity(sp_data, zinstitu_bog_points)

#Zonas usadas predominantemente para propósitos educativos
zedu_bog_points <- retrieve_amenities(bbox_bog, "landuse", "education", "polygons")
sp_data$dist_zeducation <- nearest_amenity(sp_data, zedu_bog_points)

#Aeropuerto
airport_bog_points <- retrieve_amenities(bbox_bog, "aeroway", "aerodrome", "polygons")
sp_data$dist_airport <- nearest_amenity(sp_data, airport_bog_points)

#Universidades 
uni_bog_points <- retrieve_amenities(bbox_bog, "amenity", "university", "polygons")
sp_data$dist_uni <- nearest_amenity(sp_data, uni_bog_points)

#Hospitales ya está en variables.R 

#Estaciones de policía ya está en variables.R 
#Estaciones de bomberos en cambio
fire_bog_points <- retrieve_amenities(bbox_bog, "amenity", "fire_station", "polygons")
sp_data$dist_firest <- nearest_amenity(sp_data, fire_bog_points)

sp_data <- sp_data %>%
  rename(dist_zinstitutional = dist_zinstitu)

#Supermercado
supermarket_bog_points <- retrieve_amenities(bbox_bog, "shop", "supermarket", "polygons")
sp_data$dist_supermercado <- nearest_amenity(sp_data, supermarket_bog_points)





checkOSM <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "landuse", value = "construction") %>%
  osmdata_sf()




