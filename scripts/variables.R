##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2                                             #
##############################################################

#-------------------------------------------
# Load packages
pkg <- list("dplyr", "tidyverse", "sf", "leaflet", "Rcpp", "rio", "plotly", "tmaptools", "osmdata", "tidymodels", "ggmap")
lapply(pkg, require, character.only = T)
rm(pkg)

# Clean environment
rm(list = ls())
#-------------------------------------------

# Load data ---------------------------------
test_data <- read_csv("stores/test.csv")
train_data <- read_csv("stores/train.csv")

#Append data
train_data <- train_data %>% mutate(type = "train")
test_data <- test_data %>% mutate(type = "test")

data <- rbind(train_data, test_data)


# Clean data ---------------------------------

#Datos faltantes
#Vamos a imputar valores para el número de rooms, baños, 
#área de superficie total y cubierta. Los dos primeros con la moda al
#tomar valores enteros y los dos últimos con la mediana.

##Moda
data %>%count(rooms) %>% head() #La moda es 3
data %>%count(bathrooms) %>% head() #La moda es 2

##Mediana
mediana_sup_cubierta <- median(data$surface_covered, na.rm = TRUE)
mediana_sup_total<- median(data$surface_total, na.rm = TRUE)

## Poner datos faltantes
data <- data %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
         surface_total = replace_na(surface_total, mediana_sup_total),)


#Procesamiento de la descripción

##Caracteres especiales
data <- data %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

##Espacios extras
data <- data %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

# Selección variables ----------------------------------

##Parqueadero
data <- data %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero | garaje", data$description)))

##Balcón
data <- data %>%
  mutate(balcon = as.numeric(grepl("balc", data$description)))

##Terraza
data <- data %>%
  mutate(terraza = as.numeric(grepl("terraza", data$description)))


# OSM ---------------------------------------------------------------------
source("scripts/functions_OSM.R")

#Datos como sf
sp_data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

#Primero el mapa de Bogotá y ubicar las UPL
upl_bog <- st_read("stores/unidadplaneamientolocal.gpkg") %>%
  st_transform(crs=4326) %>%
  select(c(NOMBRE, SECTOR, SHAPE)) 

sp_data <- sp_data %>%
  st_join(upl_bog, left = T, join=st_intersects)

bbox_bog <- st_bbox(upl_bog)

#### Amenidades

#Parques
parques_bog_points <- retrieve_amenities(bbox = bbox_bog, key = "leisure", value = "park", type = "polygons")
sp_data$dist_parque <- nearest_amenity(sp_data, parques_bog_points)

#Hospitales
hospitals_bog_points <- retrieve_amenities(bbox_bog, "amenity", "hospital")
sp_data$dist_hsopital <- nearest_amenity(sp_data, hospitals_bog_points)

#Colegios
schools_bog_points <- retrieve_amenities(bbox_bog, "amenity", "school")
sp_data$dist_colegio <- nearest_amenity(sp_data, schools_bog_points)

#Buses
bus_bog_points <- retrieve_amenities(bbox_bog, "amenity", "bus_station", "points")
sp_data$dist_bus <- nearest_amenity(sp_data, bus_bog_points)

#Policía
police_bog_points <- retrieve_amenities(bbox_bog, "amenity", "police")
sp_data$dist_policia <- nearest_amenity(sp_data, police_bog_points)

#Bares
bar_bog_points <- retrieve_amenities(bbox_bog, "amenity", "bar", "points")
sp_data$dist_bar <- nearest_amenity(sp_data, bar_bog_points)

#Restaurantes
restaurant_bog_points <- retrieve_amenities(bbox_bog, "amenity", "restaurant", "points")
sp_data$dist_restaurante <- nearest_amenity(sp_data, restaurant_bog_points)

#Club-discoteca
nightclub_bog_points <- retrieve_amenities(bbox_bog, "amenity", "nightclub", "points")
sp_data$dist_nightclub <- nearest_amenity(sp_data, nightclub_bog_points)

#Carcel
prison_bog_points <- retrieve_amenities(bbox_bog, "amenity", "prison", "points")
sp_data$dist_carcel <- nearest_amenity(sp_data, prison_bog_points)

#Supermercado
supermarket_bog_points <- retrieve_amenities(bbox_bog, "shop", "supermarket", "points")
sp_data$dist_supermercado <- nearest_amenity(sp_data, supermarket_bog_points)

#Mall
mall_bog_points <- retrieve_amenities(bbox_bog, "shop", "mall")
sp_data$dist_mall <- nearest_amenity(sp_data, mall_bog_points)































