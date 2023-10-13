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
test_data <- read_csv("../stores/test.csv")
train_data <- read_csv("../stores/train.csv")

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


# External variables -----------------------------------

##Parques
parques <- getbb("Bogota Colombia") %>% opq() %>%
  add_osm_feature(key = "leisure" , value = "park") %>% osmdata_sf()

parques_centroid <- parques$osm_polygons %>% select(osm_id, name) %>% st_centroid()

centroides_parques <- do.call(rbind, st_geometry(parques_centroid)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

centroides_parques <- st_as_sf(centroides_parques, coords = c("lon", "lat"), crs=4326)


##Public transportation
transport_station <- getbb("Bogota Colombia") %>% opq() %>%
  add_osm_feature(key = "public_transport" , value = "station") %>% osmdata_sf()

transport_centroid <- transport_station$osm_polygons %>%
  filter(amenity == "bus_station") %>% select(osm_id, name) %>%
  st_centroid()

centroides_transport <- do.call(rbind, st_geometry(transport_centroid)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

centroides_transport <- st_as_sf(centroides_transport, coords = c("lon", "lat"), crs=4326)


tmap_mode("view")
tm_shape(transport_centroid) +
  tm_sf()


# Spatial data ---------------------------------
sp_data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

factpal <- colorFactor(palette = c("red", "purple"), sp_data$type)

leaflet() %>%
  addTiles() %>%
  addCircles(data = sp_data, color = ~factpal(type)) %>%
  addLegend("bottomright", colors = c('red', 'purple'), values = ~type,
            title = "Type of sample",
            labels = c("Test", "Train"),
            opacity = 1
  )

# Distance  -----------------------------------

##Parks
dist_parks <- st_distance(x = sp_data, y = centroides_parques)


##Transport
dist_transport <- st_distance(x = sp_data, y = centroides_transport)