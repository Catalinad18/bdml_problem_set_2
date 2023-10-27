##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2                                             #
##############################################################

#-------------------------------------------
# Load packages

require(pacman)
p_load("dplyr", "tidyverse", "sf", "leaflet", "Rcpp", "rio", "plotly", "tmaptools", "osmdata",
       "tidymodels", "ggmap", "tm", "udpipe", "stringi", "gdata")

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

#Zonas comerciales 
zcomer_bog_points <- retrieve_amenities(bbox_bog, "landuse", "commercial", "polygons")
sp_data$dist_zcommercial <- nearest_amenity(sp_data, zcomer_bog_points)

#Zonas retail: "Commercial businesses which sell goods"  
zretail_bog_points <- retrieve_amenities(bbox_bog, "landuse", "retail", "polygons")
sp_data$dist_zretail <- nearest_amenity(sp_data, zretail_bog_points)

#Zonas industriales
zindus_bog_points <- retrieve_amenities(bbox_bog, "landuse", "industrial", "polygons")
sp_data$dist_zindustrial <- nearest_amenity(sp_data, zindus_bog_points)

#Zonas en construcción y desarrollo activo
zcons_bog_points <- retrieve_amenities(bbox_bog, "landuse", "construction", "polygons")
sp_data$dist_zconstruction <- nearest_amenity(sp_data, zcons_bog_points)

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
sp_data$dist_university <- nearest_amenity(sp_data, uni_bog_points)

#Estaciones de bomberos
fire_bog_points <- retrieve_amenities(bbox_bog, "amenity", "fire_station", "polygons")
sp_data$dist_firest <- nearest_amenity(sp_data, fire_bog_points)

# Análisis de textos ------------------------------------------------------

model_udpipe <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")

#Vectorizacion de las descripciones de los hogares usando BOW

lemmatizer <- function(x, model) {
  
  stemmed_descriptions <- udpipe_annotate(model, x, tagger = "default", parser = "none")
  tibble <- as_tibble(stemmed_descriptions)

  paste(tibble$lemma, collapse = " ")
  
}


clean_descriptions <- function(v){
  
  useless_words <- c("cod", "codfr", "m2", "mt2", "metros", "cuadrados", "area", "fr", "mts", "x", "br", "fr", "aacute", "eacute",
                     "iacute", "oacute", "uacute", "tilde", "b", "patricia", "tania", "uberney", "luzma")
  
  descriptions <- VCorpus(VectorSource(v), readerControl = list(language = "spanish"))
  descriptions <- tm_map(descriptions, content_transformer(tolower))
  descriptions <- tm_map(descriptions, removeWords, stopwords("spanish"))
  descriptions <- tm_map(descriptions, removeNumbers)
  descriptions <- tm_map(descriptions, removeWords, useless_words)
  descriptions <- tm_map(descriptions, stripWhitespace)
  descriptions <- data.frame(description=unlist(sapply(descriptions, `[`, "content")), 
                                          stringsAsFactors=F)
  
  stemmed_descriptions <- c()
  
  for (d in descriptions$description) {
    stemmed <- lemmatizer(d, model_udpipe)
    stemmed_descriptions <- c(stemmed_descriptions, stemmed)
  }
  #Somme lemmas have accents
  stemmed_descriptions <- stri_trans_general(stemmed_descriptions, id = "Latin-ASCII")
  
  #Return stemmed descriptions in a vector
  stemmed_descriptions
  
}

# Las descripciones limpias son utilizadas como variables. 
train_descriptions <- clean_descriptions(train_data$description)

# Matriz de palabras train
train_descriptions_corpus <- VCorpus(VectorSource(train_descriptions), readerControl = list(language = "spanish"))
dtm_train<-DocumentTermMatrix(train_descriptions_corpus)
rm_sparse_dtm_train <- removeSparseTerms(dtm_train, 0.99)

dense_dtm_train <- as.matrix(rm_sparse_dtm_train)
words_train <- colnames(dense_dtm_train)

#Repetir el ejercicio con test, pero solo incluimos las palabras de train
test_descriptions <- clean_descriptions(test_data$description)
test_descriptions_corpus <- VCorpus(VectorSource(test_descriptions), readerControl = list(language = "spanish"))

dense_dtm_test<-as.matrix(DocumentTermMatrix(test_descriptions_corpus, list(dictionary=words_train)))






