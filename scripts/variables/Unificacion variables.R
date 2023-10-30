##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2: Definicion de variables                    #
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
test_data <- read_csv("scripts/base_datos_limpia_test.csv") #Bases procesadas por Jose
train_data <- read_csv("scripts/base_datos_limpia_train.csv")

#Append data
train_data <- train_data %>% mutate(type = "train")
test_data <- test_data %>% mutate(type = "test")

data <- rbind(train_data, test_data)

#Variables Hector
hector_clean <- read_csv("scripts/sp_dataHector.csv")
vars_clean_hector <- c("property_id","distancia_minCC", "area_CC", "distancia_minCCgrandes", "Min_dist_vias", "Min_dist_principales", "dist_zcomer", "dist_airport", "dist_uni",              
"dist_zindus", "dist_zcommercial", "dist_zretail", "dist_zindustrial", "dist_zconstruction", "dist_zinstitutional", 
"dist_zeducation", "dist_university", "dist_firest", "dist_supermercado", "area_zindustrial", "area_supermercado", "area_zconstruction", "area_zinstitutional", "area_zeducation")
hector_clean <- hector_clean %>%
  select(all_of(vars_clean_hector))

#Pegarlo a la base limpia de Jose
data <- data %>%
  left_join(hector_clean)

#Por comparar la variable de Hector, distancia a el Dorado únicamente (más adelante)
coords_aeropuerto <- data.frame(lon = -74.14690, lat = 4.70159)
aeropuerto <- coords_aeropuerto %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

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

#Bibliotecas
library_bog_points <- retrieve_amenities(bbox_bog, "amenity", "library")
sp_data$dist_library <- nearest_amenity(sp_data, library_bog_points)

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

#Universidades 
uni_bog_points <- retrieve_amenities(bbox_bog, "amenity", "university", "polygons")
sp_data$dist_university <- nearest_amenity(sp_data, uni_bog_points)

#Estaciones de bomberos
fire_bog_points <- retrieve_amenities(bbox_bog, "amenity", "fire_station", "polygons")
sp_data$dist_firest <- nearest_amenity(sp_data, fire_bog_points)

# Otras distancias --------------------------------------------------------

#Aeropuerto (sólo El Dorado): Reemplazar la variable de Héctor
dist_eldorado <- st_distance(sp_data, aeropuerto)
sp_data$dist_airport <- dist_eldorado

#Centros de interes 
centros_bog <- st_read("stores/Centros_de_interes_Bog.gpkg")
dist_centros <- st_distance(sp_data, centros_bog) %>%
  as.data.frame()
colnames(dist_centros) <- tolower(paste("dist", centros_bog$Name))

sp_data<- cbind(sp_data, dist_centros)


# Intersección con datos en shp -------------------------------------------

#Zonas de interés turístico
interes_turistico <- st_read("stores/ZITu.shp")%>%
  st_transform(crs = 4326)

sp_data$turistico <- lengths(st_intersects(sp_data, interes_turistico))

#Troncales de transmilenio
transmilenio <- st_read("stores/transmilenio.shp")%>%
  st_transform(crs = 4326)

sp_data$dist_transmilenio <- nearest_amenity(sp_data, transmilenio)

#Estrato
estrato <- st_read("stores/ManzanaEstratificacion.shp")%>%
  st_transform(crs = 3116) %>% select("ESTRATO", "geometry")

sp_data <- sp_data %>%
  st_transform(3116) %>%
  st_join(estrato, join = st_nearest_feature) %>%
  st_transform(4326)

#Suciedad calles
suciedad <- st_read("stores/PCAC.shp")%>%
  st_transform(crs = 4326)

sp_data$dist_suciedad <- nearest_amenity(sp_data, suciedad)

#Alumbrado público
alumbrado <- st_read("stores/Luminarias_UPZ.shp")%>%
  st_transform(crs = 4326) %>%
  select(c(LED, Mh, Na, geometry))

sp_data <- sp_data %>%
  st_join(alumbrado, join = st_nearest_feature) %>%
  st_transform(4326)

#Crimen

crimen <- st_read("stores/DAISCAT.gpkg")%>%
  st_transform(crs = 3116) %>%
  select(-c(CMIUSCAT, CMNOMSCAT, CMMES, CMHVAR, CMHTOTAL, CMLPVAR, CMLPTOTAL, CMHPVAR, CMHPTOTAL,
            CMHAVAR, CMHATOTAL, CMHBVAR, CMHBTOTAL, CMHMVAR, CMHMTOTAL, CMHCVAR, CMHCTOTAL, CMHCEVAR, CMHCETOTAL,
            CMHRVAR, CMHRTOTAL,CMVIVAR, CMVITOTAL,CMDSVAR, CMDSTOTAL, SHAPE_AREA, SHAPE_LEN))

sp_data <- sp_data %>%
  st_transform(3116) %>%
  st_join(crimen, join = st_nearest_feature)%>%
  st_transform(4326)

#Calidad del aire 

aire <- st_read("stores/pm25_promedio_anual.gpkg") %>%
  st_transform(4326) %>%
  filter(ano==2019) %>%
  select(concpm25, geom)

sp_data <- sp_data %>%
  st_join(aire, join = st_nearest_feature) %>%
  st_transform(4326)


# Número de amenities en un radio -----------------------------------------

rows_sp_data <- split(sp_data[, c("property_id","geometry")], 1:nrow(sp_data))

#Restaurantes
n_res <- unlist(lapply(rows_sp_data, function(x) {amenities_radius(x,restaurant_bog_points, radius = 350)}))
sp_data$n_restaurants <- unlist(n_res)

#Parques
n_parks <- unlist(lapply(rows_sp_data, function(x) {amenities_radius(x,parques_bog_points, radius = 350)}))
sp_data$n_parques <- unlist(n_parks)

#Supermercado
n_super <- unlist(lapply(rows_sp_data, function(x) {amenities_radius(x,supermarket_bog_points, radius = 350)}))
sp_data$n_supermercados <- unlist(n_super)


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


# Final dataset -----------------------------------------------------------
saveRDS(sp_data, "stores/Bases Finales/sp_data_final.rds")
saveRDS(dense_dtm_test, "stores/Bases Finales/DTM_test.rds")
saveRDS(dense_dtm_train, "stores/Bases Finales/DTM_train.rds")

