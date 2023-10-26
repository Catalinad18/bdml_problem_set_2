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

########TRATAMIENTO Y LIMPIEZA DE DATOS

####Verificación variable property_type

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

df <- df %>%
  mutate(property_type = case_when(property_type_2 == "apartamento" ~ 0,
                                   property_type_2 == "Apartamento" ~ 0,
                                   property_type_2 == "Casa" ~ 1,
                                   property_type_2 == "casa" ~ 1)
  )

df_test <- df_test %>%
  mutate(property_type = case_when(property_type_2 == "apartamento" ~ 0,
                                   property_type_2 == "Apartamento" ~ 0,
                                   property_type_2 == "Casa" ~ 1,
                                   property_type_2 == "casa" ~ 1)
  )

####Reemplazo e Imputación NA's en variables de surface

#Detectamos expresiones regulares que cumplan la condición establecida

df <- df %>%
  mutate(area = str_extract(description, "(\\d+|\\w+) mts|(\\d+|\\w+) m2|(\\d+|\\w+) metros|(\\d+|\\w+) metros cuadrados|(\\d+|\\w+) metros 2|(\\d+|\\w+) metros2"))

df_test <- df_test %>%
  mutate(area = str_extract(description, "(\\d+|\\w+) mts|(\\d+|\\w+) m2|(\\d+|\\w+) metros|(\\d+|\\w+) metros cuadrados|(\\d+|\\w+) metros 2|(\\d+|\\w+) metros2"))

#Creamos vectores para cambiar de palabras a números

area_escrita <- c("seis", "siete", "ocho", "nueve|nuebe", "diez|dies", "once|onse", "doce|dose", "trece|trese", "catorce|catorse", "quince|quinse", "dieciseis|diesiseis", "diecisiete|diesisiete", "dieciocho|diesiocho", "diecinueve|diesinueve", "veinte|beinte", "veintiun|beintiun|ventiun|bentiun", "veintidos|ventidos|bentidos|beintidos", "veintitres|ventitres|bentitres|beintitres", "veinticuatro|venticuatro|benticuatro|beinticuatro", "veinticinco|venticinco|beinticinco|benticinco", "veintiseis|ventiseis|beintiseis|bentiseis", "veintisiete|ventisiete|beintisiete|bentisiete", "veintiocho|ventiocho|beintiocho|bentiocho", "veintinueve|ventinueve|beintinueve|bentinueve", "treinta", "treinta y uno", "treinta y dos", "treinta y tres", "treinta y cuatro", "treinta y cinco", "treinta y seis", "treinta y siete", "treinta y ocho", "treinta y nueve", "cuarenta", "cuarenta y uno", "cuarenta y dos", "cuarenta y tres", "cuarenta y cuatro", "cuarenta y cinco", "cuarenta y seis", "cuarenta y siete", "cuarenta y ocho", "cuarenta y nueve", "cincuenta", "cincuenta y uno", "cincuenta y dos", "cincuenta y tres", "cincuenta y cuatro", "cincuenta y cinco", "cincuenta y seis", "cincuenta y siete", "cincuenta y ocho", "cincuenta y nueve", "sesenta", "sesenta y uno", "sesenta y dos", "sesenta y tres", "sesenta y cuatro", "sesenta y cinco", "sesenta y seis", "sesenta y siete", "sesenta y ocho", "sesenta y nueve", "setenta", "setenta y uno", "setenta y dos", "setenta y tres", "setenta y cuatro", "setenta y cinco", "setenta y seis", "setenta y siete", "setenta y ocho", "setenta y nueve", "ochenta", "ochenta y uno", "ochenta y dos", "ochenta y tres", "ochenta y cuatro", "ochenta y cinco", "ochenta y seis", "ochenta y siete", "ochenta y ocho", "ochenta y nueve", "noventa", "noventa y uno", "noventa y dos", "noventa y tres", "noventa y cuatro", "noventa y cinco", "noventa y seis", "noventa y siete", "noventa y ocho", "noventa y nueve", "cien")
area_numerica <- as.character(6:100)

#Reemplazamos palabras por números en los strings detectados

df <- df %>%
  mutate(area = str_replace_all(area, setNames(area_numerica, area_escrita)))

df_test <- df_test %>%
  mutate(area = str_replace_all(area, setNames(area_numerica, area_escrita)))

#Extraemos el número del string anterior

df <- df %>%
  mutate(area = as.integer(str_extract(area, "\\d+")))

df_test <- df_test %>%
  mutate(area = as.integer(str_extract(area, "\\d+")))

##Eliminación datos extraños o errados de área

summary(df$surface_total) %>%
  as.matrix() %>%
  as.data.frame()

#V1
#Min.       16.0000
#1st Qu.    84.0000
#Median    119.0000
#Mean      153.9502
#3rd Qu.   185.0000
#Max.    17137.0000
#NA's    30790.0000

#Hay una distancia enorme entre el máximo y el tercer quintil.

summary(df$surface_covered) %>%
  as.matrix() %>%
  as.data.frame()

#V1
#Min.        2.0000
#1st Qu.    81.0000
#Median    108.0000
#Mean      131.9324
#3rd Qu.   160.0000
#Max.     1336.0000
#NA's    30079.0000

#Hay una distancia enorme entre el máximo y el tercer quintil. Procederemos a eliminar (volver NA), el valor de las áreas más grandes del dataset.

#Revisamos distribuciones

distributionSurfaceTotal <- function (surface_totall) {
  N = length(df$surface_total)
  surface_totall <- na.omit(df$surface_total)
  hist( df$surface_total,col = "light blue")
}

distributionSurfaceTotal()

distributionSurfaceTotalTest <- function (surface_totall) {
  N = length(df_test$surface_total)
  surface_totall <- na.omit(df_test$surface_total)
  hist( df_test$surface_total,col = "light blue")
}

distributionSurfaceTotalTest()

#Vemos que hay uno o más valores que no permiten visualizar bien la distribución.Procederemos a eliminar (volver NA), el valor de las áreas más grandes del dataset. El intervalo establecido será 16 < surface_total < 1500

distributionSurfaceCovered <- function (surface_coveredd) {
  N = length(df$surface_covered)
  surface_coveredd <- na.omit(df$surface_covered)
  hist( df$surface_covered,col = "light blue")
}

distributionSurfaceCovered()

distributionSurfaceCoveredTest <- function (surface_coveredd) {
  N = length(df_test$surface_covered)
  surface_coveredd <- na.omit(df_test$surface_covered)
  hist( df_test$surface_covered,col = "light blue")
}

distributionSurfaceCoveredTest()

#Hay una distancia enorme entre el máximo y el tercer quintil. Procederemos a eliminar (volver NA), el valor de las áreas más grandes del dataset. El intervalo será 16 < surface_covered < 1500. La distribución del dataset test parece mucho más normalizada, aunque sigue estando echada a la izquierda.

#Asignamos valor NA a las observaciones dichas, de acuerdo a los intervalos establecidos previamente.

df <- df %>%
  mutate(area = ifelse(area > 1500| area < 16, NA, area))

df_test <- df_test %>%
  mutate(area = ifelse(area > 1500 | area < 16, NA, area))

#Reemplazamos NA's en surface_total con los valores de la variable area

df$surface_total <- ifelse(is.na(df$surface_total), df$area, df$surface_total)

df_test$surface_total <- ifelse(is.na(df_test$surface_total), df_test$area, df_test$surface_total)

#Reemplazamos NA's en surface_covered con los valores de la variable area

df$surface_covered <- ifelse(is.na(df$surface_covered), df$area, df$surface_covered)

df_test$surface_covered <- ifelse(is.na(df_test$surface_covered), df_test$area, df_test$surface_covered)

#Como la superficie total no puede ser menor a la cubierta, si esto sucede, imputamos la superficie total por el valor de la cubierta

df$surface_total <- ifelse(is.na(df$surface_total) | is.na(df$surface_covered) | df$surface_total < df$surface_covered, df$surface_covered, df$surface_total)

df_test$surface_total <- ifelse(is.na(df_test$surface_total) | is.na(df_test$surface_covered) | df_test$surface_total < df_test$surface_covered, df_test$surface_covered, df_test$surface_total)

#Debido a las distribuciones vistas previamente, vemos que la media no es representativa en ninguno de los dos datasets, para ninguna de las dos variables. Por ello, imputaremos la mediana a los valores faltantes de superficie, luego de todo el proceso anterior.

mediana_sup_cubierta <- median(df$surface_covered, na.rm = TRUE) #108
mediana_sup_total <- median(df$surface_total, na.rm = TRUE) #110

mediana_sup_cubierta_test <- median(df_test$surface_covered, na.rm = TRUE) #118
mediana_sup_total_test <- median(df_test$surface_total, na.rm = TRUE) #118

df <- df %>%
  mutate(surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
         surface_total = replace_na(surface_total, mediana_sup_total),)

df_test <- df_test %>%
  mutate(surface_covered = replace_na(surface_covered, mediana_sup_cubierta_test),
         surface_total = replace_na(surface_total, mediana_sup_total_test),)


###Con las variables de superficie imputadas, podemos calcular el valor por metro cuadrado, y así eliminar outliers o valores errados de la variable dependiente

#Distribución variable dependiente

distributionPrice <- function (pricee) {
  N = length(df$price)
  pricee <- na.omit(df$price)
  hist( df$price,col = "light blue")
}

distributionPrice()

#Posee una distribución asimétrica hacia la izquierda, por lo que conviene hacer una transformación logarítmica. Sin embargo, esto lo dejaremos para un paso posterior.

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

df_test <- df_test %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))

summary(df$precio_por_mt2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

#V1
#Min.         $20,424
#1st Qu.   $3,909,091
#Median    $5,136,986
#Mean      $5,878,355
#3rd Qu.   $6,818,182
#Max.    $187,500,000

#Vemos que hay un valor mínimo de $20,424 y uno máximo de $40,450,000 por metro cuadrado, lo cual, a todas luces es muy bajo para el mínimo, y extraordinariamente alto para el máximo.

#Estableceremos un rango con un mínimo de $600,000 y un máximo de $15,000,000 por metro cuadrado (según estimaciones, por barrio, el metro cuadrado más caro de Bogotá ronda alrededor de $7,145,435, y el más barato, alrededor de $873.138. Por ello dejaremos un poco de espacio para evitar sesgo, pero sí para eliminar valores irreales que puedan perjudicar las predicciones)

df <- df %>%
  filter(between(precio_por_mt2, 600000,  15e6))


#Visualizamos la nueva distribución de la variable dependiente.

p <- ggplot(df, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()

p

#Está mucho más normalizada, y de las observaciones iniciales conservamos alrededor del 95%.

####Reemplazo e Imputación NA's variable bathrooms

#Detección

df <- df %>%
  mutate(bathrooms_info = str_extract(description, "(\\w+|\\d+) baños|(\\w+|\\d+) baos|(\\w+|\\d+) banos|(\\w+|\\d+) lavabos"))

df_test <- df_test %>%
  mutate(bathrooms_info = str_extract(description, "(\\w+|\\d+) baños|(\\w+|\\d+) baos|(\\w+|\\d+) banos|(\\w+|\\d+) lavabos"))

#Vectores transformación números

banos_escritos <- c("uno|un", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "once", "doce", "trece")
banos_numericos <- as.character(1:13)

#Transformación valores numéricos

df <- df %>%
  mutate(bathrooms_info = str_replace_all(bathrooms_info, setNames(banos_numericos, banos_escritos)))

df_test <- df_test %>%
  mutate(bathrooms_info = str_replace_all(bathrooms_info, setNames(banos_numericos, banos_escritos)))

#Extracción valores numéricos

df <- df %>%
  mutate(bathrooms_info = as.integer(str_extract(bathrooms_info, "\\d+")))

df_test <- df_test %>%
  mutate(bathrooms_info = as.integer(str_extract(bathrooms_info, "\\d+")))

##Eliminación outliers y datos leverage.

#Revisamos la distribución

summary(df$bathrooms) %>%
  as.matrix() %>%
  as.data.frame()

#V1
#Min.       1.00000
#1st Qu.    2.00000
#Median     3.00000
#Mean       2.88087
#3rd Qu.    3.00000
#Max.      13.00000
#NA's    9826.00000

distributionBathrooms <- function (bathrooms) {
  N = length(df$bathrooms)
  bathrooms <- na.omit(df$bathrooms)
  hist( df$bathrooms,col = "light blue")
}

distributionBathrooms()

#Vemos que hay una distribución asimétrica y echada a la izquierda. Imputaremos con moda, al tomar valores enteros.

distributionBathroomsTest <- function (bathrooms) {
  N = length(df_test$bathrooms)
  bathrooms <- na.omit(df_test$bathrooms)
  hist( df_test$bathrooms,col = "light blue")
}

distributionBathroomsTest()

#Vemos que hay una distribución asimétrica y echada a la izquierda. Imputaremos con moda, al tomar valores enteros.

#Reemplazamos datos faltantes con datos encontrados en la descripción

df$bathrooms <- ifelse(is.na(df$bathrooms), df$bathrooms_info, df$bathrooms)

df_test$bathrooms <- ifelse(is.na(df_test$bathrooms), df_test$bathrooms_info, df_test$bathrooms)

#Calculamos las modas

df %>%count(bathrooms) %>% head() #Moda es 2

df_test %>%count(bathrooms) %>% head() #Moda es 2

#Imputamos datos faltantes persistentes con moda

df <- df %>%
  mutate(bathrooms = replace_na(bathrooms, 2))

df_test <- df_test %>%
  mutate(bathrooms = replace_na(bathrooms, 2))

#### Creación de variable Piso para apartamentos

df <- df %>%
  mutate(piso_info = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

df_test <- df_test %>%
  mutate(piso_info = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

numeros_escritos <- c("uno|primer|primero", "dos|segund|segundo", "tres|tercer|tercero", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo", "once|undecimo|onceavo", "doce|duodecimo|doceavo|doseavo", "trece|trese|treceavo|treseavo", "catorce|catorse|catorceavo|catorseavo", "quince|quinse|quinceavo|quinseavo")
numeros_numericos <- as.character(1:15)

df <- df %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos, numeros_escritos)))

df_test <- df_test %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos, numeros_escritos)))

df <- df %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

df_test <- df_test %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

#Eliminamos datos extraños de piso

df <- df %>%
  mutate(piso_numerico = ifelse(piso_numerico > 40, NA, piso_numerico))

df_test <- df_test %>%
  mutate(piso_numerico = ifelse(piso_numerico > 40, NA, piso_numerico))

#Imputamos moda para valores faltantes de piso, en caso de que se trate de una casa

df %>%
  filter(property_type_2 == "Apartamento") %>%
  count(piso_numerico)

#La moda es 2

df_test %>%
  filter(property_type_2 == "Apartamento") %>%
  count(piso_numerico)

#La moda es 2

df <- df %>%
  mutate(piso_numerico = replace_na(piso_numerico, 2))

df_test <- df_test %>%
  mutate(piso_numerico = replace_na(piso_numerico, 2))

####La única variable que queda por imputar es Rooms. Es muy ambigua, cuenta con muchos NA's. Veamos su distribución:

summary(df$rooms) %>%
  as.matrix() %>%
  as.data.frame()

distributionRooms <- function (rooms) {
  N = length(df$rooms)
  rooms <- na.omit(df$rooms)
  hist( df$rooms,col = "light blue")
}

distributionRooms()

#Vemos que hay una distribución asimétrica, y aunque más normalizada, con un mínimo de 1 y un máximo de 11, sigue estando echada a la izquierda. Imputaremos con moda, al tomar valores enteros.

distributionRoomsTest <- function (rooms) {
  N = length(df_test$rooms)
  rooms <- na.omit(df_test$rooms)
  hist( df_test$rooms,col = "light blue")
}

distributionRoomsTest()

#Vemos que hay una distribución asimétrica, y aunque más normalizada, sigue estando echada a la izquierda. Imputaremos con moda, al tomar valores enteros. No eliminaremos esta variable pues esto implica quedarnos con casi la mitad de nuestra muestra.

#Calculamos modas

df %>%count(rooms) %>% head() #La moda es 3
df_test %>%count(rooms) %>% head() #La moda es 3

#Imputamos

df <- df %>%
  mutate(rooms = replace_na(rooms, 3))

df_test <- df_test %>%
  mutate(rooms = replace_na(rooms, 3))

#Guardamos bases de datos finales

write_csv(df, "base_datos_limpia_train.csv")

write_csv(df_test, "base_datos_limpia_test.csv")

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