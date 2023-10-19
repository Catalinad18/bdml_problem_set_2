#Problem Set 2

#Cargamos paquetes requeridos

library(pacman)
p_load(tidyverse, tidymodels, glmnet, stargazer)

#Cargamos data-frames

file_dir <- this.path::here()
setwd(file_dir)
df <- read_csv("../stores/train.csv")
df_test <- read_csv(("../stores/test.csv"))

#Visualizamos resumen de data-frames para ver si hay NaN's y dónde los hay

glimpse(df)
glimpse(df_test)
sapply(df, function(x) sum(is.na(x)))

#Revisamos distribución de datos en variables que tienen NaN's


distributionSurfaceTotal <- function (surface_totall) {
  N = length(df$surface_total)
  surface_totall <- na.omit(df$surface_total)
  hist( df$surface_total,col = "light blue")
}

distributionSurfaceTotal()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con moda.

distributionSurfaceCovered <- function (surface_coveredd) {
  N = length(df$surface_covered)
  surface_coveredd <- na.omit(df$surface_covered)
  hist( df$surface_covered,col = "light blue")
}

distributionSurfaceCovered()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con moda.

distributionRooms <- function (rooms) {
  N = length(df$rooms)
  rooms <- na.omit(df$rooms)
  hist( df$rooms,col = "light blue")
}

distributionRooms()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con moda.

distributionBathrooms <- function (bathrooms) {
  N = length(df$bathrooms)
  bathrooms <- na.omit(df$bathrooms)
  hist( df$bathrooms,col = "light blue")
}

distributionBathrooms()

#Vemos que hay una distribución asimétrica, echada a la izquierda. Imputaremos con moda.