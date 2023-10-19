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

descriptive_statistics <- stargazer(df,
                                    type = "text", min.max = TRUE, mean.sd = TRUE,
                                    nobs = TRUE, median = TRUE, iqr = FALSE,
                                    digits = 1, align = T,
                                    title = "Summary Statistics",
                                    covariate.labels = c("Ingreso por Hora", "Edad", "Sexo", "Clase", "Departamento", "Formalidad", "Máximo Nivel Educativo Alcanzado", "Oficio", "Total de Horas Trabajadas"),
                                    out="outputs/summ_statistics.tex"

ggplot(df, aes(x=surface_total)) + geom_boxplot()

ggplot(df, aes(x=surface_total)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

df$surface_total %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*10