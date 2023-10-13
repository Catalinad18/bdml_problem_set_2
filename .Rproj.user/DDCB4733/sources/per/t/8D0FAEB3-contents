##############################################################
#       Big Data y Machine Learning                          #
#       Taller 1 - Punto 4                                   #
##############################################################

#-------------------------------------------
# Load packages
pkg <- list("dplyr", "readr", "tidyverse", "rio", "stargazer", "boot")
lapply(pkg, require, character.only = T)
rm(pkg)

# Clean environment
rm(list = ls())
#-------------------------------------------

# Load data ---------------------------------
base <- read_csv("stores/geih_scraped.csv")
set.seed(123)

# Item a ------------------------------------
base <- base[!is.na(base$y_ingLab_m_ha), ]
base <- base[!is.na(base$maxEducLevel), ]

# Regresión unconditional wage gap
base$logwage <- log(base$y_ingLab_m_ha)
base$female <- ifelse(base$sex == 1, 0, 1)
reg1 <- lm(logwage ~ female, data = base)

# Item b -------------------------------------
base$age2 <- (base$age)^2

## FWL
base <- base %>% mutate(femaleResidF = lm(female ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, base)$residuals)
base <- base %>% mutate(WageResidF = lm(logwage ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, base)$residuals)
reg2 <- lm(WageResidF ~ femaleResidF, base)

stargazer(reg1, reg2, type = "latex", digits = 6, covariate.labels = c("Female", "Female Resid"), dep.var.labels = c("Wage", "Wage"), title = "Modelos wage gap", omit = c("Constant"))

## FWL with boostrap
fn <- function(data, index) {
  data <- data %>% mutate(femaleResidF = lm(female ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, data = data)$residuals)
  data <- data %>% mutate(WageResidF = lm(logwage ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, data = data)$residuals)
  coef(lm(WageResidF ~ femaleResidF, data = data, subset = index))[2]
}

boot(base, fn, R = 1000)
