#25/05/2025

#Inicio de trabajo

# I- Cargamos las librerias y base de datos

pacman::p_load(tidyverse, # Manipulacion de datos
               car, # Recodificar
               sjPlot, # Tablas y graficos
               sjmisc, # Descriptivos
               kableExtra, # Tablas
               psych, # Bivariados
               corrplot, # Graficos correlación
               broom, 
               gginference,
               ggplot2,
               dplyr,
               stargazer,
               lme4,
               haven) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

load(url("https://datos.gob.cl/dataset/c6983439-49f6-4e71-85fe-e8de6e73dae0/resource/ed81f50c-1c7d-43d9-9083-dfc161e0cd66/download/20240516_enssex_data.rdata"))

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# II- Selección de variables 

test1 <- enssex4 %>% select(region, p5, p267, p285)

# p267 -> pasar valor 99 a NA
# p285 pasar valor 9 a NA

test1$p267[test1$p267 %in% c(99)] <- NA
test1$p285[test1$p285 %in% c(9)] <- NA

test1 <- na.omit(test1)

frq(test1)
a
