# Cargar librerías y base de datos
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
               haven,
               reghelper,
               texreg, 
               gt) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

load("casen.RData")

# Creear variables de nvl 2

#1) Promedio nivel educacional de los padres por comuna
casen = casen %>%  
  group_by(comuna) %>% 
  mutate(mean_educ_padres = mean(nvl_educ_padres, na.rm = TRUE))

#2) Promedio de conectividad por comuna
casen = casen %>%  
  group_by(comuna) %>% 
  mutate(mean_conectividad = mean(conectividad, na.rm = TRUE))


  
# Guardar base de datos
save(casen, file = "~/Desktop/Github/grupo 7 trabajo 1/casen.RData")

