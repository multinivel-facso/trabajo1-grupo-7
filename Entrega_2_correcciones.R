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
               sjlabelled,
               dplyr,
               stargazer,
               lme4,
               haven,
               reghelper,
               texreg, 
               gt) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

load("casen.Rdata") #cargamos base de datos

# 1) Ver descriptivos en una única tabla


# Descriptivos variables de nivel 1
casen_desc1 = casen %>% select(nvl_educ, pueblo_indigena,
                              dificultad_conc, comuna)

casen_desc1$comuna_factor <- as_factor(casen$comuna)

casen_desc1 %>% ungroup() %>%
  select(-comuna, -comuna_factor) %>%
  psych::describe() %>%
  select(n, mean, sd, median, min, max, range, median) %>%
  kable(
    caption = "Descriptivos generales de variables nivel 1",
    digits = 2
  ) %>%
  kable_styling(full_width = TRUE)


# Descriptivos variables de nivel 2
agg_casen=casen %>% group_by(comuna) %>% summarise_all(funs(mean))

casen_desc2 = agg_casen %>% select(mean_educ_padres, mean_conectividad)

casen_desc2 %>% ungroup() %>%
  select(mean_educ_padres, mean_conectividad) %>%
  psych::describe() %>%
  select(n, mean, sd, median, min, max, range, median) %>%
  kable(
    caption = "Descriptivos generales de variables nivel 2",
    digits = 2
  ) %>%
  kable_styling(full_width = TRUE)

# Tabla de descriptivos de la variable cluster
tabla_cluster <- casen_desc1 %>%
  count(comuna_factor, name = "N")

tabla_cluster %>% ungroup() %>%
  summarise(
    Promedio = mean(N),
    Valor_minimo = min(N),
    Valor_maximo = max(N),
    Rango = max(N) - min(N),
    Total_de_comunas = n()
  ) %>%
  kable(caption = "Descriptivos generales de la variable cluster") %>%
  kable_styling(full_width = TRUE) # 335 comunas, min=6 max=1805

# 2) 




