#25/05/2025

#Base de trabajo (rama principal):
#Con este script creamos un subset de datos con las variables de interés debido
#a que la base de datos original era muy grande para cargarla y daba errores al
#hacer commit. La nueva base se llama casen y tiene 51776 obs. y 7 variables, y 
#se guardó en la carpeta del trabajo. Se pude cargar con el siguiente código: 
#load("casen.Rdata")

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
               haven,
               reghelper,
               texreg) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

casen2022 <- read_sav("Base de datos Casen 2022 SPSS_18 marzo 2024.sav") 
casen2022_c <- read_sav("Base de datos provincia y comuna Casen 2022 SPSS.sav")
#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# II- Selección de variables - 153.015 obs. of 12 variables

casen2022_full <- merge(casen2022, 
                        casen2022_c[, c("folio", "id_persona", "comuna")], 
                        by = c("folio", "id_persona"), 
                        all.x = TRUE)

rec_casen <- casen2022_full %>% 
  filter(edad >= 20) %>% select(educ, r3, h7d, area, r12a, r12b,
         r17a, r17b, r17c, r17d, r17e, comuna)

# Filtramos por personas con 20 años o más, ya que a 
# ellos se les presentaban todas las opciones de nivel educacional

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# III- Recodificación de variables y creación de escalas

# 1) Nivel educacional máximo (variable dependiente) (0-8)
rec_casen <- rec_casen %>%
  mutate(nvl_educ = case_when(
    educ == -88 ~ NA_real_,
    educ == 0 ~ 0,
    educ == 1 ~ 1,
    educ == 2 ~ 2,
    educ %in% c(3, 4) ~ 3,
    educ %in% c(5, 6) ~ 4,
    educ %in% c(7, 9) ~ 5,
    educ %in% c(8, 11) ~ 6,
    educ == 10 ~ 7,
    educ == 12 ~ 8))

# 2) Pertenencia a pueblo indígena (dummy, 1=sí pertenece)
rec_casen <- rec_casen %>% 
  mutate(pueblo_indigena = case_when(
    r3 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) ~ 1,
    r3 == 11 ~ 0))

# 3) Dificultad para recordar o para concentrarse (0-3)
rec_casen <- rec_casen %>%
  mutate(dificultad_conc = case_when(
    h7d == 1 ~ 0,
    h7d == 2 ~ 1,
    h7d == 3 ~ 2,
    h7d == 4 ~ 3))

# 4) Area urbana o rural (1=urbana, 0=rural)
rec_casen <- rec_casen %>%
  mutate(area = case_when(
    area == 1 ~ 1,
    area == 2 ~ 0))

# 5) Nivel educacional de los padres (1-5)
# a) Nivel educacional madre
rec_casen <- rec_casen %>%
  mutate(educ_madre = case_when(
    r12a == 1 ~ 1,
    r12a == 2 ~ 2,
    r12a %in% c(3, 4) ~ 3,
    r12a %in% c(5, 6) ~ 4,
    r12a == 7 ~ 5))

# b) Nivel educacional padre
rec_casen <- rec_casen %>%
  mutate(educ_padre = case_when(
    r12b == 1 ~ 1,
    r12b == 2 ~ 2,
    r12b %in% c(3, 4) ~ 3,
    r12b %in% c(5, 6) ~ 4,
    r12b == 7 ~ 5))

# c) Construir el promedio
rec_casen <- rec_casen %>%
  mutate(nvl_educ_padres = rowMeans(
    select(., educ_padre, educ_madre)))

# 6) Índice de conectividad
# a) Recodificar variables
rec_casen <- rec_casen %>%
  mutate(r17a = case_when(
    r17a == 1 ~ 1,
    r17a == 2 ~ 0))

rec_casen <- rec_casen %>%
  mutate(r17b = case_when(
    r17b == 1 ~ 1,
    r17b == 2 ~ 0))

rec_casen <- rec_casen %>%
  mutate(r17c = case_when(
    r17c == 1 ~ 1,
    r17c == 2 ~ 0))

rec_casen <- rec_casen %>%
  mutate(r17d = case_when(
    r17d == 1 ~ 1,
    r17d == 2 ~ 0))

rec_casen <- rec_casen %>%
  mutate(r17e = case_when(
    r17e == 1 ~ 1,
    r17e == 2 ~ 0))

# b) Construir escala sumativa
rec_casen$conectividad <- rec_casen$r17a + rec_casen$r17b + rec_casen$r17c +
  rec_casen$r17d + rec_casen$r17e

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# IV- Tratamiento de NA y segunda selección de variables (para reducir el 
# tamaño de la base de datos)

colSums(is.na(rec_casen))

rec_casen <- na.omit(rec_casen) # 51.686 obs.

casen <- rec_casen %>% select(nvl_educ, pueblo_indigena, dificultad_conc,
                              area, nvl_educ_padres, conectividad, comuna)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# V- Correlación Intra Clase (ICC) y modelo nulo

results_0 = lmer(nvl_educ ~ 1 + (1 | comuna), data = casen)

reghelper::ICC(results_0)


#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# VI- Creear variables de nvl 2

#1) Promedio nivel educacional de los padres por comuna
casen = casen %>%  
  group_by(comuna) %>% 
  mutate(mean_educ_padres = mean(nvl_educ_padres, na.rm = TRUE))

#2) Promedio de conectividad por comuna
casen = casen %>%  
  group_by(comuna) %>% 
  mutate(mean_conectividad = mean(conectividad, na.rm = TRUE))

# VII- Guardar base de datos 
save(casen, file = "~/Desktop/Github/grupo 7 trabajo 1/casen.RData")






