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
               knitr,
               sjlabelled,
               dplyr,
               stargazer,
               lme4,
               haven,
               reghelper,
               texreg, 
               gt,
               ggeffects,
               labelled) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

load("casen.Rdata") #cargamos base de datos

# 1) Ver descriptivos en una única tabla

# Descriptivos variables de nivel 1
casen_desc1 = casen %>% select(nvl_educ, pueblo_indigena,
                              dificultad_conc, comuna)

casen$comuna_factor <- as_factor(casen$comuna)

casen_desc1 %>% ungroup() %>%
  select(-comuna) %>%
  psych::describe() %>%
  select(n, mean, sd, median, min, max, range) %>%
  kable(
    caption = "Descriptivos generales de variables nivel 1",
    digits = 2
  ) %>%
  kable_styling(full_width = TRUE)


# Tabla de descriptivos de las variables de nivel 2
casen_desc2 <- casen %>%
  group_by(comuna) %>%
  summarise(
    mean_educ_padres = mean(mean_educ_padres),
    mean_conectividad = mean(mean_conectividad),
    comunas = n()
  )

casen_desc2 %>%
  select(mean_educ_padres, mean_conectividad, comunas) %>%
  psych::describe() %>%
  select(n, mean, sd, median, min, max, range) %>%
  kable(
    caption = "Descriptivos generales de variables de nivel 2",
    digits = 2
  ) %>%
  kable_styling(full_width = TRUE)


# 2) Centrado
# Centrado de la variable comprometida en la interacción
casen <- casen %>% ungroup() %>%
  mutate(mean_educ_padres_gmc = mean_educ_padres-mean(mean_educ_padres))

mg_educ_padres <- mean(casen$mean_educ_padres) # 2.24 = media general del nivel educacional de los padres

# Centrado de la variable mean_conectividad
casen <- casen %>% ungroup() %>%
  mutate(mean_conectividad_gmc = mean_conectividad-mean(mean_conectividad))

mg_conectividad <- mean(casen$mean_conectividad) # 2.22 = med

# mg = media general de las variables

# 3) Resultados

# 1) Modelo nulo
modelo_nulo = lmer(nvl_educ ~ 1 + (1 | comuna_factor), data = casen)

screenreg(modelo_nulo)

# 2) Modelo con variables de nivel 1
resultados_1 = lmer(nvl_educ ~ pueblo_indigena + dificultad_conc + (1 | comuna_factor), data = casen)

# 3) Modelo con variables de nivel 2
resultados_2 = lmer(nvl_educ ~ mean_conectividad_gmc + mean_educ_padres_gmc + (1 | comuna_factor), data = casen)

# 4) Modelo con variables de nivel 1 y 2
resultados_3 <- lmer(nvl_educ ~ pueblo_indigena + dificultad_conc + 
                       mean_conectividad_gmc + mean_educ_padres_gmc + 
                    (1 | comuna_factor), data = casen)

# 5) Pendiente aleatoria (+ test de devianza)
# Pendiente aleatoria para pueblo_indigena
reg_al1=lmer(nvl_educ ~ 1 + pueblo_indigena + dificultad_conc + mean_conectividad_gmc + 
               mean_educ_padres_gmc + ( 1 + pueblo_indigena | comuna_factor), data = casen)

# Comparar modelo con y sin pendiente aleatoria
tab_model(resultados_3, reg_al1,
          show.ci = FALSE,
          show.icc = FALSE,
          show.dev = TRUE,
          title = "Comparación de modelos",
          dv.labels = c("Modelo multinivel",
                        "Modelo con pendiente aleatoria"),
          pred.labels = c("(Intercepto)", 
                          "Pertenencia a pueblo indígena", 
                          "Dificultad para concentrarse", 
                          "Promedio educativo de los padres (comuna)", 
                          "Promedio conectividad (comuna)"))

# Test de devianza
anova(resultados_3, reg_al1)

# Graficar pendiente aleatoria

graf1=ggpredict(reg_al1, terms = c("pueblo_indigena","comuna_factor [sample=9]"), type="random")

plot(graf1) # esto ponerlo dsp en el análisis separado de cada modelo

# Poner varios gráficos para mostrar cómo varía por comuna

# 6) Interacción entre niveles
reg_int <- lmer(nvl_educ ~ pueblo_indigena*mean_educ_padres_gmc + dificultad_conc + 
                  mean_conectividad_gmc + (1 + pueblo_indigena | comuna_factor), data = casen)

plot_model(reg_int, type = "int") # esto ponerlo dsp en el análisis separado de cada modelo


# CÓDIGO PARA COMPARAR MODELOS
tab_model(modelo_nulo, resultados_1, resultados_2, resultados_3, reg_al1, reg_int,
          show.ci = FALSE,
          show.icc = FALSE,
          title = "Comparación de modelos",
          dv.labels = c("Modelo Nulo", 
                        "Modelo con variables de nivel 1", 
                        "Modelo con variables de nivel 2", 
                        "Modelo multinivel",
                        "Modelo con pendiente aleatoria",
                        "Modelo con interacción entre niveles"),
          pred.labels = c("(Intercepto)", 
                          "Pertenencia a pueblo indígena", 
                          "Dificultad para concentrarse", 
                          "Promedio educativo de los padres (comuna)", 
                          "Promedio conectividad (comuna)",
                          "Interacción (pueblo_indigena:mean_educ_padres)"))


################################################################################

#Analisis de casos influyentes

#Primer paso

dcook <- influence(resultados_3, "comuna_label")

# ¿lo hacemos con el modelo multinivel o con el modelo con pendiente aleatoria?

#Distancia de cook

cooks.distance(dcook, sort = TRUE)

cut_dcook <- 4/51686

cut_dcook

plot(dcook, which="cook",
     cutoff=(.0000773904), sort=TRUE,
     xlab="Cooks Distance",
     ylab="ID Comuna")

# ESTE NOS DA NULL, NO SABEMOS SI ES PORQUE NO TENEMOS CASOS INFLUYENTES O PORQUE LO HICIMOS MAL
# AYUDA####
sigtest(dcook, test=-1.96)$structure[1:335,] 


