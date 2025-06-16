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

attach(casen)

# 1) Ver descriptivos en una única tabla

# Descriptivos variables de nivel 1
casen_desc1 = casen %>% select(nvl_educ, pueblo_indigena,
                              dificultad_conc, comuna)

casen$comuna_factor <- as_factor(casen$comuna)

casen_desc1 %>% ungroup() %>%
  select(-comuna) %>%
  psych::describe() %>%
  select(n, mean, sd, median, min, max, range, median) %>%
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
    N_comunas = n()
  )

casen_desc2 %>%
  select(mean_educ_padres, mean_conectividad, N_comunas) %>%
  psych::describe() %>%
  select(n, mean, sd, median, min, max, range) %>%
  kable(
    caption = "Descriptivos generales de variables de nivel 2",
    digits = 2
  ) %>%
  kable_styling(full_width = TRUE)



# 2) Resultados

# MODELO MULTINIVEL
resultados_1 <- lmer(nvl_educ ~ pueblo_indigena + dificultad_conc + 
                    mean_educ_padres + mean_conectividad + 
                    (1 | comuna), data = casen)

screenreg(resultados_1)

tab_model(resultados_1,
          show.ci = FALSE,
          show.icc = FALSE,
          title = "Modelo multinivel",
          dv.labels = c("Nivel educacional máximo"),
          pred.labels = c("(Intercepto)", 
                          "Pertenencia a pueblo indígena", 
                          "Dificultad para concentrarse", 
                          "Promedio educativo de los padres (comuna)", 
                          "Promedio conectividad (comuna)"))



  # 3) Efecto aleatorio 

# Pendiente aleatoria para pueblo_indigena
reg_al1=lmer(nvl_educ ~ 1 + pueblo_indigena + dificultad_conc + mean_conectividad + 
               mean_educ_padres + ( 1 + pueblo_indigena | comuna), data = casen)


tab_model(resultados_1, reg_al1,
          show.ci = FALSE,
          show.icc = FALSE,
          title = "Comparación de modelos",
          dv.labels = c("Modelo multinivel",
                        "Modelo con pendiente aleatoria"),
          pred.labels = c("(Intercepto)", 
                          "Pertenencia a pueblo indígena", 
                          "Dificultad para concentrarse", 
                          "Promedio educativo de los padres (comuna)", 
                          "Promedio conectividad (comuna)"))

anova(resultados_1, reg_al1)


