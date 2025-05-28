# Cargar librerías y base de datos
pacman::p_load(tidyverse, # Manipulacion de datos
               car, # Recodificar
               sjPlot, # Tablas y graficos
               sjmisc, # Descriptivos
               sjlabelled,
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

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# Crear modelo con datos individuales
reg <- lm(nvl_educ~mean_educ_padres+mean_conectividad+pueblo_indigena + 
            dificultad_conc, data = casen)

# Crear base de datos colapsada
agg_casen=casen %>% group_by(comuna) %>% summarise_all(funs(mean)) %>% as.data.frame()

# Crear modelo con datos agregados
reg_agg<- lm(nvl_educ~mean_educ_padres+mean_conectividad+pueblo_indigena + 
               dificultad_conc, data=agg_casen)

# Comparación de modelos
stargazer(reg,reg_agg, title = "Comparación de modelos",column.labels=c("Individual","Agregado"), type ='text')


tab_model(reg, reg_agg, show.ci=F, show.se = T, dv.labels = c("Individual", "Agregado"))


#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# Comparar modelos

# Modelo 1 con predictores de nivel 1
results_1 = lmer(nvl_educ ~ 1 + pueblo_indigena + dificultad_conc + (1 | comuna), data = casen)
screenreg(results_1, naive=TRUE)

# Modelo 2 con predictores de nivel 2


#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# Coeficientes aleatorios

reg_casen0=lmer(nvl_educ ~ 1 + ( 1 | comuna), data = casen)
reg_casen1=lmer(nvl_educ ~ 1 + pueblo_indigena + ( 1 | comuna), data = casen)

reg_casen2=lmer(nvl_educ ~ 1 + pueblo_indigena + (1 + pueblo_indigena | comuna), data=casen)
screenreg(list(reg_casen1, reg_casen2), doctype = FALSE) # para ver en la consola utilizar screenreg()



