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
agg_casen=casen %>% group_by(comuna) %>% summarise_all(funs(mean))

# Crear modelo con datos agregados
reg_agg<- lm(nvl_educ~mean_educ_padres+mean_conectividad+pueblo_indigena + 
               dificultad_conc, data=agg_casen)

# Comparación de modelos
stargazer(reg,reg_agg, title = "Comparación de modelos",column.labels=c("Individual","Agregado"), type ='text')


#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# Comparar modelos

# Modelo 1 con predictores de nivel 1
results_1 = lmer(nvl_educ ~ 1 + pueblo_indigena + dificultad_conc + (1 | comuna), data = casen)
screenreg(results_1, naive=TRUE)

# Modelo 2 con predictores de nivel 2
results_2 = lmer(nvl_educ ~ 1 + mean_conectividad + mean_educ_padres + (1 | comuna), data = casen)
screenreg(results_2)

# Modelo 3 con predictores de nivel 1 y 2
results_3 = lmer(nvl_educ ~ 1 + pueblo_indigena + dificultad_conc + mean_conectividad + 
                   mean_educ_padres + (1 | comuna), data = casen)
screenreg(results_3)


# Comparación regresión nivel agregado, individual y multinivel
reg_ind=lm(nvl_educ ~ pueblo_indigena + dificultad_conc + mean_conectividad + mean_educ_padres, data=casen)

reg_agg=lm(nvl_educ ~ pueblo_indigena + dificultad_conc + mean_conectividad + mean_educ_padres, data=agg_casen)


screenreg(list(reg_ind, reg_agg, results_3))     


tab_model(
  results_1,
  results_2,
  results_3,
  show.ci = FALSE,
  show.p = TRUE,
  p.style = "stars",
  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),
  use.viewer = FALSE
)
            

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/


  