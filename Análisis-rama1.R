#Rama de trabajo - análisis

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
               texreg, 
               gt) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

load("casen.Rdata") #cargamos base de datos

names(casen)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
#Analisis descriptivo

#funcion para ver la moda
moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#Vaiables: nvl_educ, pueblo_indigena, dificultad_conc, area, nvl_educ_padres, conectividad, ypchautcor

#variable dependiente --> nivel educacional

desc1 <- casen %>% 
  summarise("Media" = mean(nvl_educ),
            "Mediana" = median(nvl_educ),
            "Cuartil 1" = quantile(nvl_educ, probs = .25),
            "Cuartil 3" = quantile(nvl_educ, probs = .75),
            "Rango" = max(nvl_educ) - min(nvl_educ),
            "Desviacion estandar" = sd(nvl_educ),
            "Varianza" = var(nvl_educ),
            "Moda" = moda(casen$nvl_educ))

tabla1 <- kableExtra::kbl(desc1, escape=F, full_width = F, caption = "Tabla 1: Estadísticos descriptivos nivel educacional")  %>%
  kable_paper("hover") %>%
  kableExtra::kable_classic(full_width = F, font_size = 14) %>% kable_minimal() %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla1

casen <- casen %>%
  mutate(nvl_educ_label = case_when(
    nvl_educ == 0 ~ "Sin educación",
    nvl_educ == 1 ~ "Básica incompleta",
    nvl_educ == 2 ~ "Básica completa",
    nvl_educ == 3 ~ "Media incompleta",
    nvl_educ == 4 ~ "Media completa",
    nvl_educ == 5 ~ "Superior incompleta",
    nvl_educ == 6 ~ "Superior completa",
    nvl_educ == 7 ~ "Postgrado incompleta",
    nvl_educ == 8 ~ "Postgrado completa",
    FALSE ~ "Otro / No especificado"
  ))

casen <- casen %>%
  mutate(nvl_educ_label = factor(nvl_educ_label,
                                 levels = c(
                                   "Sin educación",
                                   "Básica incompleta",
                                   "Básica completa",
                                   "Media incompleta",
                                   "Media completa",
                                   "Superior incompleta",
                                   "Superior completa",
                                   "Postgrado incompleta",
                                   "Postgrado completa"
                                 )))

desc1.2 <- casen %>%
  group_by(nvl_educ_label) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / sum(n)) * 100, 2))

tabla1.2 <- desc1.2 %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    col.names = c("Nivel máximo alcanzado", "n", "Proporción (%)"),
                    caption = "Tabla 1.2: Nivel máximo alcanzado") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla1.2

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
#variables independientes nivel 1 

#pueblos indigenas

desc2 <- casen %>%
  group_by(pueblo_indigena) %>% # agrupamos 
  summarise(n = n()) %>% # contamos por categ de respuesta
  mutate(prop = round((n / sum(n)) * 100, 2)) # porcentaje

tabla2 <- desc2 %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    col.names = c("Pertenencia pueblo indigena (1=si)", "n", "Proporción (%)"),
                    caption = "Tabla 2: Pertenencia pueblo indigena") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla2


#Dificultad para concentrase
desc3 <- casen %>% 
  summarise("Media" = mean(dificultad_conc),
            "Mediana" = median(dificultad_conc),
            "Cuartil 1" = quantile(dificultad_conc, probs = .25),
            "Cuartil 3" = quantile(dificultad_conc, probs = .75),
            "Rango" = max(dificultad_conc) - min(dificultad_conc),
            "Desviacion estandar" = sd(dificultad_conc),
            "Varianza" = var(dificultad_conc),
            "Moda" = moda(casen$dificultad_conc))

tabla3 <- kableExtra::kbl(desc3, escape=F, full_width = F, caption = "Tabla 3: Estadísticos descriptivos de Dificultad de concentración")  %>%
  kable_paper("hover") %>%
  kableExtra::kable_classic(full_width = F, font_size = 14) %>% kable_minimal() %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla3

# Cargar paquetes necesarios
library(dplyr)
library(gt)

tabla_apa_gt <- casen %>%
  dplyr::count(dificultad_conc) %>%
  dplyr::mutate(
    Porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  dplyr::rename(
    Categoría = dificultad_conc,
    Frecuencia = n,
    `%` = Porcentaje
  ) %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Tabla 3.2.** Frecuencias y porcentajes de dificultad de concentración"),
    subtitle = "Encuesta CASEN 2022"
  ) %>%
  gt::fmt_number(columns = `%`, decimals = 2) %>%
  gt::tab_source_note("Fuente: Elaboración propia con base en CASEN 2022") %>%
  gt::cols_align(align = "center", columns = gt::everything())

tabla_apa_gt

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
#variables de nivel 2

#Área - rural / urbana
desc4 <- casen %>%
  group_by(area) %>% # agrupamos 
  summarise(n = n()) %>% # contamos por categ de respuesta
  mutate(prop = round((n / sum(n)) * 100, 2)) # porcentaje

tabla4 <- desc4 %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    col.names = c("Sector rural o urbano (1=urbano)", "n", "Proporción (%)"),
                    caption = "Tabla 4: Sector rural o urbano") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla4


#Maximo nivel educacional padres (promedio)
desc5 <- casen %>% 
  summarise("Media" = mean(nvl_educ_padres),
            "Mediana" = median(nvl_educ_padres),
            "Cuartil 1" = quantile(nvl_educ_padres, probs = .25),
            "Cuartil 3" = quantile(nvl_educ_padres, probs = .75),
            "Rango" = max(nvl_educ_padres) - min(nvl_educ_padres),
            "Desviacion estandar" = sd(nvl_educ_padres),
            "Varianza" = var(nvl_educ_padres),
            "Moda" = moda(casen$nvl_educ_padres))

tabla5 <- kableExtra::kbl(desc5, escape=F, full_width = F, caption = "Tabla 5: Estadísticos descriptivos Maximo nivel educacional alcanzado padres (promedio)")  %>%
  kable_paper("hover") %>%
  kableExtra::kable_classic(full_width = F, font_size = 14) %>% kable_minimal() %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla5

frq(casen$nvl_educ_padres)

casen <- casen %>%
  mutate(nvl_educ_padres_label = case_when(
    nvl_educ_padres == 1.00 ~ "Nivel muy bajo",
    nvl_educ_padres == 1.50 ~ "Nivel bajo-muy bajo",
    nvl_educ_padres == 2.00 ~ "Nivel bajo",
    nvl_educ_padres == 2.50 ~ "Nivel medio-bajo",
    nvl_educ_padres == 3.00 ~ "Nivel medio",
    nvl_educ_padres == 3.50 ~ "Nivel medio-alto",
    nvl_educ_padres == 4.00 ~ "Nivel alto",
    nvl_educ_padres == 4.50 ~ "Nivel alto-muy alto",
    nvl_educ_padres == 5.00 ~ "Nivel muy alto"))

casen <- casen %>%
  mutate(nvl_educ_padres_label = factor(nvl_educ_padres_label,
                                        levels = c(
                                          "Nivel muy bajo",
                                          "Nivel bajo-muy bajo",
                                          "Nivel bajo",
                                          "Nivel medio-bajo",
                                          "Nivel medio",
                                          "Nivel medio-alto",
                                          "Nivel alto",
                                          "Nivel alto-muy alto",
                                          "Nivel muy alto"
                                        )))

desc5.1 <- casen %>%
  group_by(nvl_educ_padres_label) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / sum(n)) * 100, 2))

tabla5.1 <- desc5.1 %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    col.names = c("Nivel educacional", "n", "Proporción (%)"),
                    caption = "Tabla 5.1: Nivel educacional máximo alcanzado por los padres") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla5.1


#escala conectividad
desc6 <- casen %>% 
  summarise("Media" = mean(conectividad),
            "Mediana" = median(conectividad),
            "Cuartil 1" = quantile(conectividad, probs = .25),
            "Cuartil 3" = quantile(conectividad, probs = .75),
            "Rango" = max(conectividad) - min(conectividad),
            "Desviacion estandar" = sd(conectividad),
            "Varianza" = var(conectividad),
            "Moda" = moda(casen$conectividad))

tabla6 <- kableExtra::kbl(desc6, escape=F, full_width = F, caption = "Tabla 6: Estadísticos descriptivos Nivel de conectividad")  %>%
  kable_paper("hover") %>%
  kableExtra::kable_classic(full_width = F, font_size = 14) %>% kable_minimal() %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla6

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
#variable de anidación : comuna

# Paso 1: Agrupar por comuna y contar
casen <- casen %>%
  mutate(comuna_label = as_factor(comuna))

comuna_freq <- casen %>%
  count(comuna_label) %>%
  mutate(porcentaje = n / sum(n) * 100)


frq(casen$comuna_label)


# Calcular frecuencias y ordenar
freq_comunas <- casen %>%
  count(comuna_label, sort = TRUE)

top_25 <- freq_comunas %>%
  slice_max(n, n = 25)

ggplot(top_25, aes(x = reorder(comuna_label, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Tabla 7: Top 25 comunas más frecuentes", x = "Comuna", y = "Frecuencia") +
  theme_minimal()

comuna_freq <- casen %>%
  group_by(comuna) %>%
  summarise(frecuencia = n())

desc7 <- comuna_freq %>% 
  summarise(
    Media = mean(frecuencia),
    Mediana = median(frecuencia),
    `Cuartil 1` = quantile(frecuencia, probs = .25),
    `Cuartil 3` = quantile(frecuencia, probs = .75),
    Rango = max(frecuencia) - min(frecuencia),
    `Desviación estándar` = sd(frecuencia),
    Varianza = var(frecuencia),
    Moda = moda(frecuencia)  # asegúrate de tener la función `moda()` definida
  )

tabla7.1 <- kableExtra::kbl(desc7, escape = FALSE, full_width = FALSE,
                            caption = "Tabla 7.1: Estadísticos descriptivos de la frecuencia de comunas") %>%
  kableExtra::kable_classic(full_width = FALSE, font_size = 14) %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")

tabla7.1

  
#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# Modelo nulo (sin predictores de nivel 1)
reg_mlm0 <- lmer(nvl_educ ~ 1 + (1 | comuna), data = casen)

# Modelo con predictores de nivel 1
reg_mlm1 <- lmer(nvl_educ ~ 1 + dificultad_conc + pueblo_indigena + (1 | comuna), data = casen)

# Guardar las predicciones del modelo con efectos aleatorios por comuna
casen$pred_mlm1 <- predict(reg_mlm1)

# Graficar relación entre dificultad para concentrarse y nivel educativo predicho, por comuna
casen %>%
  ggplot(aes(x = dificultad_conc, y = pred_mlm1, color = as.factor(comuna), group = comuna)) +
  geom_smooth(se = FALSE, method = "lm") +
  labs(
    title = "Relación entre dificultad para concentrarse y nivel educativo por comuna",
    x = "Dificultad para concentrarse",
    y = "Nivel educacional predicho",
    color = "Comuna"
  ) +
  theme_minimal() +
  theme(legend.position = "corner")  # Oculta la leyenda para no saturar el gráfico

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# Estimación correlación intraclase (ICC) -------------------------------------

# Modelo nulo (sin predictores), solo el intercepto aleatorio por comuna
modelo_nulo <- lmer(nvl_educ ~ 1 + (1 | comuna), data = casen)

# Resumen del modelo
summary(modelo_nulo)

# Visualización con texreg
screenreg(modelo_nulo)  # requiere library(texreg)

# Cálculo del ICC
reghelper::ICC(modelo_nulo)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

#-------------------------------
# COMPARACIÓN: REGRESIONES INDIVIDUAL, AGREGADA Y MULTINIVEL
#-------------------------------

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
#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/



