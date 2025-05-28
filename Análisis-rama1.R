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
  labs(title = "Top 25 comunas más frecuentes", x = "Comuna", y = "Frecuencia") +
  theme_minimal()

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

