# Anova dos vías mixto

# Se miden los síntomas de resaca para dos grupos de sujetos
# hijos de padres alcohólicos 
# hijos de padres no alcohólicos (control)
# Se mide en tres ocasiones el nivel de resaca (muestras relacionadas)


# Cargamos las librerías --------------------------------------------------

library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(WRS2)
library(asbio)
library(emmeans)
library(faraway)
library(ggfortify)


# Observamos los datos ----------------------------------------------------

head(hangover)
View(hangover)


# Análisis descriptivos ---------------------------------------------------

hangover %>% 
  group_by(time, group) %>% 
  get_summary_stats(symptoms, type = "mean_sd")

# Evaluación de los supuestos ---------------------------------------------

# outliers
hangover %>% 
  group_by(time, group) %>% 
  identify_outliers(symptoms)

# grafico
ggplot(data = hangover, aes(x = time, y = symptoms)) +
  geom_boxplot(aes(fill = group), width = 0.8) + theme_bw()

# normalidad
hangover %>% 
  group_by(time, group) %>% 
  shapiro_test(symptoms)

# grafico
ggqqplot(hangover, "symptoms", ggtheme = theme_bw()) + 
  facet_grid(time ~ group)

# homogeneidad de varianza y covarianza
hangover %>% 
  group_by(time) %>% 
  levene_test(symptoms ~ group)

box_m(hangover[, "symptoms", drop = FALSE], hangover$group)

# Hay 5 valores atípicos para el grupo alcoholic en la medición 1 y 3
# Las muestras no parecen seguir una distribución normal
# Se observa homogeneidad en la varianza y la covarianza
# El ANOVA es robusto ante la falta de normalidad
# Se aplica la versión parámetrica que es más fácil de interpretar


# Aplicación de ANOVA -----------------------------------------------------
res.aov <- anova_test(data = hangover, 
                      dv = symptoms, wid = id, between = group, within = time )
get_anova_table(res.aov) # aplica corrección automática

# grafico
grouped_ggwithinstats(data = hangover, x = time, y = symptoms, 
                      grouping.var = group,
                      results.subtitle = F, messages = F, var.equal = T, 
                      p.adjust.method = "holm")

# Ningún efecto es estadísticamente significativo para alfa del 5%
# Debido a la presencial de outliers se podría aplicar una prueba robusta