# Problema 01
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(WRS2)
library(asbio)
library(emmeans)
library(faraway)
library(ggfortify)

# Determinar si el peso del lote difiere en los grupos de tratamiento
head(PlantGrowth)
# Un factor explicativo -> peso (numerico)
# 3 niveles -> grupos 
# Anova de una via de tres muestras independientes

# Calculo de los descriptivos para cada grupo
PlantGrowth %>%
  group_by(group) %>%
  get_summary_stats(weight, 
                    type = "mean_sd")

# Revisión de valores atípicos
PlantGrowth %>% 
  group_by(group) %>%
  identify_outliers(weight)
# En el grupo trat1 hay un dos outliers. No son extremos
# En caso de tener valores atípicos se puede hacer una ANOVA robusta usando 
# el paquete WRS2

# Normalidad
fit <- lm(weight ~ group, data = PlantGrowth)
ggqqplot(residuals(fit))
shapiro_test(residuals(fit))
# En el gráfico se observan los dos valores extremos mencionados anteriormente. 

# normalidad para cada grupo por separado
PlantGrowth %>%
  group_by(group) %>%
  shapiro_test(weight)
ggqqplot(PlantGrowth, "weight", facet.by = "group")
# Se observa que los datos por grupo son normales
# En caso de que no sean normales se puede aplicar la prueba de Kruskal Wallis 
# (no parametrica)

# homocedasticidad
plot(fit, 1)
# En el gráfico no se observan relaciones evidentes entre los residuos y valores 
# ajustados(medias de cada grupo). Se asume homogeneidad de varianzas

# Se puede hacer lo mismo a través de una prueba de Levene
PlantGrowth %>% 
  levene_test(weight ~ group)
# El valor de p = 0.341 no es significativo. No hay diferencias significativas 
# entre las variaciones de los grupos. Se asume la homogeneidad de los grupos. 

# Realizadas las pruebas se ajusta el modelo
PlantGrowth %>% 
  anova_test(weight ~ group)
# Existe un efecto significativo del grupo sobre el peso p = 0.016
# El efecto es pequeño 0.264 (26%) significa que el 26% del cambio de peso puede 
# explicarse por las condiciones del tratamiento

# Pruebas post hoc para anova unidireccional significativo
# Prueba de Tukey hace comparaciones por pares entre grupos
PlantGrowth %>% 
  pairwise_t_test(weight ~ group, p.adjust.method = "bonferroni", 
                  pool.sd = TRUE)

# Grafico
ggbetweenstats(x = group, y = weight, data = PlantGrowth, 
               p.adjust.method = "bonferroni",
               bf.message = FALSE, var.equal = TRUE, 
               ggsignif.args = list(textsize = 5, tip_length = 0.01)) +
  theme(text = element_text(size = 14), plot.subtitle = element_text(size=18))

# Hay diferencias significativas entre trt1 y trt2 con p = 0.0134
