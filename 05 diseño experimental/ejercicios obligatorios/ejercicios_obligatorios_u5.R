# carga de las librerías
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(WRS2)
library(asbio)
library(emmeans)
library(faraway)
library(ggfortify)
data(coagulation)
head(coagulation)


# Problema 01 -------------------------------------------------------------


# Calculo de los descriptivos para cada grupo
coagulation %>%
  group_by(diet) %>%
  get_summary_stats(coag, 
                    type = "mean_sd")
# Revisión de valores atípicos
coagulation %>% 
  group_by(diet) %>%
  identify_outliers(coag)
# En el grupo C se observa un valor atípico extremo. En mismo grupo presenta
# un valor atípico no extremo. El grupo B también presenta un valor atípico
# no extremo

# Normalidad
fit <- lm(coag ~ diet, data = coagulation)
ggqqplot(residuals(fit))
shapiro_test(residuals(fit))
# En el gráfico se observan los valores atípicos mencionados anteriormente

# normalidad para cada grupo por separado
coagulation %>%
  group_by(diet) %>%
  shapiro_test(coag)
ggqqplot(coagulation, "coag", facet.by = "diet")
# Se observa que los datos que los datos por grupo son normales. Con excepción
# del valor atípico del grupo C

# homocedasticidad
plot(fit, 1)
# En el gráfico no se observan relaciones evidentes entre los residuos y valores 
# ajustados(medias de cada grupo). Se asume homogeneidad de varianzas

# Se puede hacer lo mismo a través de una prueba de Levene
coagulation %>% 
  levene_test(coag ~ diet)
# El valor de p = 0.593 no es significativo. No hay diferencias significativas 
# entre las variaciones de los grupos. Se asume la homogeneidad de los grupos. 

# Realizadas las pruebas se ajusta el modelo
coagulation %>% 
  anova_test(coag ~ diet)
# Existe un efecto significativo del grupo sobre la coagulación p = 4.66e-05
# El efecto es 0.671 (67%) significa que el 67% del cambio de coagulación puede 
# explicarse por las condiciones del tratamiento

# Pruebas post hoc para anova unidireccional significativo
# Prueba de Tukey hace comparaciones por pares entre grupos
coagulation %>% 
  pairwise_t_test(coag ~ diet, p.adjust.method = "bonferroni", 
                  pool.sd = TRUE)

# Grafico
ggbetweenstats(x = diet, y = coag, data = coagulation, 
               p.adjust.method = "bonferroni",
               bf.message = FALSE, var.equal = TRUE, 
               ggsignif.args = list(textsize = 5, tip_length = 0.01)) +
  theme(text = element_text(size = 14), plot.subtitle = element_text(size=18))

# Desde el gráfico y la prueba se pueden hacer las siguientes observaciones
# Se observan diferencias significativas entre los grupos: A y B, A y C, By D,
# Cy D. 