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
library(HSAUR)
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


# Problema 02 -------------------------------------------------------------
data(weightgain)
head(weightgain)

# Descripción de los datos
# ganancia de peso por fuente
weightgain %>% 
  group_by(source, type) %>% 
  get_summary_stats(weightgain, type = "mean_sd")

# Evaluación de los supuestos
# outliers
weightgain %>% 
  group_by(source, type) %>% 
  identify_outliers(weightgain)
# El conjunto de datos no presenta valores extremos

# gráfico de outliers (no observados en este conjunto de datos)
ggplot(data = weightgain, aes(x = source, y = weightgain)) + 
  geom_boxplot(aes(fill = source), width = 0.8) + theme_bw()

# normalidad
weightgain %>% 
  group_by(source, type) %>% 
  shapiro_test(weightgain)
# Se observa que los datos se distribuyen de forma normal

# grafico de normalidad
ggqqplot(weightgain, "weightgain", ggtheme = theme_bw()) +
  facet_grid(source ~ type)

# homocedasticidad
weightgain %>% 
  levene_test(weightgain ~ source * type)
# se aprecia homocestadicidad en los datos

# Prueba paramétrica de ANOVA
weightgain %>% 
  anova_test(weightgain ~ source * type)

weightgain %>% 
  group_by(source) %>%
  pairwise_t_test(weightgain ~ type, p.adjust.method = "holm") 

grouped_ggbetweenstats(data = weightgain, x = type, 
                       y = weightgain, grouping.var = source,
                       results.subtitle = F, messages = F, var.equal = T, 
                       p.adjust.method = "holm")
# En los valores de la prueba ANOVA se observa un efecto entre los que consumen
# alta (high) y baja (low) cantidad de carne con p = 0.00494 (valor límite)
# En el gráfico no se observan diferencias significativas con el ajuste de holm


# Problema 03 -------------------------------------------------------------

data("invisibility")
head(invisibility)

invisibility %>%
  group_by(cloak) %>%
  get_summary_stats(mischief1, mischief2, type = "mean_sd")

# Gráfico para el supuesto de linealidad ----------------------------------

ggscatter(invisibility, x = "mischief1", y = "mischief2", color = "cloak", 
          add = "reg.line", size = 3) 
# Vemos que los actos traviesos durante las segundas tres semana aumentan 
# para el grupo con capa

# Comprobación de supuestos
# Homogeneidad de las pendientes de regresión
model <- lm(mischief2 ~ mischief1*cloak, data = invisibility) # con interacción
anova_test(model)

# Normalidad, homogeneidad de varianza y outliers
model <- lm(chief2 ~ chief1 + cloak, data = invisibilty) # sin interacción
library(ggfortify)
autoplot(model) + theme(text = element_text(size = 4) )

# Se cumple el supuesto de homogeneidad de las pendientes de regresión, 
# la interacción no es significativa

# ajuste del modelo
invisibility %>% anova_test(mischief2 ~ mischief1 + cloak)

library(emmeans)
pwc <- invisibility %>% 
  emmeans_test(mischief2 ~ cloak, covariate = mischief1, 
               p.adjust.method = "bonferroni")
pwc

get_emmeans(pwc) # medias ajustadas (marginales)
pwc <- pwc %>% add_xy_position(x = "cloak", fun = "mean_se")

ggline(get_emmeans(pwc), x = "cloak", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs( subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc) ) + theme(text = element_text(size = 8) )

# Se observan diferencias significativas entre los que usaron capa y los que 
# no. 


# Problema 04 -------------------------------------------------------------

# observación y descripción de los datos
data("eggprod")
head(eggprod)
eggprod %>%
  group_by(treat) %>%
  get_summary_stats(eggs, 
                    type = "mean_sd")

table(eggprod[, -1]) # 1 réplica

xtabs(eggs ~ block + treat, 
      data = eggprod)

# Evaluación de los supuestos
# aditividad 
library(asbio)
with(eggprod, tukey.add.test(eggs, treat, block))

# outliers
eggprod %>% 
  group_by(treat) %>% 
  identify_outliers(eggs)

# grafico
ggplot(data = eggprod, aes(x = treat, y = eggs)) + 
  geom_boxplot() + theme_bw()

# normalidad
eggprod %>%
  group_by(treat) %>%
  shapiro_test(eggs)

# grafico
ggqqplot(eggprod, "eggs", ggtheme = theme_bw()) +
  facet_grid(eggprod$treat)

# homocedasticidad
eggprod %>% levene_test(eggs ~ treat)

# Conclusiones
# No existe interacción entre el bloque y el factor principal (tukey.add.test)
# Existe un valor atípico extremo en el tratamiento E bloqque 1
# Los datos siguen una distribución aproximadamente normal a excepción del
# tratamiento E (se observa un outlier extremo)
# Los datos son homocedásticos

# ajuste del modelo
( res.aov <- anova_test(data = eggprod, 
                        formula = eggs ~ block + treat) )
library(emmeans)
fit <- lm(eggs ~ + block + treat, eggprod) 
(pwc <- eggprod %>% 
    emmeans_test(eggs ~ treat, model = fit) )

# grafico
bxp <- ggboxplot( eggprod, x = "treat", y = "eggs", color = "treat")
pwc <- pwc %>% add_xy_position(x = "treat")
bxp + stat_pvalue_manual(pwc, hide.ns = TRUE) + 
  labs( subtitle = get_test_label(res.aov, detailed = T), 
        caption = get_pwc_label(pwc) )

# El término de bloque y del tratamiento son significativos p = 0.045. 
# Nos interesa interpretar el tratamiento, cuyo efectoes moderado/fuerte 
# (g=.64),detectamos que el tratamiento O  produce un rendimiento 
# significativamente menor que la E y F