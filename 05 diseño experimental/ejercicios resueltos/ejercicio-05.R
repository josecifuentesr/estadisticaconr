# ANOVA con bloque
# Se desea comprobar el rendimiento en gramos de 8 variedades de avena
# Las unidades experimentales se agruparon en 5 bloques


# Observación y descripción de los datos ----------------------------------

head(oatvar)
View(oatvar)

oatvar %>%
  group_by(variety) %>%
  get_summary_stats(yield, 
                    type = "mean_sd")

table(oatvar[, -1]) # 1 réplica

xtabs(yield ~ block + variety, 
      data = oatvar)


# Evaluación de los supuestos ---------------------------------------------

# aditividad 
library(asbio)
with(oatvar, tukey.add.test(yield, variety, block))

# outliers
oatvar %>% 
  group_by(variety) %>% 
  identify_outliers(yield)

# grafico
ggplot(data = oatvar, aes(x = variety, y = yield)) + 
  geom_boxplot() + theme_bw()

# normalidad
oatvar %>%
  group_by(variety) %>%
  shapiro_test(yield)

# grafico
ggqqplot(oatvar, "yield", ggtheme = theme_bw()) +
  facet_grid(oatvar$variety)

# homocedasticidad
oatvar %>% levene_test(yield ~ variety)

# Conclusiones
# No existe interacción entre el bloque y el factor principal (tukey.add.test)
# Existe un valor atípico extremo en la variedad 7 bloqque V
# Los datos siguen una distribución aproximadamente normal a excepción de 
# la variedad 7
# Los datos son homocedásticos


# Ajuste del modelo -------------------------------------------------------

( res.aov <- anova_test(data = oatvar, 
                        formula = yield ~ block + variety) )
library(emmeans)
fit <- lm(yield ~ + block + variety, oatvar) 
(pwc <- oatvar %>% 
    emmeans_test(yield ~ variety, model = fit) )

# grafico
bxp <- ggboxplot( oatvar, x = "variety", y = "yield", color = "variety")
pwc <- pwc %>% add_xy_position(x = "variety")
bxp + stat_pvalue_manual(pwc, hide.ns = TRUE) + 
  labs( subtitle = get_test_label(res.aov, detailed = T), 
        caption = get_pwc_label(pwc) )

# El término de bloque y del tratamiento son significativos. 
# Nos interesa interpretar el tratamiento, cuyo efectoes moderado/fuerte 
# (g=.674),detectamos que la variedad 5 produce un rendimiento 
# significativamente mayor que la 1,4,6,7 y 
# la variadad 4 produce un rendimiento significativamente menor que 2,5,8