# ANCOVA
# Explicar los valores de estrés post traumatico según los tipos de abuso
# sufrido
# cpa = tipo de abuso físico en escala estandarizada
# psdt = estrés pos-traumático

# Cargamos las librerías y describimos los datos --------------------------

library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(WRS2)
library(asbio)
library(emmeans)
library(faraway)
library(ggfortify)

library(faraway)
data(sexab)
head(sexab)
View(sexab)

sexab %>%
  group_by(csa) %>%
  get_summary_stats(ptsd, cpa, type = "mean_sd")


# Gráfico para el supuesto de linealidad ----------------------------------

ggscatter(sexab, x = "cpa", y = "ptsd", color = "csa", 
          add = "reg.line", size = 3) 

# Vemos que las mujeres con problemas de abuso (Abused) presentan altos niveles
# de ptsd respecto alas mujeres que no han sido abusadas sexualmente en su 
# infancia (NotAbused). Sin embargo, también observamos que aquellas mujeres 
# que sufrieron abuso sexual de niñas también presentaron abuso físico.
# Esto sugiere que el abuso físico es una explicación alternativa a los altos
# valores de ptsd en el grupo deabuso sexual. El ANCOVA nos permite evaluar 
# estas dos explicaciones.


# Comprobación de supuestos -----------------------------------------------

# Homogeneidad de las pendientes de regresión
model <- lm(ptsd ~ cpa*csa, data = sexab) # con interacción
anova_test(model)

# Normalidad, homogeneidad de varianza y outliers
model <- lm(ptsd ~ cpa + csa, data = sexab) # sin interacción
library(ggfortify)
autoplot(model) + theme(text = element_text(size = 4) )

# Se cumple el supuesto de homogeneidad de las pendientes de regresión, 
# la interacción no es significativa
# La observación 24 podría ser influyente. En el ANCOVA es más sencillo evaluar 
# los supuestos a partir de los residuos.


# Ajuste del modelo -------------------------------------------------------

sexab %>% anova_test(ptsd ~ cpa + csa)

library(emmeans)
pwc <- sexab %>% 
  emmeans_test(ptsd ~ csa, covariate = cpa, 
               p.adjust.method = "bonferroni")
pwc

get_emmeans(pwc) # medias ajustadas (marginales)

pwc <- pwc %>% add_xy_position(x = "csa", fun = "mean_se")

ggline(get_emmeans(pwc), x = "csa", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs( subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc) ) + theme(text = element_text(size = 8) )
