### Prueba de puntuaciones -> dos muestras relacionadas
### Problema
# Utiliza los datos "anorexia" del paquete MASS que contiene los pesos de 72 chicas antes y después de un tratamiento para combatir la anorexia.
# Para las jóvenes que han recibido el tratamiento de ayuda familiar compara los pesos de antes y después. 

# cargamos los datos y mostramos un resumen
library(MASS)
data(anorexia)
summary(anorexia)

# Los datos del paquete tienen tres niveles
# Treat -> "Cont" (control), "CBT" (cognitivo conductual), "FT" (tratamiento familiar)
# Prewt = peso en libras antes del estudio
# Post = peso en libras después del estudio

anorexia2 <- subset(anorexia, subset = Treat == "FT")
anorexia2$id <- 1:nrow(anorexia2)

anorexia2 %>% 
  dplyr::select(Prewt, Postwt) %>% 
  rstatix::get_summary_stats(type = "mean_sd")

library(dplyr)
library(tidyr)
anorexia2.long <- anorexia2 %>% 
  pivot_longer(c(Prewt, Postwt),
               names_to = "test",
               values_to = "score") %>% 
  arrange(test, id)

head(anorexia2)

library(ggpubr)
ggboxplot(x = "test", y = "score",
          data = anorexia2.long,
          add = c("mean"),
          add.params = list(color='red'))

# No se observan valores atípicos significativos

anorexia2 <- anorexia2 %>% mutate(differences = Postwt - Prewt)
anorexia2 %>% rstatix::identify_outliers(differences)

# No se observan outliers para la diferencia de peso antes y después del tratamiento

# Analizamos si los datos siguen una distribución normal
anorexia2 %>% rstatix::shapiro_test(differences)
ggqqplot(anorexia2, "differences")

# No hay evidencia suficiente para rechazar que la diferencia de peso sigue una distribución normal.
# Sin embargo, el tamaño de la muestra es pequeño n = 17, por tanto, se debe considerar este elemento

## Conclusión: en principio se usarán pruebas parámetricas para evaluar la diferencia de peso
ggstatsplot::ggwithinstats(x = test, y = score, data = anorexia2.long, bf.message = FALSE)

# El tratamiento familiar fue significativo para el aumento de peso
# t(16) = 4.18, p = 0.001, n = 17. El tamaño del efecto fue alto con g = 0.97

# Si existen datos sobre el comportamiento de los outliers para la muestra previa al tratamiento se puede aplicar la prueba robusta

ggstatsplot::ggwithinstats(x = test, y = score, data = anorexia2.long, bf.message = FALSE, type = "r", tr = .2)

# Los resultados son muy similares a los obtenidos anteriormente


## Otra opción paso a paso
# Como se quiere comparar las puntuaciones para dos muestras relacionadas, se utiliza la prueba de t de Student para muestras relacionadas indicando paired = TRUE. 
# La hipótesis nula es que la diferencia de peso no es significativa

# prueba t
rstatix::t_test(score ~ test, data = anorexia2.long, paired = TRUE)


# tamaño del efecto
library(effectsize)
cohens_d(score ~ test, data = anorexia2.long, paired = TRUE)
hedges_g(score ~ test, data = anorexia2.long, paired = TRUE)


# NOTA: para tamaños de muestra pequeños (n<20) la g de Hedges se considera menos sesgada que la d de Cohen.
# Los resultados de la prueba indica que existen diferencias significativas en el peso de las jóvenes luego del tratamiento familiar para combatir la anorexia (t(16)=4.18, p<.001), y el tamaño del efecto es grande (1.015).
