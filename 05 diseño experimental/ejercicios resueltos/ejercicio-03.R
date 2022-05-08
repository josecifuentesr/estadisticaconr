# determinar si las percepciones del atractivo físico se vuelven menos 
# rigurosas por efecto del alcohol
# determinar dependencia del sexo del sujeto


# carga de librerías ------------------------------------------------------

library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(WRS2)
library(asbio)
library(emmeans)
library(faraway)
library(ggfortify)


# vista general de los datos ----------------------------------------------
head(goggles)

# descripción de los datos ------------------------------------------------
# consumo de alcohol por género
goggles %>% 
  group_by(gender, alcohol) %>% 
  get_summary_stats(attractiveness, type = "mean_sd")


# Evaluación de los supuestos ---------------------------------------------

# outliers
goggles %>% 
  group_by(gender, alcohol) %>% 
  identify_outliers(attractiveness)
# Hay dos outliers de género femenino. Uno de ellos es extremo

# gráfico de outliers
ggplot(data = goggles, aes(x = gender, y = attractiveness)) + 
  geom_boxplot(aes(fill = alcohol), width = 0.8) + theme_bw()

# normalidad
goggles %>% 
  group_by(gender, alcohol) %>% 
  shapiro_test(attractiveness)
# Se observa que los datos se distribuyen de forma normal

# grafico de normalidad
ggqqplot(goggles, "attractiveness", ggtheme = theme_bw()) +
  facet_grid(gender ~ alcohol)
# En el gráfico se pueden observar los outliers mencionados anteriormente
# Se observa que los datos se distribuyen normalmente entre grupos

# homocedasticidad
goggles %>% 
  levene_test(attractiveness ~ gender * alcohol)
# se aprecia homocestadicidad en los datos

# Hay un valor extremo. Los datos se distribuyen de manera normal
# Existe homocestadicidad. Se aplica prueba parámetrica de ANOVA

goggles %>% 
  anova_test(attractiveness ~ gender * alcohol)

goggles %>% 
  group_by(gender) %>%
  pairwise_t_test(attractiveness ~ alcohol, p.adjust.method = "holm") 

grouped_ggbetweenstats(data = goggles, x = alcohol, 
                       y = attractiveness, grouping.var = gender,
                       results.subtitle = F, messages = F, var.equal = T, 
                       p.adjust.method = "holm")


# Detectamos un efecto significativo de la variable alcohol (ges = .489)
# también sobre la interacción gender:alcohol sobre el atractivo de las parejas
# conseguidas (ges = .362)

#  Para los hombres, el atractivo de sus parejas de aquellos que 
# tomaron 4 Pintas es significativamente menor que la de los que no han tomado 
# alcohol o han tomado 2 Pintas. En las mujeres no detectamos ninguna relación 
# entre beber alcohol y el atractivo de las parejas que encontraron.

























