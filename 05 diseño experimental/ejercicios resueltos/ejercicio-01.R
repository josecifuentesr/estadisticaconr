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
View(PlantGrowth)
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

