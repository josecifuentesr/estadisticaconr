### Problema para puntuaciones -> 2 muestras independientes
### Problema
### Comparar el número de goles por partido en la primera división española y en la Bundesliga alemana. 

# Cargamos el paquete de datos
library(WRS2)
data(eurosoccer)
summary(eurosoccer)

# Seleccionamos las ligas que nos interesan y eliminamos las que no interesan
library(dplyr)
SpainGer <- eurosoccer %>%
  filter(League %in% c("Spain", "Germany"))

SpainGer$League <- droplevels(SpainGer$League)
levels(SpainGer$League)

# Resumimos los datos
SpainGer %>%
  group_by(League) %>%
  rstatix::get_summary_stats(GoalsGame,
                             type = "mean_sd")

# Se obtienen 20 observaciones de España y 18 de Alemania.
# También, se obtiene la media y desviación estándar

# La distribución de los datos
library(ggpubr)
ggboxplot(x = "League", y = "GoalsGame",
          data = SpainGer,
          add = c("mean"),
          add.params = list(color = 'red'))


# En el diagrama de caja se observa que la liga española tiene algunos outliers. Por tanto, se usarán métodos robustos para la comparación. 

# Sin valores atípicos (outliers) significativos
library(dplyr)
SpainGer %>% 
  group_by(League) %>% 
  rstatix::identify_outliers(GoalsGame) %>% 
  select(League:Games, GoalsGame:is.extreme)

# Se detectan tres casos de valores atípicos y uno de ellos es extremo

# Homogeneidad de la varianza (variación o dispersión)
SpainGer %>% 
  rstatix::levene_test(GoalsGame ~ League)

# El p valor es de 0.475 no se puede rechazar el supuesto de homogeneidad de varianza

# Los datos deben tener una distribución similar a la normal
# aplicamos el test de Shapiro
SpainGer %>% 
  group_by(League) %>% 
  rstatix::shapiro_test(GoalsGame)

# Se observa que los datos de la liga española se aproximan a una distribución normal
# No ocurre lo mismo con la liga alemana (mirar p-valores)

# Graficamos la información obtenida anteriormente
ggqqplot(SpainGer, 
         x = "GoalsGame",
         facet.by = "League")

# Debido a la presencia de outliers podríamos realizar una prueba robusta para comparar las medias recortadas del número de goles por partido entre España y Alemania. 

ggstatsplot::ggbetweenstats(x = League, y = GoalsGame, data = SpainGer,
                            bf.message = FALSE, type = "r", tr = .2)

# Desde el gráfico se puede observar que no existen diferencias significativas en la media recortada del número de goles por partido en la liga española y alemana. 

# t(16.17) = 0.84, p = 0.41, n = 38

### El mismo procedimiento de forma manual 
library(WRS2)
yuen(GoalsGame ~ League, data = SpainGer)

# La media recortada (al 20%) de goles por partido es similar en ambas ligas (p=.4065).
# NOTA: Por defecto la función utiliza tr=.2. Existen pequeñas diferencias en cómo se realizan los cálculos con la función YuenTTest y la función yuen (que es la que utiliza el gráfico ggstatsbetween), pero los resultados son muy muy similares, no es necesario preocuparse por eso. 

# Para observar el valor de la media recortada se debe ejecutar
library(data.table)
SpainGer %>% 
  group_by(League) %>% 
  filter(between(GoalsGame, 
                 quantile(GoalsGame, 0.1),
                 quantile(GoalsGame, 0.9))) %>% 
  rstatix::get_summary_stats(GoalsGame, type = "mean_sd")

