# Ejercicios resueltos unidad 4
# Estos son los ejercicios que se encuentran en el documento PDF de la unidad

###############################################################################

# Prueba para dos proporciones
# 2 muestras independientes
# Problema: Una mujer británica aficionada a tomar té con leche dice poder distinguir cuando se agrega primero a una taza la leche o el té. Para comprobarlo se le dan 8 tazas de té en donde 4 de ellas se agregó la leche primero. ¿Es cierta su afirmación de la mujer?

# Los datos son los siguientes:

TeaTesting <- matrix(c(3, 1, 1, 3),
                     nrow = 2,
                     dimnames = list(Guess = c("Milk", "Tea"),
                                     Truth = c("Milk", "Tea")))
TeaTesting

# Debido a que se tiene proporciones y bajas frecuencias se usará la prueba de Fischer
# La hipótesis nula consiste en que no hay asociación entre adivinar y la certeza.

# Para la hipótesis nula consideramos OR = 1, que representa que no hay ningún efecto, por tanto, las probabilidades de que ocurra el evento 1 o el evento 2 son exactamente las mismas. Para la hipótesis alternativa consideramos OR != 1

# Presentamos los datos en un gráfico con la función *ggbarstats*

library(ggstatsplot)
ggbarstats(data = as.data.frame(as.table(TeaTesting)),
           x = Guess, y = Truth, counts = Freq, 
           results.subtitle = FALSE, bf.message = FALSE)

# La función ggbarstats() no indica los resultados de la prueba de Fischer. En esta oportunidad se usó solo para explorar los datos.  # nolint

# En el gráfico se puede observar que la mujer acertó al 75% de las tazas donde se sirvió la leche primero y al 75% donde se sirvió primero el Té.  # nolint

# Analizaremos si estos resultados son estadísticamente significativos aplicando la prueba de Fischer. 

library(gmodels)
CrossTable(x = TeaTesting,
           prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE, fisher = TRUE, digits = 2)

# Como el valor de p = 0.4857143 para un 95% de confianza se puede concluir que la afirmación de la mujer no es cierta. 

# NOTA: también podríamos haber considerado la hipótesis alternativa donde OR>1 (la mujer acierta con mayor probabilidad que por puro azar), en cuyo caso el resultado es el mismo,no es cierta la afirmación de la mujer.

###############################################################################
## Prueba para proporciones: más de dos muestras independientes
## Se quiere comprobar si el voto en EEUU está determinado por el sexo. Para esto, se obtienen los resultados de una encuesta con intención de voto. Los resultados son los siguientes:

M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
dimnames(M) <- list(genero = c("Hombres", "Mujeres"),
                    partido = c("Democrata", "Independiente", "Republicano"))
M

# Pasamos los datos a un dataframe para visualizarlos con la función ggbarstats
library(ggplot2)
library(ggstatsplot)
ggbarstats(data = as.data.frame(M),
           x = genero, y = partido, counts = Freq, bf.message = FALSE)

# Como se quiere comparar la proporción de más de dos muestras independientes se ocupa la prueba Chi Cuadrado.
# Se observa que existe una relación significativa entre el sexo y el tipo de voto X(2) = 30.07 , p < .0001, n = 2757$

# Para evaluar si existen diferencias significativas en el porcentaje de mujeres que votan a cada partido político, debemos realizar comparaciones múltiples pareadas
library(rstatix)
pairwise_prop_test(M)

# Se observan diferencias significativas en el grupo Republicanos respecto a los demás, donde las mujeres representan un mayor número de votantes.
# A continuación, resolveremos el mismo problema a través de un método manual
# Obtenemos la tabla de frecuencias y aplicamos la prueba Chi Cuadrado
library(gmodels)
CrossTable(M, expected = TRUE,
           prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE, chisq = TRUE, digits = 2)
# Existe una relación significativa entre el sexo y el tipo de voto (X2(2)=30.07, p<.0001)

# Estimamos el grado de asociación de las variables usando la V de Cramer
library(vcd)
assocstats(M)

# Se observa que existe una relación débil con un V=0.104

###############################################################################
# Pruebas para proporiciones: 2 muestras relacionadas
# Problema
# Supone que eres un diseñador web y quieres mejorar la capacidad de conversión de una web cambiando la estética del botón "COMPRAR". Analizas, para período de tiempo determinado, cuántos clicks en "COMPRAR" se han realizado antes y después del cambio.
# Los datos son los siguientes

datos <- matrix( c(6, 2, 8, 4), ncol = 2, byrow = T,
                 dimnames = list(Version_antigua = c("Si", "No"),
                                 Version_nueva = c("Si", "No")))
datos

# Como la prueba consiste en analizar las proporciones para dos muestras relacionadas usaremos el test de Mc Nemar.
# Utilizaremos la función ggbarstats() para obtener los resultados de la prueba y el gráfico de los datos.
as.data.frame(as.table(datos)) # los datos en lista larga se aprecian mejor
# graficamos la información
library(ggplot2)
library(ggstatsplot)
ggbarstats(data = as.data.frame(as.table(datos)),
           x = Version_antigua, y = Version_nueva, counts = Freq, paired = TRUE,
           bf.message = FALSE)
# Para un nivel de confianza del 95%, no encontramos evidencia de que existan diferencias significativas en el número de compras realizada antes y después del cambio en la web. VAlor de p = 0.06 Es decir, **el nuevo diseño web no ha sido efectivo para mejorar las ventas.**

## opción manual
library(gmodels)
CrossTable(datos,
           prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, mcnemar = TRUE)
# Para un nivel de confianza del 95%, no existan diferencias significativas en el número de compras realizada antes y después del cambio en la web.
# NOTA: como el p-valor de la prueba está cercano al .05 (p=0.05777957) algunos investigadores indican en estos casos que existe una "significación marginal"

###############################################################################
### Pruebas para proporciones: más de dos muestras relacionadas ###
### Problema
# Un profesor desea evaluar si sus alumnos han mejorado en el tiempo, para esto compara a los aprobados (1) con los reprobados (0) en tres exámenes sucesivos.
# Los datos son los siguientes

examen1 <- c(0, 1, 1, 0, 1, 1, 1, 0, 0, 0)
examen2 <- c(0, 1, 1, 0, 1, 0, 1, 1, 0, 0)
examen3 <- c(1, 1, 0, 1, 1, 1, 1, 0, 0, 1)
examenes <- data.frame(examen1, examen2, examen3)
examenes

# El encabezado del data frame
head(examenes)

# Nos preguntamos si la proporción de aprobados ha cambiado (H1) o no (H0/hipótesis nula). Como hay tres muestras relacionadas, se está analizando el mismo grupo de estudiantes, se aplicará la prueba de Q de Cochran.

# Pasaremos los datos de formato ancho a formato largo. Antes agregamos una nueva con el ID para cada estudiante
examenes$ID <-1:10 #agrega la columna con el ID
library(tidyr)
ex_long <- pivot_longer(data=examenes, cols = -c ("ID"),
                        names_to = "examen", values_to = "resultado")
ex_long

# Exploramos los datos con la función ggbarstats
library(ggstatsplot)
ggbarstats(data = ex_long, x = resultado, y = examen, paired = TRUE, bf.message = FALSE)

# Se observa que en el primer y en el segundo examen hay igual estudiantes de alumnos aprobados y reprobados 50%. En el tercer examen hay 70% de aprobados y 30% de reprobados
# Analizaremos si estas diferencias son significativas aplicando la prueba Q de Cochran

rstatix::cochran_qtest(ex_long, resultado ~ examen | ID)
# No existen diferencias significativas en el porcentaje de aprobados en cada exámen (X2(2)=1.33, p=.513), los alumnos no han mejorado. Se acepta H0.

###############################################################################
### Problema para puntuaciones: 2 muestras independientes
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


















































































