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