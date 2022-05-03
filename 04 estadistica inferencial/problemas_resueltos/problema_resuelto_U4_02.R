## Prueba para proporciones -> más de dos muestras independientes
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
