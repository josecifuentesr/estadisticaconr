# Pruebas para proporciones -> 2 muestras relacionadas
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
