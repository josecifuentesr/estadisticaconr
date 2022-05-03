### Pruebas para proporciones -> más de dos muestras relacionadas
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
