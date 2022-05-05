# Problema 01
library(ggstatsplot)
datos <- data.frame(name = c ("impuestos", "servicios"), count=c(624, 1200-624))
ggpiestats(datos, x=name, counts = count)

# Interpretación
# Se observa x(1) = 1.02; p = 0.17; n = 1200
# No se puede rechazar H0, por tanto, no se observa una diferencia significativa entre las preferencias de los encuestados. 