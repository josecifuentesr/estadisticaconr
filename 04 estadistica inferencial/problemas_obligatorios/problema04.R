# Problema 04
library(vcd)
data("Arthritis")
datos <- subset(Arthritis, Treatment=="Treated" & (Improved=="None" | Improved =="Marked"))
head(datos)
# Se eliminan las categorías fantasmas
datos$Improved <- droplevels(datos$Improved)
# aplicamos una prueba t para comparar medias
# observamos si las edades se distribuyen de forma normal
rstatix::shapiro_test(datos$Age)
# Con un p = 0.00127 se puede observar que los datos se distribuyen normalmente
# graficamos los datos
library(ggpubr)
ggqqplot(datos, 
         x = "Age",
         facet.by = "Improved")
# Ahora revisamos la homogeneidad de la varianza
datos %>% 
  rstatix::levene_test(Age ~ Improved)
# con p = 0.0145 se puede indicar que hay homogeneidad de la varianza
# prueba t
t_test(Age ~ Improved, data = datos)
# con un p = 0.187 No se observa diferencia significativa en las medias de las edades