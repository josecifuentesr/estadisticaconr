# Problema 02
twins <- 
  matrix(c(2, 10, 15, 3),
         nrow = 2,
         dimnames = list("Dizygotic" = c("Convicted", "Not convicted"),
                         "Monozygotic" = c("Convicted", "Not convicted")))
twins
# Se aplica una prueba de Fisher para dos muestras independientes
# Exploramos los datos con la función ggbarstats
library(ggstatsplot)
ggbarstats(data = as.data.frame(as.table(twins)),
           x = Dizygotic, y = Monozygotic, counts = Freq, 
           results.subtitle = FALSE, bf.message = FALSE)
#Realizamos la prueba de Fischer correspondiente
library(gmodels)
CrossTable(x = twins,
           prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE, fisher = TRUE, digits = 2)
# Como el valor de p = 0.0005367241 para un 95% de confianza se rechaza H0