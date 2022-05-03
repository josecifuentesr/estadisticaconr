# Prueba para dos proporciones -> 2 muestras independientes
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
