# SEMANA 2. Diseño unifactorial. Comparación de puntuaciones entre más de 2 muestras.

# Cuando queremos comparar varias muestras independientes pero nuestros datos no siguen una distribución normal (e.g. trabajamos con datos ordinales), podemos la versión no paramétrica del ANOVA que se llama prueba de Kruskall-Wallis.

# La prueba Kruskall-Wallis se utiliza para comparar las distribuciones de más de dos grupos de entidades diferentes. Asume que la variable dependiente es una escala ordinal o de intervalo, y no requiere que los datos sigan una distribución normal ya que es una prueba de sumas de rangos.


library("dplyr") # permite usar pipe
library("rstatix") # permite cargar método kruskal_test
library("openintro") # permite usar los datos de hsb2
data(hsb2)
hsb2 %>%
  kruskal_test(write ~ prog) # aplicación del procedimiento de Kruskal_test

# Para estimar el tamaño del efecto utilizamos la función kruskal_effsize() del paquete rstatix

hsb2 %>%
  kruskal_effsize(write ~ prog)

# Para realizar pruebas de comparaciones múltiples post hoc usamos la prueba de Dunn

hsb2 %>%
  dunn_test(write ~prog,
            p.adjust.method = "bonferroni")