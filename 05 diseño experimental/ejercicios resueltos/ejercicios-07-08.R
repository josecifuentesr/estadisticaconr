# Determinar tamaño de una muestra


# Ejercicio 7 -------------------------------------------------------------

# Queremos obtener el tamaño muestral de dos tipos de dieta A y B, para que la 
# diferencia de medias en elpeso de los encuestados sea muA-muB=-10, es decir, 
# la dieta B tenga individuos con 10kg más (en promedio)que los de la dieta A. 
# Asimismo la desviación estándar conjunta es: sqrt(152+1722)= 16.03 
# con un nivel =0.05, y una potencia de 0.8

library(pwr)
pwr.t.test(d=(0 -10) / 16.03, power = .8, sig.level = .05,
           type = "two.sample", alternative = "two.sided")

# Necesitamos 42 sujetos para cada grupo


# Ejercicio 08 ------------------------------------------------------------

# Supongamos que queremos contrastar si la media de la altura de los sujetos del 
# curso de primer año de Estadística es mayor a 172m. Realizamos un estudio 
# piloto en una clase donde obtenemos una media de 174cm de altura, con 
# desviación típica de 10cm. ¿Cuál sería el tamaño muestral necesario para 
# obtener una potencia de 0.80 con un nivel de significación del 5%?

pwr.t.test(d = (174 - 172) / 10, sig.level = .05, power = .8,
           type="one.sample", alternative = "greater")

# La muestra debe ser de 156 personas 