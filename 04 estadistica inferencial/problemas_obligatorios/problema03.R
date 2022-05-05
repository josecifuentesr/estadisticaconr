# Problema 03
# Cargamos los datos
data(infert, package = "datasets")
# Construimos la tabla de frecuencias
tab <- table(infert$education, infert$induced)
# hacemos las comparaciones entre columnas
library(rstatix)
pairwise_prop_test(tab[, c(1,3)], p.adjust.method = "bonferroni")
# En esta comparaciónhay diferencia significativa entre 0 y 5 años
pairwise_prop_test(tab[, c(1,2)], p.adjust.method = "bonferroni")
# En esta comparación no hay diferencias significativas
pairwise_prop_test(tab[, c(2,3)], p.adjust.method = "bonferroni")
# En esta comparación no hay diferencias significativas

# Aplicamos las pruebas múltiples de Fisher ya que las frecuencias son bajas
library(RVAideMemoire)
fisher.multcomp(tab)
# Se obtienen resultados muy similares a los anteriores con la mismas conclusiones