# Problema 05
# cargamos los datos y los observamos 
library(MASS)
data(immer)
head(immer)

# seleccionamos las columnas correspondientes
immer2 <- immer %>% 
  dplyr::select("Y1", "Y2")
immer2$id <- 1:nrow(immer2)

# obtenemos un resumen de los datos
immer2 %>% 
  rstatix::get_summary_stats(type = "mean_sd")

# pasamos los datos a formato largo
library(dplyr)
library(tidyr)
immer2.long <- immer2 %>% 
  pivot_longer(c(Y1, Y2),
               names_to = "year",
               values_to = "performance") %>% 
  arrange(year, id)
head(immer2.long)

# graficamos los datos
library(ggpubr)
ggboxplot(x = "year", y = "performance",
          data = immer2.long,
          add = c("mean"),
          add.params = list(color='red'))

# En el primer año hay un valor atípico (no es extremo)
immer2 %>% rstatix::identify_outliers(Y1)
# En el segundo año no hay valores atípicos
immer2 %>% rstatix::identify_outliers(Y2)

# distribución normal de los datos
immer2.long %>% rstatix::shapiro_test(performance)
# es normal con p = 0.0500

# graficamos los datos
library(ggpubr)
ggqqplot(immer2.long, "performance")

# aplicamos la prueba parametrica
ggstatsplot::ggwithinstats(x = year, y = performance, data = immer2.long, bf.message = FALSE)
# Para una prueba t(29) se obtiene p =  0.00241 con n = 30. Se observan diferencias significativa

# prueba parametrica recortada
ggstatsplot::ggwithinstats(x = year, y = performance, data = immer2.long, bf.message = FALSE, type = "r", tr = .2)
# se obtienen resultados a los obtenidos anteriormente