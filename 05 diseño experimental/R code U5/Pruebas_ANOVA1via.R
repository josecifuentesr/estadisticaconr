
# revisa si es necesario instalar los paquetes
list.of.packages <- c("tidyverse",
                      "ggpubr",
                      "ggstatsplot", 
                      "openintro",
                      "rstatix",
                      "DescTools",
                      "WRS2")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)   # manipulación y visualización de datos  
library(ggpubr)      # gráficos sencillos 
library(ggstatsplot) # gráficos listos para publicar
library(openintro)   # datos de ejemplo
library(rstatix)     # pruebas P/NP y estadísticos con tuberías.  
library(DescTools)   # prueba robusta de Yuen 
library(WRS2)        # tamaño de efecto robustos

# library(openintro)
data(hsb2)
head(hsb2)
# str(hsb2)

data(hsb2)
head(hsb2)
# str(hsb2)

hsb2 %>%
  group_by(prog) %>%
  get_summary_stats(write, 
                    type = "mean_sd")


ggboxplot(hsb2, 
          x = "prog", 
          y = "write",
          orientation = "horizontal")


hsb2 %>% 
  group_by(prog) %>%
  identify_outliers(write)


fit <- lm(write ~ prog, data = hsb2)


ggqqplot(residuals(fit))

shapiro_test(residuals(fit))



plot(fit, 1)


hsb2 %>% 
  levene_test(write ~ prog)


## anova_test(data,       # datos con las variables (o modelo)
##   formula,             # y ~ g, con y numérica, g factor
##   dv,                  # variable dependiente (numérica)
##   between,             # variables entre sujetos (factores; opcional)
##   type = NULL,         # tipo de sumas de cuadrados (1, 2 o 3).
##   effect.size = "ges", # "ges" (eta cuadrado generalizado) o "pes" (parcial)
##   white.adjust = FALSE,...) # corrección de heterocedasticidad (TRUE) para EG
## 

hsb2 %>% 
  anova_test(write ~ prog)


## pairwise_t_test(data,                        # datos con las variables
##                 formula,                     # y ~ g, y numérico, g factor
##                 comparisons = NULL,          # comparaciones planeadas
##                 ref.group = NULL,            # grupo de referencia (i.e. control)
##                 p.adjust.method = "holm",    # ajuste del p-valor
##                 pool.sd = TRUE, ...)         # si asume varianzas iguales

hsb2 %>% pairwise_t_test(write ~ prog, p.adjust.method = "bonferroni", pool.sd=TRUE)



ggbetweenstats(x = prog, y = write, data = hsb2, p.adjust.method = "bonferroni",
               bf.message = FALSE, var.equal = TRUE, 
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

hsb2 %>% 
  welch_anova_test(write ~ prog)


hsb2 %>% pairwise_t_test(write ~ prog, p.adjust.method = "bonferroni", pool.sd=FALSE)


ggbetweenstats(x = prog, y = write, data = hsb2, p.adjust.method = "none",
               bf.message = FALSE, var.equal = FALSE, 
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

data(hsb2)
head(hsb2)
# str(hsb2)

hsb2 %>% 
  group_by(prog) %>%
  get_summary_stats(write, 
                    type = "median_iqr")


hsb2 %>% 
  ggboxplot(x = "prog", y = "write",
            orientation = "horizontal")


## kruskal_test(data,         # datos con las variables
##              formula,      # y ~ g, y numérica, g factor
##              ...)

hsb2 %>% 
  kruskal_test(write ~ prog)


## 
## library(stats)
## kruskal.test(x, g, ...)
## 
## kruskal.test(formula, data, subset, na.action, ...)
## 
## kruskal.test(write ~ prog, data=hsb2)
## 

## kruskal_effsize(data,               # datos con las variables
##                 formula,            # y ~ g, y numérico, g factor
##                 ci = FALSE,         # si calcula IC (con bootstrap)
##                 conf.level = 0.95,  # nivel de confianza
##                 ci.type = "perc",   # norm", "basic", "perc", or "bca"
##                 nboot = 1000)       # réplicas para bootstrap

hsb2 %>% 
  kruskal_effsize(write ~ prog)


hsb2 %>% 
  dunn_test(write ~ prog, 
            p.adjust.method = "bonferroni") 


ggbetweenstats(x = prog, y = write, data = hsb2, type = "np",
          bf.message = FALSE, p.adjust.method = "bonferroni", 
          ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

data(hsb2)
head(hsb2)
# str(hsb2)

hsb2 %>%
  group_by(prog) %>%
  filter(between(write, 
                 quantile(write, 0.1), 
                 quantile(write, 0.9))) %>%
  get_summary_stats(write, 
                    type = "mean_sd")


hsb2 %>%
filter(between(write, 
                 quantile(write, 0.1), 
                 quantile(write, 0.9))) %>%
ggboxplot(x = "prog", 
          y = "write",
          orientation = "horizontal")


hsb2 %>%
  group_by(prog) %>%
    filter(between(write, 
          quantile(write, .1), 
          quantile(write, .9))) %>%
  shapiro_test(write)


hsb2 %>%
  group_by(prog) %>%
    filter(between(write, 
          quantile(write, .1), 
          quantile(write, .9))) %>%
ggqqplot("write", 
         facet.by = "prog")


## # library(WRS2)        # Homoscedasticity assumption not required.
## t1way(formula,         # y ~ g, con y numérica, g factor
##       data,            # datos con las variables (o modelo)
##       tr = 0.2,        # nivel de recorte (20%)
##       alpha = 0.05,    # nivel de significación (5%)
##       nboot = 100)     # réplcicas en bootstrap

t1way(write ~ prog, data = hsb2)


## # library(WRS2)        # Homoscedasticity assumption not required.
## lincon(formula,        # y ~ g, y numérico, g factor
##        data,           # datos con las variables a analizar
##        tr = 0.2,       # nivel de recorte (20%)
##        alpha = 0.05)   # nivel de significación (5%)

lincon(write ~ prog, data = hsb2)


ggbetweenstats(x = prog, y = write, data = hsb2, p.adjust.method = "none", 
               bf.message = FALSE, type = "r", tr = 0.2,
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

data(hsb2)
# head(hsb2)
 
hsb2.long <- hsb2 %>%
   pivot_longer(c(write, read, math), 
                names_to = "test", 
                values_to = "score") 
 
 head(hsb2.long)

hsb2 %>%
  dplyr::select(write, read, math) %>%
  get_summary_stats(type = "mean_sd")




ggboxplot(x = "test", y = "score", 
          data = hsb2.long,
          add = c("mean"), 
          add.params = list(color='red'))


hsb2.long %>%
  group_by(test) %>%
  identify_outliers(score)


hsb2.long %>%
  group_by(test) %>%
  shapiro_test(score)


ggqqplot(hsb2.long, 
         "score", 
         facet.by = "test")


## anova_test(data,       # datos con las variables (o modelo)
##   formula,             # y ~ g, con y numérica, g factor
##   dv,                  # variable dependiente (numérica)
##   wid,                 # identificador del sujeto
##   within,              # variables intra sujetos (factores; opcional)
##   type = NULL,         # tipo de sumas de cuadrados (1, 2 o 3).
##   effect.size = "ges", # "ges" (eta cuadrado generalizado) o "pes" (parcial)
##   ...)

res.aov <- anova_test(data = hsb2.long, dv = score, wid = id, within = test)

res.aov$`Mauchly's Test for Sphericity`


## 
## get_anova_table(x,         # objeto de la clase anova_test
##     correction = "auto" ) #  corrección de los grados de libertad para los
##     # factores intra-sujetos; c("GG", "HF", "none")
## 

get_anova_table(res.aov)

## # library(rstatix)
## pairwise_t_test(data,                        # datos con las variables
##                 formula,                     # y ~ g, y numérico, g factor
##                 comparisons = NULL,          # comparaciones planeadas
##                 ref.group = NULL,            # grupo de referencia (i.e. control)
##                 p.adjust.method = "holm",    # ajuste del p-valor
##                 paired = TRUE, ...)          # si usa muestras relacionadas
## 

hsb2.long %>%
  pairwise_t_test(score ~ test, paired = TRUE, p.adjust.method = "bonferroni")


## 
## t_test(data,                     # datos con las variables
##        formula,                  # y ~g, y numérica, g factor
##        comparisons = NULL,       # grupos de interés para comparar
##        ref.group = NULL,         # grupo de referencia para comparar
##        p.adjust.method = "holm", # ajuste del p-valor
##        # "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
##        paired = TRUE,  ...)     # TRUE para muestras pareadas
## 
## hsb2.long %>%
##   t_test(score ~ test,
##          paired = TRUE,
##          p.adjust.method = "bonferroni")
## 

ggwithinstats(x = test, y = score, data = hsb2.long, type = "p",
               bf.message = FALSE, p.adjust.method = "bonferroni", 
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )


data(hsb2)
# head(hsb2)

hsb2.long <- hsb2 %>%
   pivot_longer(c(write, read, math), 
                names_to = "test", 
                values_to = "score") 
 
 head(hsb2.long)


hsb2.long %>%
  group_by(test) %>%
  get_summary_stats(score, 
                    type = "median_iqr")



ggboxplot(x = "test", y = "score", 
          data = hsb2.long,
          add = c("median"), 
          add.params = list(color='red'))


## friedman_test(data,              # datos con las variables
##               formula, ...)      # y ~ g, y numérico, g factor

hsb2.long %>% 
  friedman_test(score ~ test |id)


## friedman_effsize(data,               # datos con las variables
##                  formula,            # y ~ g, y numérico, g factor
##                  ci = FALSE,         # si IC (por bootstrap)
##                  conf.level = 0.95,  # nivel de confianza
##                  ci.type = "perc",   # tipo de IC
##                  nboot = 1000, ...)  # réplicas para bootstrap
## 

hsb2.long %>% 
  friedman_effsize(score ~ test |id)

## # library(rstatix)
## pairwise_wilcox_test(data,                      # datos con las variables
##                     formula,                    # y ~ g, y numérico, g factor
##                     paired = TRUE,              # si usa muestras relacionadas
##                     comparisons = NULL,         # grupos de interés para comparar
##                     ref.group = NULL,           # grupo de referencia (i.e. control)
##                     p.adjust.method = "holm",   # ajuste del p-valor,
##                     conf.level = 0.95, ...)     # nivel de confianza
## 

hsb2.long %>% 
  pairwise_wilcox_test(score ~ test, paired = TRUE, p.adjust.method = "bonferroni")


ggwithinstats(x = test, y = score, data = hsb2.long, type = "np",
               bf.message = FALSE, p.adjust.method = "bonferroni", 
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )


data(hsb2)
# head(hsb2)
 
hsb2.long <- hsb2 %>%
   dplyr::select(write, read, math, id) %>%
   pivot_longer(!id, 
                names_to = "test", 
                values_to = "score") 
 
 head(hsb2.long)

hsb2 %>%
  dplyr::select(write, read, math) %>%
  filter_all(all_vars(between(., 
             quantile(., .1),
             quantile(., .9)))) %>%
  get_summary_stats(type = "mean_sd")



hsb2.long  %>%
ggboxplot(x = "test", y = "score", 
          add = c("mean"), 
          add.params = list(color='red'))


hsb2.long %>%
  group_by(test) %>%
    filter(between(score, 
          quantile(score, .1), 
          quantile(score, .9))) %>%
  shapiro_test(score)


hsb2.long %>%
  group_by(test) %>%
    filter(between(score, 
          quantile(score, .1), 
          quantile(score, .9))) %>%
ggqqplot("score", 
         facet.by = "test")


## # library(WRS2)      # Homoscedasticity assumption not required.
## rmanova(y,           # variable respuesta
##         groups,      # variable de agrupación
##         blocks,      # variable de bloque
##         tr = 0.2)    #nivel de recorte (20%)


rmanova(y = hsb2.long$score, 
        groups = hsb2.long$test,
        blocks = hsb2.long$id)


## # library(WRS2)        # Homoscedasticity assumption not required.
## rmmcp(y,               # variable respuesta
##       groups,          # variable de agrupación
##       blocks,          # variable de bloque
##       tr = 0.2,        # nivel de recorte (20%)
##       alpha = 0.05)    # nivel de significación (5%)
## 

rmmcp(y = hsb2.long$score, 
      groups = hsb2.long$test,
      blocks = hsb2.long$id)


ggwithinstats(x = test, y = score, data = hsb2.long, p.adjust.method = "none", 
               bf.message = FALSE, type = "r", tr = 0.2,   
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )
