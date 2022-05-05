
# revisa si es necesario instalar los paquetes
list.of.packages <- c("tidyverse",
                      "ggpubr",
                      "ggstatsplot", 
                      "pwr",
                      "WebPower",
                      "rstatix",
                      "WRS2")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)   # manipulación y visualización de datos  
library(ggpubr)      # gráficos sencillos 
library(ggstatsplot) # gráficos listos para publicar
library(pwr)         # tamaño de muestra y potencia
library(WebPower)    # tamaño de muestra y potencia para modelos más complejos
library(rstatix)     # pruebas P/NP y estadísticos con tuberías.  
library(WRS2)        # tamaño de efecto robustos

## pwr.t.test(n = NULL,            # tamaño de muestra a estimar
##     d = NULL,                   # tamaño del efecto (d de Cohen)
##     sig.level = 0.05,           # nivel de significación (alfa=5%)
##     power = NULL,               # potencia estadística (1-beta=80%)
##     type = c("two.sample", "one.sample", "paired"), # una o 2 muestras
##     alternative = c("two.sided", "less", "greater"))  # uni o bilateral

(out <- pwr.t.test(d = -.5, sig.level = .05, power = .8, 
                   type = "one.sample", alternative = "less"))

plot(out)

## cohen.ES(test = c("p", "t", "r", "anov", "chisq", "f2"),
##     size = c("small", "medium", "large"))
## 

library(pwr)
cohen.ES(test = "r", size = "medium")

## pwr.p.test(h = ,                                           # tamaño de efecto
##            sig.level =,                                    # nivel de significación
##            power =,                                        # potencia
##            alternative=c("two.sided", "less", "greater") ) # tipo de prueba

pwr.p.test(h=0.2, sig.level=0.05, power=0.80, alternative="two.sided")

## pwr.2p.test(h = ,                         # tamaño de efecto
##             sig.level =,                  # nivel de significación
##             power =,                      # potencia
##             alternative=c("two.sided", "less", "greater"))  # tipo de prueba

pwr.2p.test(h=0.2, sig.level=0.05, power=.80, alternative="two.sided")

## pwr.chisq.test(w =,            # tamaño de efecto
##                df = ,          # grados de libertad
##                sig.level =,    # nivel de significación
##                power = )       # potencia

pwr.chisq.test(w = 0.30, df = 6, 
               sig.level = 0.05, power = 0.80)

(dat<-matrix(c(60, 25, 1, 14, 65, 21, 11, 3), ncol=4, byrow=T, 
             dimnames = list(c("M","F"),c("White","Black","Am.Indian","Asian"))))

## rstatix::cramer_v(datos)
## effectsize::phi(datos)	
## pwr::ES.w2(prop.table(datos))

pwr.chisq.test(w = cramer_v(dat), df = (2-1)*(4-1), sig.level = 0.05, power = 0.80)  

( prob <- matrix(c(10,20,40,30), ncol=2,
                 dimnames = list(c("M","F"),c("Usa","No usa"))) )

## pwr.t.test(d = ,                                          # tamaño de efecto
##            sig.level = ,                                  # nivel de significación
##            power = ,                                      # potencia
##            type = c("two.sample", "one.sample", "paired"))  # tipo de prueba
## 

pwr.t.test(d=.50, sig.level=.05, power=.80, 
           type="one.sample", alternative="two.sided")

## pwr.t.test(d = ,                                          # tamaño de efecto
##            sig.level = ,                                  # nivel de significación
##            power = ,                                      # potencia
##            type = c("two.sample", "one.sample", "paired"))  # tipo de prueba

pwr.t.test(d=.5, sig.level=.05, power=.8, type="two.sample", alternative="greater")

## pwr.t.test(d = ,                                          # tamaño de efecto
##            sig.level = ,                                  # nivel de significación
##            power = ,                                      # potencia
##            type = c("two.sample", "one.sample", "paired"))  # tipo de prueba

pwr.t.test(d=0.8, sig.level=0.05, power=0.80, type="paired", alternative="greater")

## pwr.anova.test(k =,            # número de grupos
##                f = ,           # tamaño del efecto
##                sig.level = ,   # nivel de significación
##                power = )       # potencia

pwr.anova.test(k = 6, f = 0.1, sig.level = 0.05, power = 0.80)

datos <- matrix(c(6.3, 9.9, 5.1, 1.0 ,
                  2.8, 4.1, 2.9, 2.8 ,
                  7.8, 3.9, 3.6, 4.8 ,
                  7.9, 6.3, 5.7, 3.9 ,
                  4.9, 6.9, 4.5, 1.6 ), ncol=4, byrow=TRUE)
colnames(datos) <- c("Option1", "Option2", "Option3", "Option4")

## library("WebPower")
## wp.rmanova(ng = NULL,      # número de grupos
##            nm = NULL,      # número de mediciones
##            f = NULL,       # tamaño del efecto
##            nscor = 1,      # coeficiente de corrección de esfericidad
##            alpha = 0.05,   # nivel de significación
##            power = NULL,   # potencia
##            type = c(0,1,2))   # "0" para efecto "entre" (MI);
##     # "1" para efectos "dentro" (MR); y "2" para efectos de interacción.
## 

wp.rmanova(n=NULL, ng=1, nm=4, f=0.1, nscor=1, alpha=0.05, power=0.80, type=1)

datos <- matrix(c(38, 38, 46, 52, 13, 44, 15, 29, 32, 35, 53, 60, 35, 48, 
                  51, 44, 21, 27, 29, 36), ncol=4, byrow=TRUE)
colnames(datos) <- c( "6 months", "12 months", "18 months", "24 months")


x<-pwr.t.test(d=.5,sig.level=.05,power=.80,type="one.sample",alternative="greater")
round(x$n*1.15,0)   # redondeo el resultado de pwr.t.test + 15%   

x<-pwr.t.test(d=.2,sig.level=.05,power=.80,type="two.sample",alternative="two.sided")
round(x$n*1.15,0)     # redondeo el resultado de pwr.t.test + 15%   

x<-pwr.t.test(d=.8,sig.level=.05,power=.80,type="paired",alternative="greater")
round(x$n*1.15,0)    # redondeo el resultado de pwr.t.test + 15%   


x <- pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, power = 0.80)
round(x$n * 1.15, 2)    # redondeo el resultado de pwr.anova.test + 15%   

x <- wp.rmanova(n=NULL, ng=1, nm=3, f=0.25, nscor=1, alpha=0.05, power=0.80, type=1)
round(x$n * 1.15, 2)    # redondeo el resultado de pwr.anova.test + 15%   

datos <- matrix(c(38, 38, 46, 52, 13, 44, 15, 29, 32, 35, 53, 60, 35, 48, 
                  51, 44, 21, 27, 29, 36), ncol=4, byrow=TRUE)
colnames(datos) <- c( "6 months", "12 months", "18 months", "24 months")


## wp.kanova(ndf = NULL,     # grados de libertad del numerador
##           f = NULL,       # tamaño del efecto
##           ng = NULL,      # nñumero de grupos
##           alpha = 0.05,   # nivel de significación
##           power = NULL)   # potencia
## 

wp.kanova(ndf = 2, f = 0.25, ng = 12, alpha = 0.05, power = 0.80)

## wp.kanova(ndf = NULL,     # grados de libertad del numerador
##           f = NULL,       # tamaño del efecto
##           ng = NULL,      # nñumero de grupos
##           alpha = 0.05,   # nivel de significación
##           power = NULL)   # potencia
## 

wp.kanova(ndf = 8, f = 0.10, ng = 15, alpha = 0.05, power = 0.80) 

datos<-matrix(c(-6.4, 8.7, -3.1, 1.3, -6.0, 6.8, -2.0, -4.3, -1.2, -8.2, -6.3,
-6.5, 3.6, 1.3, 2.4, 1.5, 1.3, 1.1, 7.9, -1, -1.5, 3.9, -1.9, 1.3, 2.5, -8.2,-9.7), 
  ncol=3, byrow=T, dimnames = list(c("c", "a", "e", "c", "a", "e", "c", "a", "e"), 
                                   c("TratA", "TratB", "TratC")) )
datos

wp.kanova(ndf = 16, f = 0.10, ng = 45, alpha = 0.05, power = 0.80) 
