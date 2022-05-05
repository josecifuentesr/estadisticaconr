
library(agricolae)

library(randomizr)

## simple_ra (N,                    # número de unidades (entero positivo)
##            prob = NULL,          # probabilidad de asignación (longitud 1)
##            prob_unit = NULL,     # probabilidad de asignación (longitud N)
##            prob_each = NULL,     # probabilidades de asignación a cada condición
##            num_arms = NULL,      # niveles del tratamiento
##            condiciones = NULL,   # nombres de los niveles del tratamiento
##            check_inputs = TRUE,  # lógico. predeterminado: TRUE.
##            simple = VERDADERO)   # lógico. solo para uso interno.

simple <- simple_ra(N = 10, 
               num_arms = 2, 
               conditions=c("treatment 1", "placebo"))
head(simple)
table(simple)


completa <- complete_ra(N = 10)
head(completa)
table(completa)


completa2 <- complete_ra(N = 10, m_each = c(3, 2, 5))
table(completa2)

students<-read.csv("http://www.discovr.rocks/csv/students.csv")
summary(students)


students$experiment <- block_ra(blocks = students$group, 
                                conditions = c("Control", "Treatment"),
                                prob_each = c(.5, .5))
head(students)
table(students$experiment, students$group)


## library(agricolae)
## design.crd(trt,         # número de factores y sus niveles
##            r,           # número de réplicas o bloques
##            serie = 2,   # 1: 11, 12; 2: 101,102; 3: 1001,1002
##            seed = 0,    # semilla de aleatoriedad
##            kinds = "Super-Duper",   # método para aleatorizar
##            randomization = TRUE)    # aleatorizar (lógico)
## 

out_CRD<-design.crd(trt = c("A", "B", "C"), 
                    r=c(4, 3, 4), seed=777, serie=0)

head(out_CRD$book)

## library(agricolae)
## design.rcbd(trt,                # tratamientos
##             r,                  # replicaciones o bloques
##             serie = 2,          # numeración de la serie
##             seed = 0,           # semilla de aleatoriedad
##             kinds = "Super-Duper",   # método para aleatorizar
##             first = TRUE,       # aleatorizar la rep 1 (lógico)
##             continue = FALSE,   # numeración continua de la parcela (lógico)
##             randomization = TRUE) # aleatorizar (lógico)
## 

out_RCBD<- design.rcbd(trt=c("A", "B", "C","D","E"),
                       r=4, seed=-513, serie=2)

head(out_RCBD$book)

print(out_RCBD$sketch)
book2<- zigzag(out_RCBD) # zigzag numeration 
print(matrix(book2[,1],byrow = TRUE, ncol = 5))


## design.bib(trt,                    # tratamientos
##            k,                      # tamaño dle bloque
##            r=NULL,                 # replicaciones
##            serie = 2,              # numeración de la serie
##            seed = 0,               # semilla de aleatoriedad
##            kinds = "Super-Duper",  # método para aleatorizar
##            maxRep=20,              # repetición máxima
##            randomization=TRUE)     # aleatorización (lógico)

outdesign <- design.bib(trt = c("A", "B", "C", "D", "E" ), 
                        k=4, seed=543, serie=2)
head(outdesign$book, n=3)


outdesign$statistics
outdesign$parameters

## design.lsd(trt,                   # tratamientos
##            serie = 2,             # numeración de la serie
##            seed = 0,              # semilla de aleatoriedad
##            kinds = "Super-Duper", # método para aleatorizar
##            first=TRUE,            # aleatorizar la rep 1
##            randomization=TRUE)    # aleatorizar (lógico)

out_latinsquare <- design.lsd(trt= c("A","B","C","D"),
                              seed=543, serie=2) 

head(out_latinsquare$book)


print(outdesign$sketch)

book <- zigzag(out_latinsquare) 
print(matrix(book[,1],byrow = TRUE, ncol = 4))


## design.ab(trt,                  # niveles de los factores
##           r = NULL,             # replicaciones o bloques
##           serie = 2,            # numeración de la serie
##           design = c("rcbd", "crd", "lsd"),   # tipo de diseño
##           seed = 0,             # semilla de aleatoriedad
##           kinds = "Super-Duper",  # método para aleatorizar
##           first = TRUE,         # # aleatorizar la rep 1
##           randomization = TRUE) # aleatorizar (lógico)
## 

out_factorial <-design.ab(trt=c(3,2),  
                          r=3, seed=123, serie=2) 

head(out_factorial$book )


zigzag(out_factorial)
