datos <- data.frame(name = c ("impuestos", "servicios"), count=c(624, 1200-624))
ggpiestats(datos, x=name, counts = count)
