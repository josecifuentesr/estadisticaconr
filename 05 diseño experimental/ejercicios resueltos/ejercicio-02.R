# ANOVA de 1 VIA IS
head(bush)
# Pasamos los datos de formato ancho a largo
bush.long <- bush %>% 
  pivot_longer(c(stick_insect, kangaroo_testicle, fish_eye, witchetty_grub),
               names_to = "test", values_to = "score")
head(bush.long)

# Análisis descriptivo de los datos
bush %>% 
  dplyr::select(stick_insect, kangaroo_testicle, fish_eye, witchetty_grub) %>% 
  get_summary_stats(type = "mean_sd")

# Evaluamos los supuestos 
# outliers
bush.long %>% 
  group_by(test) %>% 
  identify_outliers(score)
# No hay valores extremos

# Normalidad
bush.long %>% 
  group_by(test) %>% 
  shapiro_test(score)
# Los datos por grupo se distribuyen normal

# grafico de normalidad
ggqqplot(bush.long,
         "score", 
         facet.by = "test")

# esfericidad
res.aov <- anova_test(data = bush.long, dv = score, wid = participant, within = test)
res.aov$`Mauchly's Test for Sphericity`
# con p = 0.047 se cumple la esfericidad al límite

# ajustamos el modelo 
# get_anova_table corrige automáticamente para una eventual
# desviación del supuesto de esfericidad
get_anova_table(res.aov)
# efecto bajo de 32,7 %

# aplicación del modelo
bush.long %>%
  pairwise_t_test(score ~ test, paired = TRUE, p.adjust.method = "bonferroni")

# grafico de la prueba
ggwithinstats(x = test, y = score, data = bush.long, type = "p",
              bf.message = FALSE, p.adjust.method = "bonferroni", 
              ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

# Conclusiones
# Efecto siginificativo y pequeño (g = .327)
# fish_eye y kangaroo_testicle tienen tiempos significativamente menores
# que sticky_insect
