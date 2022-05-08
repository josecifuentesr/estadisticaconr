

library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(WRS2)
library(asbio)
library(emmeans)
library(faraway)
library(ggfortify)


#-------------------------------- Ejercicio 1 --------------------------------#
head(PlantGrowth)

# decriptivos
PlantGrowth %>%
  group_by(group) %>%
  get_summary_stats(weight, 
                    type = "mean_sd")


# outliers
PlantGrowth %>% 
  group_by(group) %>%
  identify_outliers(weight)


# normalidad
fit <- lm(weight ~ group, data = PlantGrowth)
ggqqplot(residuals(fit))
shapiro_test(residuals(fit))


# normalidad
PlantGrowth %>%
  group_by(group) %>%
  shapiro_test(weight)
ggqqplot(PlantGrowth, "weight", facet.by = "group")


# homocedasticidad
plot(fit, 1)

# homocedasticidad
PlantGrowth %>% 
  levene_test(weight ~ group)


PlantGrowth %>% 
  anova_test(weight ~ group)


PlantGrowth %>% pairwise_t_test(weight ~ group, 
                                p.adjust.method = "bonferroni", pool.sd=TRUE)


ggbetweenstats(x = group, y = weight, data = PlantGrowth, 
               p.adjust.method = "bonferroni",
               bf.message = FALSE, var.equal = TRUE, 
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )


#-------------------------------- Ejercicio 2 --------------------------------#
head(bush)
 
bush.long <- bush %>%
   pivot_longer(c(stick_insect, kangaroo_testicle, fish_eye, witchetty_grub), 
                names_to = "test", 
                values_to = "score") 
 
 head(bush.long)

bush %>%
  dplyr::select(stick_insect, kangaroo_testicle, fish_eye, witchetty_grub) %>%
  get_summary_stats(type = "mean_sd")



# outliers
bush.long %>%
  group_by(test) %>%
  identify_outliers(score)

# normalidad
bush.long %>%
  group_by(test) %>%
  shapiro_test(score)

ggqqplot(bush.long, 
         "score", 
         facet.by = "test")

# esfericidad
res.aov <- anova_test(data = bush.long, dv = score, 
                      wid = participant, within = test)

res.aov$`Mauchly's Test for Sphericity`


get_anova_table(res.aov)

bush.long %>%
  pairwise_t_test(score ~ test, paired = TRUE, p.adjust.method = "bonferroni")

ggwithinstats(x = test, y = score, data = bush.long, type = "p",
               bf.message = FALSE, p.adjust.method = "bonferroni", 
               ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )


#-------------------------------- Ejercicio 3 --------------------------------#
head(goggles)

goggles %>%
  group_by(gender, alcohol) %>%
  get_summary_stats(attractiveness, type = "mean_sd")


# outliers
goggles %>%
  group_by(gender, alcohol) %>%
  identify_outliers(attractiveness)

ggplot(data = goggles, aes(x = gender, y = attractiveness)) + 
  geom_boxplot(aes(fill = alcohol), width = 0.8) + theme_bw()

# normalidad
goggles %>%
  group_by(gender, alcohol) %>%
  shapiro_test(attractiveness)

ggqqplot(goggles, "attractiveness", ggtheme = theme_bw()) +
  facet_grid(gender ~ alcohol)

# homocedasticidad
goggles %>% levene_test(attractiveness ~ gender*alcohol)


goggles %>% 
  anova_test(attractiveness ~ gender * alcohol)

goggles %>% 
  group_by(gender) %>%
  pairwise_t_test(attractiveness ~ alcohol, p.adjust.method = "holm") 

grouped_ggbetweenstats(data = goggles, x = alcohol, 
                       y = attractiveness, grouping.var = gender,
  results.subtitle = F, messages = F, var.equal = T, p.adjust.method = "holm")

#-------------------------------- Ejercicio 4 --------------------------------#
head(hangover)

hangover %>%
  group_by(time, group) %>%
  get_summary_stats(symptoms, type = "mean_sd")



# outliers
hangover %>%
  group_by(time, group) %>%
  identify_outliers(symptoms)

ggplot(data = hangover, aes(x = time, y = symptoms)) + 
  geom_boxplot(aes(fill = group), width = 0.8) + theme_bw()

# normalidad
hangover %>%
  group_by(time, group) %>%
  shapiro_test(symptoms)

ggqqplot(hangover, "symptoms", ggtheme = theme_bw()) +
  facet_grid(time ~ group)

# homogeneidad de varianza y covarianza
hangover %>%
  group_by(time) %>%
  levene_test(symptoms ~ group)

box_m(hangover[, "symptoms", drop = FALSE], hangover$group)


res.aov <- anova_test(data = hangover, 
                      dv = symptoms, wid = id, between = group, within = time )
get_anova_table(res.aov) # aplica corrección automática

grouped_ggwithinstats(data = hangover, x = time, y = symptoms, 
                      grouping.var = group,
  results.subtitle = F, messages = F, var.equal = T, p.adjust.method = "holm")

#-------------------------------- Ejercicio 5 --------------------------------#
head(oatvar)

oatvar %>%
  group_by(variety) %>%
  get_summary_stats(yield, 
                    type = "mean_sd")

table(oatvar[,-1]) # 1 réplica
xtabs(yield ~ block + variety, 
      data=oatvar)

# aditividad
library(asbio)
with(oatvar, tukey.add.test(yield, variety, block))

# outliers
oatvar %>%
  group_by(variety) %>%
  identify_outliers(yield)

ggplot(data = oatvar, aes(x = variety, y = yield)) + 
  geom_boxplot() + theme_bw()

# normalidad
oatvar %>%
  group_by(variety) %>%
  shapiro_test(yield)

ggqqplot(oatvar, "yield", ggtheme = theme_bw()) +
  facet_grid(oatvar$variety)

# homocedasticidad
oatvar %>% levene_test(yield ~ variety)


( res.aov <- anova_test(data = oatvar, 
                        formula = yield ~ block + variety) )

library(emmeans)
fit <- lm(yield ~ + block + variety, oatvar) 
(pwc <- oatvar %>% 
        emmeans_test(yield ~ variety, model = fit) )

bxp <- ggboxplot( oatvar, x = "variety", y = "yield", color = "variety")
pwc <- pwc %>% add_xy_position(x = "variety")
bxp + stat_pvalue_manual(pwc, hide.ns = TRUE) + 
  labs( subtitle = get_test_label(res.aov, detailed = T), 
 caption = get_pwc_label(pwc) )

#-------------------------------- Ejercicio 6 --------------------------------#
library(faraway)
data(sexab)
head(sexab)

sexab %>%
  group_by(csa) %>%
  get_summary_stats(ptsd, cpa, type = "mean_sd")


ggscatter(sexab, x = "cpa", y = "ptsd", color = "csa", 
          add = "reg.line", size = 1) 


# Homogeneidad de las pendientes de regresión
model <- lm(ptsd ~ cpa*csa, data = sexab) # con interacción
anova_test(model)

# Normalidad, homogeneidad de varianza y outliers
model <- lm(ptsd ~ cpa + csa, data = sexab) # sin interacción
library(ggfortify)
autoplot(model) + theme(text = element_text(size = 4) )


sexab %>% anova_test(ptsd ~ cpa + csa)

library(emmeans)
pwc <- sexab %>% 
  emmeans_test(ptsd ~ csa, covariate = cpa, p.adjust.method = "bonferroni")
pwc

get_emmeans(pwc) # medias ajustadas (marginales)

pwc <- pwc %>% add_xy_position(x = "csa", fun = "mean_se")
ggline(get_emmeans(pwc), x = "csa", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs( subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc) ) + theme(text = element_text(size = 8) )


#-------------------------------- Ejercicios 7 y 8 ---------------------------#
library(pwr) 
pwr.t.test(d=(0-10)/16.03,power=.8,sig.level=.05,
           type="two.sample", alternative="two.sided")

pwr.t.test(d = (174-172)/10, sig.level=0.05, power=0.80, 
           type="one.sample" , alternative="greater")
