

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

hsb2 %>%
  group_by(gender, prog) %>%
  get_summary_stats(math, type = "mean_sd")


ggplot(data = hsb2, aes(x = gender, y = math)) + 
  geom_boxplot(aes(fill = prog), width = 0.8) + theme_bw()


hsb2 %>%
  group_by(gender, prog) %>%
  shapiro_test(math)


ggqqplot(hsb2, "math", ggtheme = theme_bw()) +
  facet_grid(gender ~ prog)


hsb2 %>% levene_test(math ~ gender*prog)

hsb2 %>%
  group_by(gender, prog) %>%
  identify_outliers(math)

hsb2 %>% 
  anova_test(math ~ gender * prog)


hsb2 %>%
  pairwise_t_test(math ~ prog,  p.adjust.method = "holm" )


ggbetweenstats(data = hsb2, x = prog, y = math, 
    results.subtitle = FALSE, messages = FALSE,
    var.equal = TRUE, p.adjust.method = "holm", 
  ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

hsb2 %>% 
  group_by(gender) %>%
  pairwise_t_test(math ~ prog, p.adjust.method = "holm") 

grouped_ggbetweenstats(data = hsb2, x = prog, y = math, grouping.var = gender,
  results.subtitle = FALSE, messages = FALSE, var.equal = TRUE, p.adjust.method = "holm", 
  ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

hsb2_long <- hsb2 %>%
  gather(key = "test", value = "score", 
         read, write, math, science, socst) %>%
  convert_as_factor(id, test)

head(hsb2_long)


hsb2_long %>%
  group_by(test, prog) %>%
  get_summary_stats(score, type = "mean_sd")


ggplot(data = hsb2_long, aes(x = test, y = score)) + 
  geom_boxplot(aes(fill = prog), width = 0.8) + theme_bw()


hsb2_long %>%
  group_by(test, prog) %>%
  identify_outliers(score)

hsb2_long %>%
  group_by(test, prog) %>%
  shapiro_test(score)

ggqqplot(hsb2_long, "score", ggtheme = theme_bw()) +
  facet_grid(test ~ prog)

hsb2_long %>%
  group_by(test) %>%
  levene_test(score ~ prog)

box_m(hsb2_long[, "score", drop = FALSE], hsb2_long$prog)

res.aov <- anova_test(data = hsb2_long, 
                      dv = score, wid = id, between = prog, within = test )
get_anova_table(res.aov) # aplica corrección automática

hsb2_long %>%
  group_by(prog) %>%
  pairwise_t_test(score ~ test, paired = TRUE, 
                  p.adjust.method = "holm") %>%
  filter(p.adj < 0.05)

hsb2_long %>% filter(prog == "academic") %>% 
ggwithinstats( x = test, y = score, 
  results.subtitle = FALSE, messages = FALSE, var.equal = TRUE, p.adjust.method = "holm", 
  ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )


hsb2_long %>%
  pairwise_t_test(score ~ prog, p.adjust.method = "holm")


ggbetweenstats(data = hsb2_long, x = prog, y = score,   results.subtitle = FALSE, messages = FALSE, var.equal = TRUE, p.adjust.method = "holm", 
  ggsignif.args = list(textsize = 1.5, tip_length = 0.01)) +
  theme(text = element_text(size = 8), plot.subtitle = element_text(size=8) )

set.seed(100)
hsb2$IQ <- rnorm(n=nrow(hsb2), mean =100, sd=15)

hsb2 %>%
  group_by(prog) %>%
  get_summary_stats(math, IQ, type = "mean_sd")


ggscatter(hsb2, x = "IQ", y = "math", color = "prog", add = "reg.line", size = 1) 

model <- lm(math ~ IQ*prog, data = hsb2) # con interacción
anova_test(model)

model <- lm(math ~ IQ + prog, data = hsb2) # sin interacción
library(ggfortify)
autoplot(model) + theme(text = element_text(size = 4) )

hsb2 %>% anova_test(math ~ IQ + prog)


library(emmeans)
pwc <- hsb2 %>% 
  emmeans_test(math ~ prog, covariate = IQ, p.adjust.method = "bonferroni")
pwc

get_emmeans(pwc) # medias ajustadas (marginales)

pwc <- pwc %>% add_xy_position(x = "prog", fun = "mean_se")
ggline(get_emmeans(pwc), x = "prog", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs( subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc) ) + theme(text = element_text(size = 8) )


library(faraway) 
data(penicillin) 
head(penicillin)

xtabs(yield ~ blend + treat, 
      data=penicillin)

ggplot(penicillin, aes(x = treat, y = yield, group = blend, linetype = blend)) +
  geom_line() + theme(legend.position = "top", legend.direction = "horizontal")

library(asbio)
with(penicillin, tukey.add.test(yield, treat, blend))


( res.aov <- anova_test(data = penicillin, 
                        formula = yield ~ blend + treat) )



library(emmeans)
fit <- lm(yield ~ + blend + treat, penicillin) 
(pwc <- penicillin %>% 
        emmeans_test(yield ~ treat, model = fit) )

bxp <- ggboxplot( penicillin, x = "treat", y = "yield", color = "treat")
pwc <- pwc %>% add_xy_position(x = "treat")
bxp + stat_pvalue_manual(pwc) + labs( subtitle = get_test_label(res.aov, detailed = T), 
 caption = get_pwc_label(pwc) )
