library(tidyr)
library(dplyr)
library(broom)  # per tidy() dei risultati dei test
library(readxl)
library(writexl)
library(ggplot2)
library(openxlsx)
library(emmeans)
library(multcomp)
library(multcompView)
library(agricolae)
library(tibble)
library(car)

Df_fit<- read_excel("Df_medie_chem.xlsx", sheet = 3) 
Df_fit_full <-  read_excel("Marciume_acido_fedele_enz.xlsx", sheet = 6) 

lmod_fit <- lm(SR_severity ~ 	Gluconic+	Acetico+ Sugar+	EtOH+	G2	+A2	+S2	+E2+
              AcGlu+AG2 +Cz+Hu+	Mp+	Po+	Pt	+Sv+Td+	Zb+	Zh, data = Df_fit)
mod_step <- step(lmod_fit)
summary(lmod_fit)
summary(mod_step)
anova(mod_step)
anova(lmod_fit)
#write_xlsx(anova(mod_step), "anova_step.xlsx")

fitted(mod_step)

Df_fit$FIT <- fitted(mod_step)

ggplot(Df_fit, aes(x = SR_severity, y = FIT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Observed",
       y = "Estimated") +
  theme_classic()

lm_sign <- lm(SR_severity ~  Sugar+S2, data = Df_fit)
summary(lm_sign)
anova(lm_sign)

m0 <- lm(SR_severity ~ 1, data = Df_fit)
m1 <- update(m0, . ~ . + Acetico)
m2 <- update(m1, . ~ . + Sugar)
m3 <- update(m2, . ~ . + EtOH)
m4 <- update(m3, . ~ . + Gluconic)
m5 <- update(m4, . ~ . + G2)
m6 <- update(m5, . ~ . + A2)
m7 <- update(m6, . ~ . + S2)
m8 <- update(m7, . ~ . + E2)
m9 <- update(m8, . ~ . + Cz)
m10 <- update(m9, . ~ . + Hu)
m11 <- update(m10, . ~ . + Mp)
m12 <- update(m11, . ~ . + Po)
m13 <- update(m12, . ~ . + Pt)
m14 <- update(m13, . ~ . + Sv)
m15 <- update(m14, . ~ . + Td)
m16 <- update(m15, . ~ . + Zb)
m17 <- update(m16, . ~ . + Zh)

anova(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17)
