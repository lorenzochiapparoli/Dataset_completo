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
library(StepReg)
#install.packages("StepReg")

Df_fit<- read_excel("Df_medie_chem.xlsx", sheet = 3) 
Df_fit_full <-  read_excel("Marciume_acido_fedele_enz.xlsx", sheet = 6) 

Df_fit$Micro_cat <- factor(Df_fit$Micro_cat)

lmod_fit <- lm(SR_severity ~ 	Gluconic+	Acetico+ Sugar+	EtOH+	G2	+A2	+S2	+E2+
              Cz+Hu+	Mp+	Po+	Pt	+Sv+Td+	Zb +Zh, data = Df_fit)
summary(lmod_fit)
anova(lmod_fit)

mod_step <- step(lmod_fit, direction = "both", k = log(nobs(lmod_fit)))
summary(mod_step)
anova(mod_step)
mod_step$anova

BIC(mod_step)

formula <- SR_severity ~ 	Gluconic+	Acetico+ Sugar+	EtOH+	G2	+A2	+S2	+E2+
  Cz+Hu+	Mp+	Po+	Pt	+Sv+Td+	Zb +Zh
mod_stepwise <- stepwise(formula = formula,
                         data = Df_fit,
                         type = "linear",
                         strategy = "bidirection",
                         metric = c("BIC"))
BIC(mod_stepwise$bidirection$BIC)
plot(mod_stepwise, strategy = "bidirection", process = "overview")
anova(mod_stepwise$bidirection$BIC)

write_xlsx(anova(mod_step), "anova_step.xlsx")

fitted(mod_step) #####

Df_fit$FIT <- fitted(mod_step)

ggplot(Df_fit, aes(x = SR_severity, y = FIT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Observed",
       y = "Estimated") +
  theme_classic()


lm_sign <- aov(SR_severity ~ 	Acetico+ Sugar+	EtOH+A2	+S2	+E2+Cz, data = Df_fit)

lm_sign <- lm(SR_severity ~  Sugar+S2, data = Df_fit)

summary(lm_sign)
anova(lm_sign)

m0 <- lm(SR_severity ~ Gluconic+	Acetico+ Sugar+	EtOH+	G2	+A2	+S2	+E2+
           Cz+Hu+	Mp+	Po+	Pt	+Sv+Td+	Zb +Zh, data = Df_fit)
m1 <- update(m0, . ~ . - Zh)
m2 <- update(m1, . ~ . - Mp)
m3 <- update(m2, . ~ . - G2)
m4 <- update(m3, . ~ . - Gluconic)
m5 <- update(m4, . ~ . - Po)
m6 <- update(m5, . ~ . - Td)
m7 <- update(m6, . ~ . - Sugar)
m8 <- update(m7, . ~ . - A2)
m9 <- update(m8, . ~ . - Pt)
m10 <- update(m9, . ~ . - Hu)
m11 <- update(m10, . ~ . - Sv)
m12 <- update(m11, . ~ . - Zb)
m13 <- update(m12, . ~ . - Cz)


anova(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13)

anova_1 <- anova(lmod_fit, . ~ . - Zh)
anova_2 <-anova(lmod_fit, . ~ . - Zh - Mp)

TSS <- sum((Df_fit$SR_severity - mean(Df_fit$SR_severity))^2)

Var_spiegata <- ((TSS-mod_step$anova$`Resid. Dev`)/TSS)*100
tabella_stepwise <- mod_step$anova
tabella_stepwise$Var_spiegata <- Var_spiegata
tabella_stepwise <- tabella_stepwise %>%
  rename(BIC = AIC)



