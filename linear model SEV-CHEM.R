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


Df_fit <- read_excel("Df_medie_chem.xlsx", sheet = 2) 

lmod_fit <- lm(SR_severity ~ 	Gluconic+	Acetico+ Sugar+	EtOH+	G2	+A2	+S2	+E2+Cz
               +Hu+	Mp+	Po+	Pt	+Sv+	+Td+	Zb+	Zh+ Acidità_titolabile+	AC2, data = Df_fit)
summary(lmod_fit)

fitted(lmod_fit)

Df_fit$FIT <- fitted(lmod_fit)

plot(Df_fit$FIT, Df_fit$SR_severity)

lm_sign <- lm(SR_severity ~ 	Gluconic+	Acetico+ Sugar+	EtOH+	G2	+A2	+S2	+E2+Cz+ + Acidità_titolabile+	AC2, data = Df_fit)
summary(lm_sign)
