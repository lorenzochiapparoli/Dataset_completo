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

Df <- read_excel("Marciume_acido_fedele_enz.xlsx", sheet = 4) 
Df$Trial <- factor(Df$Trial)
Df$Micro <- factor(Df$Micro)

Df_hca <- Df %>% 
  filter(!(Trial %in% c("1", "2"))) %>% 
  dplyr::select(Trial, Micro, arc_mck_10) %>% 
  group_by(Micro) %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>% 
  ungroup()

Df_hca <- Df_hca %>% 
  column_to_rownames("Micro")

Df_hca$Micro <- NULL

Df_scaled <- scale(Df_hca)
hc <- hclust(dist(Df_scaled)^2, method = "average")
plot(hc)

