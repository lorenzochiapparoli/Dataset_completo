library(tidyr)
library(dplyr)
library(broom)  # per tidy() dei risultati dei test
library(readxl)
library(writexl)
library(ggplot2)
library(bestNormalize)
library(agricolae)  # per LSD test
library(ggrepel)
library(pheatmap)
library(relaimpo) 
library(gt)
library(htmltools)
library(stringr)

Df <- read_excel("Marciume_acido_fedele_enz.xlsx", sheet = 4)


Df_long <- Df %>% 
  filter(!(Trial %in% c("1", "2"))) %>% 
  dplyr::select(Trial, Micro, Group, Combination, Gluconic, Acetic, `G+F`, EtOH, arc_mck_10) %>% 
  pivot_longer(
    cols = c(Gluconic, Acetic, `G+F`, EtOH, arc_mck_10),
    names_to = "Parametro",
    values_to = "Valore"
  )

Df_long_medie <- Df_long %>% 
  group_by(Micro,Group, Combination, Parametro) %>% 
  summarise(media_Valore = mean(Valore, na.rm = TRUE),
            SD = sd(Valore, na.rm = TRUE),
            n = sum(!is.na(Valore)),
            SE = SD / sqrt(n)
  )

anova_sev <- aov(sev_mean ~ 	Micro, data = medie_parametro)
summary(anova_sev)

snk <- SNK.test(anova_sev, "Micro", group = TRUE)

####################
################### ORDINE MICRO IN BASE ALLA CLUSTER ANALYSIS 

Df_clu <- read_excel("Marciume_acido_fedele_enz.xlsx", sheet = 5)

Df_clu$Clu_ord <- factor(Df_clu$Clu_ord, levels = Df_clu$Clu_ord)



Df_clu_ord <- Df_clu %>% 
  arrange(factor(Micro, levels = Df_clu$Clu_ord))

#write_xlsx(Df_clu_ord, "Df_medie_per_cluster.xlsx")

#########################################################


############# gRAFICI ############################



boxplots <- list()

for (i in parametri) {
  
  # Filtra il data frame per il composto corrente
  df_sub <- Df_long %>% filter(Parametro == i)
  
  # Crea il boxplot
  p <- ggplot(df_sub, aes(x = Combination, y = Valore)) +
    geom_boxplot()+
    theme_classic() +
    # Aggiunta della media
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 4,      #croce
      size = 3,
      fill = "black"
    )+
    labs(title = paste(i), y = "Concetration (g/L)", x = "") +
    theme(
      axis.title.x = element_text(margin = margin(t = 15)),  # push X title down by 15 pts
      axis.title.y = element_text(margin = margin(r = 15)),  # push Y title left by 15 pts
      axis.text.x = element_text(angle = 30, vjust = 0.9, hjust = 1, margin = margin(t = 10)))
  
  # Salva il grafico nella lista
  boxplots[[i]] <- p
}
boxplots

##### Lettere lsd barplot

Df_long$Parametro <- as.factor(Df_long$Parametro)


lettere_parametro_bar <- data.frame()

# ciclo per ogni Parametro
for(param in unique(Df_long$Parametro)) {
  
  df_sub <- Df_long %>%
    filter(Parametro == param)
  
  # ANOVA
  mod <- aov(Valore ~ Micro, data = df_sub)
  sum_mod <- summary(mod)[[1]][["Pr(>F)"]][1]
  
  cat("\nParametro:", param, "- p-value ANOVA:", sum_mod, "\n")
  
  # LSD test (anche se ANOVA non significativa, opzionale)
  lsd_res <- LSD.test(mod, "Micro", alpha = 0.05, group = TRUE)
  
  # aggiungi risultati in dataframe
  df_lsd <- lsd_res$groups
  df_lsd$Micro <- rownames(df_lsd)
  df_lsd$Parametro <- param
  df_lsd$p.value.aov <- sum_mod
  lettere_parametro_bar <- rbind(lettere_parametro_bar, df_lsd)
}

Df_long_medie$Group <- factor(Df_long_medie$Group, 
    levels = c("C. zemplinina", "H. uvarum", "M. pulcherrima",
               "P. occidentalis", "P. terricola", "S. vini",                 
               "T. delbrueckii", "Z. bailii", "Z. hellenicus", "A. syzygii", "G. oxydans", 
               "G. oxydans + A. syzygii", "NT", "NT AAB", "NT Y"))
Df_long_medie <- Df_long_medie %>% 
  arrange(Group)

lettere_parametro_bar <- lettere_parametro_bar %>%
  mutate(
   Group = str_extract(Micro, "^[^+]+") %>% str_trim()) %>%
  arrange(factor(Group, levels = unique(Df_long_medie$Group)))
Df_long_medie$letters <- lettere_parametro_bar$groups

### Barplot

parametri <- unique(Df_long_medie$Parametro)

# Lista vuota per salvare i grafici
barplots <- list()

# Ciclo for
for (i in parametri) {
  
  # Filtra il data frame per il composto corrente
  df_sub <- Df_long_medie %>% filter(Parametro == i)
  df_sub_lt <- lettere_parametro_bar %>% filter(Parametro == i)
  
  # Crea il boxplot
  p <- ggplot(df_sub, aes(x = Group, y = media_Valore, group = Micro, fill = Combination)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = media_Valore - SE, ymax = media_Valore + SE),
                  position = position_dodge(width = 0.8), width = 0.3) +
    labs(title = paste(i), y = "Concetration (g/L)", x = "") +
    theme_minimal() +
    geom_text_repel(
      aes(label = letters,x = Group, y = media_Valore +SE+ 0.25),
      position = position_dodge(width = 0.8),
      size = 3.9,
      direction = "y"
    )+
    theme(
      axis.title.x = element_text(margin = margin(t = 15)),  # push X title down by 15 pts
      axis.title.y = element_text(margin = margin(r = 15)),  # push Y title left by 15 pts
      axis.text.x = element_text(angle = 30, vjust = 0.9, hjust = 1, margin = margin(t = 10)))+
    scale_fill_brewer(palette = "Set1")
  
  # Salva il grafico nella lista
  barplots[[i]] <- p
}
barplots


lettere_parametro_box <- data.frame()

# ciclo per ogni Parametro
for(param in unique(Df_long$Parametro)) {
  
  df_sub <- Df_long %>%
    filter(Parametro == param)
  
  # ANOVA
  mod <- aov(Valore ~ Combination, data = df_sub)
  sum_mod <- summary(mod)[[1]][["Pr(>F)"]][1]
  
  cat("\nParametro:", param, "- p-value ANOVA:", sum_mod, "\n")
  
  # LSD test (anche se ANOVA non significativa, opzionale)
  lsd_res <- LSD.test(mod, "Combination", alpha = 0.05, group = TRUE)
  
  # aggiungi risultati in dataframe
  df_lsd <- lsd_res$groups
  df_lsd$Combination <- rownames(df_lsd)
  df_lsd$Parametro <- param
  df_lsd$p.value.aov <- sum_mod
  lettere_parametro_box <- rbind(lettere_parametro_box, df_lsd)
}




