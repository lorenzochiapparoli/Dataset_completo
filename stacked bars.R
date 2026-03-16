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

Df_sin <- Df %>% 
  filter(Combination %in% c("yeast alone", "bacteria alone")) %>% 
  filter(!(Micro == "G. oxydans + A. syzygii")) %>% 
  arrange(Trial, Micro)

perc_diff <- (Df_sin$arc_mck_10 - Df_sin$arc_mck_5) / Df_sin$arc_mck_5 * 100
Df_sin$perc_diff <- perc_diff

medie_sin <- Df_sin %>% 
  group_by(Trial, Micro, Group, Combination) %>% 
  summarise(
    across(c(Gluconic,
             Acetic,
             `G+F`,
             EtOH,
             pH,
             Acidità_titolabile,
             arc_mck_5,
             arc_mck_10),
           
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             n = ~sum(!is.na(.x)),
             se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
           ),
           
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
  
  
perc_diff_avg <- (medie_sin$arc_mck_10_mean - medie_sin$arc_mck_5_mean) / medie_sin$arc_mck_5_mean * 100
medie_sin$perc_diff <- perc_diff_avg  

medie_sin_t3 <- medie_sin %>% 
  filter(Trial == "3")

ggplot(medie_sin, aes(x = Micro)) +
  geom_col(aes(y = arc_mck_10_mean), fill = "tomato") +
  geom_col(aes(y = arc_mck_5_mean), fill = "lightblue") +
  geom_errorbar(aes(ymin = arc_mck_5_mean - arc_mck_5_se,
                    ymax = arc_mck_5_mean + arc_mck_5_se), width = 0.15)+ 
  geom_text(data = subset(medie_sin, !is.na(perc_diff)),aes(y = arc_mck_5_mean+ arc_mck_5_se, label = paste0("+", round(perc_diff,1), "%")),
            vjust = -0.5, size = 3.5)  +
facet_wrap(~Trial, ncol = 4) +
  labs(y = "Arc_severity", x = "Micro") +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1   # allinea meglio quando si ruota
    ))

ggplot(medie_sin, aes(x = Micro)) +
  geom_col(aes(y = Acetic_mean), fill = "lightblue") +
  geom_errorbar(aes(ymin = Acetic_mean - Acetic_se,
                    ymax = Acetic_mean + Acetic_se), width = 0.15)+ 
  facet_wrap(~Trial, ncol = 4) +
  labs(y = "Acetic (g/L)", x = "Micro") +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1   # allinea meglio quando si ruota
    ))



# variabili da plottare
vars <- c("Gluconic",
          "Acetic",
          "G+F",
          "EtOH",
          "pH",
          "Acidità_titolabile")

# funzione per creare il grafico
plot_bar <- function(var) {
  
  mean_col <- paste0(var, "_mean")
  se_col   <- paste0(var, "_se")
  
  ggplot(medie_sin, aes(x = Micro)) +
    geom_col(aes(y = .data[[mean_col]]), fill = "lightblue") +
    geom_errorbar(aes(
      ymin = .data[[mean_col]] - .data[[se_col]],
      ymax = .data[[mean_col]] + .data[[se_col]]
    ), width = 0.15) +
    facet_wrap(~Trial, ncol = 1) +
    labs(y = var, x = "Micro") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

# creare e mostrare tutti i grafici
for(v in vars){
  print(plot_bar(v))
}

Df_sin_NT <- Df %>% 
  filter(Combination %in% c("yeast alone", "bacteria alone", "NT")) %>% 
  filter(!(Micro == "G. oxydans + A. syzygii")) %>% 
  arrange(Trial, Micro)

#################### MEDIE SOTTRATTE A NT ######################################################################

medie_sin_NT <- Df_sin_NT %>% 
  group_by(Trial, Micro, Group, Combination) %>% 
  summarise(
    across(c(Gluconic,
             Acetic,
             `G+F`,
             EtOH,
             pH,
             Acidità_titolabile,
             arc_mck_5,
             arc_mck_10),
           
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             n = ~sum(!is.na(.x)),
             se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
           ),
           
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

NT_AAB <- medie_sin_NT %>%
  filter(Micro == "NT + brodo coltura batteri") %>%
  dplyr::select(Trial,ends_with("_mean"), ends_with("_se"))

NT_Y <- medie_sin_NT %>%
  filter(Micro == "NT + brodo coltura lieviti") %>%
  dplyr::select(Trial, ends_with("_mean"), ends_with("_se"))

bacteria_NT <- medie_sin %>%
  filter(Combination == "bacteria alone") %>%      # colonna che identifica la tesi
  left_join(NT_AAB, by = "Trial", suffix = c("", "_NT")) %>%
  mutate(
    Acetic_sub = Acetic_mean - Acetic_mean_NT,
    Acetic_se_sub = sqrt(Acetic_se^2 + Acetic_se_NT^2),
    
    Gluconic_sub = Gluconic_mean - Gluconic_mean_NT,
    Gluconic_se_sub = sqrt(Gluconic_se^2 + Gluconic_se_NT^2),
    
    
    `G+F_sub` = `G+F_mean` - `G+F_mean_NT`,
    `G+F_se_sub` = sqrt(`G+F_se`^2 + `G+F_se_NT`^2),
    
    EtOH_sub = EtOH_mean - EtOH_mean_NT,
    EtOH_se_sub = sqrt(EtOH_se^2 + EtOH_se_NT^2)
  )

yeast_NT <- medie_sin %>%
  filter(Combination  == "yeast alone") %>%
  left_join(NT_Y, by = "Trial", suffix = c("", "_NT")) %>%
  mutate(
    Acetic_sub = Acetic_mean - Acetic_mean_NT,
    Acetic_se_sub = sqrt(Acetic_se^2 + Acetic_se_NT^2),
    
    Gluconic_sub = Gluconic_mean - Gluconic_mean_NT,
    Gluconic_se_sub = sqrt(Gluconic_se^2 + Gluconic_se_NT^2),
    
    
    `G+F_sub` = `G+F_mean` - `G+F_mean_NT`,
    `G+F_se_sub` = sqrt(`G+F_se`^2 + `G+F_se_NT`^2),
    
    EtOH_sub = EtOH_mean - EtOH_mean_NT,
    EtOH_se_sub = sqrt(EtOH_se^2 + EtOH_se_NT^2)
  )

medie_sottratte <- bind_rows(bacteria_NT, yeast_NT)

# variabili da plottare
vars_sub <- c("Gluconic",
          "Acetic",
          "G+F",
          "EtOH")


plot_bar_sub <- function(var) {
  
  mean_col <- paste0(var, "_sub")
  se_col   <- paste0(var, "_se_sub")
  
  ggplot(medie_sottratte, aes(x = Micro)) +
    geom_col(aes(y = .data[[mean_col]]), fill = "lightblue") +
    geom_errorbar(aes(
      ymin = .data[[mean_col]] - .data[[se_col]],
      ymax = .data[[mean_col]] + .data[[se_col]]
    ), width = 0.10) +
    facet_wrap(~Trial, ncol = 1) +
    labs(y = var, x = "Micro") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

# creare e mostrare tutti i grafici
for(v in vars_sub){
  print(plot_bar_sub(v))
}

########### BARPLOT CON NT E RIGA ORIZZONTALE #####################

medie_sin_NT <- Df_sin_NT %>% 
  group_by(Trial, Micro, Group, Combination) %>% 
  summarise(
    across(c(Gluconic,
             Acetic,
             `G+F`,
             EtOH,
             pH,
             Acidità_titolabile,
             arc_mck_5,
             arc_mck_10),
           
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             n = ~sum(!is.na(.x)),
             se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
           ),
           
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) 
  
# statistiche normali
medie_normali <- Df_sin %>% 
  group_by(Trial, Micro, Group, Combination) %>% 
  summarise(
    across(all_of(vars),
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             n = ~sum(!is.na(.x)),
             se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
           ),
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# statistiche NT solo per Trial
medie_NT <- Df_sin_NT %>%
  filter(Group %in% c("NT Y", "NT AAB")) %>%
  group_by(Trial, Group) %>%
  summarise(
    across(all_of(vars),
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             n = ~sum(!is.na(.x)),
             se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
           ),
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Micro = Group,
    Combination = NA
  )

# dataset finale
medie_sin_NT_group <- bind_rows(medie_normali, medie_NT) %>% 
  filter(!(Group == "NT"))

# funzione per creare il grafico
plot_bar <- function(var) {
  
  mean_col <- paste0(var, "_mean")
  se_col   <- paste0(var, "_se")
  
  ggplot(medie_sin_NT_group, aes(x = Micro)) +
    geom_col(aes(y = .data[[mean_col]]), fill = "lightblue") +
    geom_errorbar(aes(
      ymin = .data[[mean_col]] - .data[[se_col]],
      ymax = .data[[mean_col]] + .data[[se_col]]
    ), width = 0.15) +
    facet_wrap(~Trial, ncol = 1) +
    labs(y = var, x = "Micro") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

# creare e mostrare tutti i grafici
for(v in vars){
  print(plot_bar(v))
}














