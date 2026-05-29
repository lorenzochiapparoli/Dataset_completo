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
Df_sin_NT <- Df %>% 
  filter(Combination %in% c("yeast alone", "bacteria alone", "NT")) %>% 
  filter(!(Micro == "G. oxydans + A. syzygii")) %>% 
  arrange(Trial, Micro)





library(dplyr)
library(ggplot2)

# 1️⃣ Calcolo delle medie e SE
vars <- c("Gluconic", "Acetic", "G+F", "EtOH", "pH", "Acidità_titolabile", "arc_mck_5", "arc_mck_10")

medie_sin_NT <- Df_sin_NT %>%
  group_by(Trial, Micro, Group, Combination) %>%
  summarise(
    across(all_of(vars),
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd   = ~sd(.x, na.rm = TRUE),
             n    = ~sum(!is.na(.x)),
             se   = ~sd(.x, na.rm = TRUE)/sqrt(sum(!is.na(.x)))
           ),
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# 2️⃣ Controlli NT
NT_AAB <- medie_sin_NT %>%
  filter(Micro == "NT + brodo coltura batteri") %>%
  dplyr::select(Trial, ends_with("_mean"), ends_with("_se"))

NT_Y <- medie_sin_NT %>%
  filter(Micro == "NT + brodo coltura lieviti") %>%
  dplyr:: select(Trial, ends_with("_mean"), ends_with("_se"))

# 3️⃣ Sottrazioni per batteri
bacteria_NT <- medie_sin_NT %>%
  filter(Combination == "bacteria alone") %>%
  left_join(NT_AAB, by = "Trial", suffix = c("", "_NT")) %>%
  mutate(
    across(c(Gluconic_mean, Acetic_mean, `G+F_mean`, EtOH_mean),
           ~ . - get(paste0(cur_column(), "_NT")),
           .names = "{.col}_sub"),
    across(c(Gluconic_se, Acetic_se, `G+F_se`, EtOH_se),
           ~ sqrt(.^2 + get(paste0(cur_column(), "_NT"))^2),
           .names = "{.col}_se_sub")
  )

# 4️⃣ Sottrazioni per lieviti
yeast_NT <- medie_sin_NT %>%
  filter(Combination == "yeast alone") %>%
  left_join(NT_Y, by = "Trial", suffix = c("", "_NT")) %>%
  mutate(
    across(c(Gluconic_mean, Acetic_mean, `G+F_mean`, EtOH_mean),
           ~ . - get(paste0(cur_column(), "_NT")),
           .names = "{.col}_sub"),
    across(c(Gluconic_se, Acetic_se, `G+F_se`, EtOH_se),
           ~ sqrt(.^2 + get(paste0(cur_column(), "_NT"))^2),
           .names = "{.col}_se_sub")
  )

# 5️⃣ Dataset finale
medie_sottratte <- bind_rows(bacteria_NT, yeast_NT)

# 6️⃣ Funzione per creare i barplot con percentuale
plot_bar_sub <- function(var) {
  
  mean_col <- paste0(var, "_mean_sub")
  se_col   <- paste0(var, "_se_se_sub")
  
  df <- medie_sottratte
  
  NT_AAB_sub <- NT_AAB %>%
    dplyr::select(Trial, all_of(paste0(var, "_mean"))) %>%
    rename(NT_value = all_of(paste0(var, "_mean")))
  
  NT_Y_sub <- NT_Y %>%
    dplyr::select(Trial, all_of(paste0(var, "_mean"))) %>%
    rename(NT_value = all_of(paste0(var, "_mean")))
  
  df <- df %>%
    left_join(NT_AAB_sub, by = "Trial") %>%
    mutate(
      NT_value = ifelse(
        Combination == "yeast alone",
        NT_Y_sub$NT_value[match(Trial, NT_Y_sub$Trial)],
        NT_value
      )
    )
  
  df$perc <- (df[[mean_col]] / df$NT_value) * 100
  
  ggplot(df, aes(x = Micro)) +
    geom_col(aes(y = .data[[mean_col]]), fill = "lightblue") +
    
    geom_errorbar(
      aes(
        ymin = .data[[mean_col]] - .data[[se_col]],
        ymax = .data[[mean_col]] + .data[[se_col]]
      ),
      width = 0.10
    ) +
    
    geom_text(
      aes(
        y = .data[[mean_col]] + .data[[se_col]],
        label = ifelse(is.na(perc), "", paste0(round(perc,1), "%"))
      ),
      vjust = -0.4,
      size = 3
    ) +
    
    geom_hline(yintercept = 0, linetype = "dashed") +
    
    facet_wrap(~Trial, ncol = 1) +
    
    labs(y = var, x = "Micro") +
    
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# 7️⃣ Creare e mostrare tutti i grafici
vars_sub <- c("Gluconic", "Acetic", "G+F", "EtOH")

for(v in vars_sub){
  print(plot_bar_sub(v))
}

