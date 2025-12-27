##### Paso 1 Variables predictoras ENSSEX (BD2) y análisis de pérdida ####
library(rstudioapi) 
library(dplyr)
library(readxl)
library(stringr)
library(haven)

#abrir directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#resetear
rm(list=ls())

#leer base
load("20241205_enssex_data.RData")
bd2 <- enssex4; rm(enssex4)

bd2 <- bd2 %>%
  mutate(
    id_fila = 1:nrow(bd2),
    edad_num = as.numeric(p4), 
    region_num = as.numeric(region),
    # Depresión (Lógica Minsal)
    dep = case_when(
      as.numeric(i_1_p18a2) == 2 ~ 0,
      as.numeric(i_1_p18a2) == 1 & as.numeric(i_1_p18e2) == 1 ~ 1,
      as.numeric(i_1_p18a2) == 1 & as.numeric(i_1_p18e2) == 2 ~ 0,
      as.numeric(i_1_p18a2) == 9 ~ NA_real_,
      as.numeric(i_1_p18a2) == 1 & as.numeric(i_1_p18e2) == 9 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Autoidentificación Género
    genid = case_when(
      as.numeric(p3) == 1 ~ "Masculino",
      as.numeric(p3) == 2 ~ "Femenino",
      as.numeric(p3) == 3 ~ "Transmasculino u hombre trans",
      as.numeric(p3) == 4 ~ "Transfemenino o mujer trans",
      as.numeric(p3) == 5 ~ "No binario",
      as.numeric(p3) == 6 ~ "Otro. Especifique",
      as.numeric(p3) %in% c(7, 8, 9) ~ "Prefiere no responder", 
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # Sustancias
    coca_12m = case_when(as.numeric(i_1_p25)==2~0, as.numeric(i_1_p25)==1 & as.numeric(i_1_p26)%in%c(1,2)~1, as.numeric(i_1_p25)==1 & as.numeric(i_1_p26)==3~0, TRUE~NA_real_),
    mari_12m = case_when(as.numeric(i_2_p25)==2~0, as.numeric(i_2_p25)==1 & as.numeric(i_2_p26)%in%c(1,2)~1, as.numeric(i_2_p25)==1 & as.numeric(i_2_p26)==3~0, TRUE~NA_real_),
    alco_12m = case_when(as.numeric(i_5_p25)==2~0, as.numeric(i_5_p25)==1 & as.numeric(i_5_p26)%in%c(1,2)~1, as.numeric(i_5_p25)==1 & as.numeric(i_5_p26)==3~0, TRUE~NA_real_),
    
    # Educación
    educ_harmo = case_when(
      as.numeric(p5)==1~"Nunca asistió", as.numeric(p5)%in%c(2,3,4)~"Preescolar", as.numeric(p5)==5~"Educación especial",
      as.numeric(p5)%in%c(6,7)~"Básica/Primaria", as.numeric(p5)%in%c(8,9)~"Media CH", as.numeric(p5)%in%c(10,11)~"Media TP",
      as.numeric(p5)==12~"Tec.Inc", as.numeric(p5)==13~"Tec.Com", as.numeric(p5)==14~"Prof.Inc", as.numeric(p5)==15~"Prof.Com",
      as.numeric(p5)%in%c(16,17)~"Postgrado", TRUE~NA_character_
    ) %>% factor(),
    
    # Satisfacción Vital
    satvida = ifelse(as.numeric(p8)%in%1:5, as.numeric(p8), NA_real_),
    
    #variables predictorias
    REGION = case_when(region_num == 1 ~ "1 Región de Tarapacá", region_num == 8 ~ "8 Región del Biobío", TRUE ~ NA_character_) %>% factor(),
    SEXO = case_when(as.numeric(p1) == 1 ~ "Hombre", as.numeric(p1) == 2 ~ "Mujer", TRUE ~ NA_character_) %>% factor(),
    EDAD = edad_num,
    p8_num = ifelse(as.numeric(p8)%in%1:5, as.numeric(p8), NA_real_),
    p10_num = ifelse(as.numeric(p10)%in%1:5, as.numeric(p10), NA_real_),
    p22_num = ifelse(as.numeric(p22)>20, as.numeric(p22), NA_real_),
    p23_num = ifelse(as.numeric(p23)>100, as.numeric(p23), NA_real_),
    i_1_p9_num = ifelse(as.numeric(i_1_p9)%in%1:7, as.numeric(i_1_p9), NA_real_),
    i_2_p9_num = ifelse(as.numeric(i_2_p9)%in%1:7, as.numeric(i_2_p9), NA_real_),
    i_3_p9_num = ifelse(as.numeric(i_3_p9)%in%1:7, as.numeric(i_3_p9), NA_real_),
    i_4_p9_num = ifelse(as.numeric(i_4_p9)%in%1:7, as.numeric(i_4_p9), NA_real_),
    i_5_p9_num = ifelse(as.numeric(i_5_p9)%in%1:7, as.numeric(i_5_p9), NA_real_),
    i_1_p24_num = ifelse(as.numeric(i_1_p24)%in%1:5, as.numeric(i_1_p24), NA_real_),
    i_2_p24_num = ifelse(as.numeric(i_2_p24)%in%1:5, as.numeric(i_2_p24), NA_real_),
    i_3_p24_num = ifelse(as.numeric(i_3_p24)%in%1:5, as.numeric(i_3_p24), NA_real_)
  ) %>%
  filter(region_num %in% c(1, 8), edad_num >= 18, edad_num <= 29)

#### Diagnostico NA de variables predictoras en observaciones a clasificar ####
# Variables candidatas
vars_check <- c("REGION", "EDAD", "SEXO", "p8_num", "p10_num", "p22_num", "p23_num", 
                "i_1_p9_num", "i_2_p9_num", "i_3_p9_num", "i_4_p9_num", "i_5_p9_num",
                "i_1_p24_num", "i_2_p24_num", "i_3_p24_num")

# 1. Depresión
faltan <- bd2 %>% filter(is.na(dep)); nrow(faltan)
colSums(is.na(faltan %>% dplyr::select(any_of(vars_check))))

# 2. Cocaína
faltan <- bd2 %>% filter(is.na(coca_12m)); nrow(faltan)
colSums(is.na(faltan %>% dplyr::select(any_of(vars_check))))

# 3. Marihuana
faltan <- bd2 %>% filter(is.na(mari_12m)); nrow(faltan)
colSums(is.na(faltan %>% dplyr::select(any_of(vars_check))))

# 4. Alcohol
faltan <- bd2 %>% filter(is.na(alco_12m)); nrow(faltan)
colSums(is.na(faltan %>% dplyr::select(any_of(vars_check))))

# 5. Satisfacción Vida (Sin p8)
vars_sat <- setdiff(vars_check, "p8_num")
faltan <- bd2 %>% filter(is.na(satvida)); nrow(faltan)
colSums(is.na(faltan %>% dplyr::select(any_of(vars_sat))))

#### Calcular perdida de datos ####
# Listas de predictores seleccionadas por tener 0 NAs en el diagnóstico previo
pred_dep  <- c("REGION", "EDAD", "SEXO", "p8_num", "i_1_p9_num", "i_2_p9_num", "i_4_p9_num", "i_5_p9_num", "i_1_p24_num", "i_2_p24_num", "i_3_p24_num")
pred_coca <- c("REGION", "EDAD", "SEXO", "p8_num", "i_1_p9_num", "i_2_p9_num", "i_4_p9_num", "i_1_p24_num")
pred_mari <- c("REGION", "EDAD", "SEXO", "p8_num", "i_1_p9_num", "i_2_p9_num", "i_4_p9_num", "i_1_p24_num", "i_2_p24_num", "i_3_p24_num")
pred_alco <- c("REGION", "EDAD", "SEXO", "p8_num", "i_1_p9_num", "i_2_p9_num", "i_1_p24_num", "i_2_p24_num", "i_3_p24_num")
pred_gen  <- c("REGION", "EDAD", "SEXO", "p8_num", "i_1_p9_num", "i_1_p24_num") # Base segura
pred_sat  <- c("REGION", "EDAD", "SEXO", "p22_num", "p23_num", "i_1_p9_num", "i_2_p9_num", "i_4_p9_num", "i_5_p9_num", "i_1_p24_num", "i_2_p24_num", "i_3_p24_num")

calc_loss <- function(nombre, target_col, preds, df) {
  base_bruta <- df %>% filter(!is.na(.data[[target_col]]))
  n_orig <- nrow(base_bruta)
  
  if(n_orig == 0) return(c(nombre, 0, 0, 0, "0%"))
  
  base_neta <- base_bruta %>% dplyr::select(all_of(c(target_col, preds))) %>% na.omit()
  n_model <- nrow(base_neta)
  
  perdidos <- n_orig - n_model
  porc <- round((perdidos/n_orig)*100, 1)
  
  return(c(nombre, n_orig, n_model, perdidos, paste0(porc,"%")))
}

# Tabla de Pérdida
tabla_perdida <- data.frame(Target=character(), N_Original=character(), N_Modelo=character(), Perdidos=character(), Porc_Perdida=character(), stringsAsFactors=F)

tabla_perdida[1,] <- calc_loss("Depresión", "dep", pred_dep, bd2)
tabla_perdida[2,] <- calc_loss("GenID", "genid", pred_gen, bd2)
tabla_perdida[3,] <- calc_loss("Cocaína", "coca_12m", pred_coca, bd2)
tabla_perdida[4,] <- calc_loss("Marihuana", "mari_12m", pred_mari, bd2)
tabla_perdida[5,] <- calc_loss("Alcohol", "alco_12m", pred_alco, bd2)
tabla_perdida[6,] <- calc_loss("Educación", "educ_harmo", pred_gen, bd2)
tabla_perdida[7,] <- calc_loss("Sat.Vida", "satvida", pred_sat, bd2)

print(tabla_perdida)

