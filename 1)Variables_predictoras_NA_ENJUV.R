##### Paso 1 Variables predictoras ENJUV y analisis de perdida ####
library(rstudioapi) 
library(dplyr)
library(readxl)
library(stringr)

#abrir directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#resetear
rm(list=ls())

#leer base
bd1 <- read_xlsx("BBDD Respuesta - Encuesta Jóvenes.xlsx", sheet = 1)

#creae variables, ajuste de predictores y filtrar 
bd1 <- bd1 %>%
  mutate(
    id_fila = 1:nrow(bd1),
    # Depresión
    dep = case_when(
      as.character(P103)=="NS/NR" | as.character(P104_1)=="NS/NR" ~ NA_real_,
      as.character(P104_1)=="Sí" ~ 1, 
      as.character(P104_1)%in%c("No","No Aplica") ~ 0, 
      TRUE ~ NA_real_
    ),
    # Sustancias
    coca_12m = case_when(as.character(P76_4)=="Sí"~1, as.character(P76_4)=="No"~0, as.character(P76_4)=="NS/NR"~NA_real_, TRUE~NA_real_),
    mari_12m = case_when(as.character(P76_3)=="Sí"~1, as.character(P76_3)=="No"~0, as.character(P76_3)=="NS/NR"~NA_real_, TRUE~NA_real_),
    alco_12m = case_when(as.character(P76_1)=="Sí"~1, as.character(P76_1)=="No"~0, as.character(P76_1)=="NS/NR"~NA_real_, TRUE~NA_real_),
    # Educación
    p14_num = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P14)))),
    educ_harmo = case_when(
      p14_num==1~"Nunca", p14_num%in%2:3~"Preescolar", p14_num==4~"Especial", p14_num%in%5:6~"Basica",
      p14_num%in%7:8~"MediaCH", p14_num%in%9:10~"MediaTP", p14_num%in%11:14~"Superior", p14_num%in%15:16~"Postgrado",
      TRUE ~ NA_character_
    ),
    educ_harmo = ifelse(is.na(educ_harmo) & !is.na(P14), NA_character_, educ_harmo),
    # Satisfacción Vital
    satvida = suppressWarnings(as.numeric(str_extract(as.character(P6_1), "^[1-5]"))),
    satvida = ifelse(satvida %in% 1:5, satvida, NA_real_),
    # Variables predictoras
    REGION = factor(REGION), SEXO = factor(SEXO), GSE = factor(GSE), EDAD = as.numeric(EDAD),
    P54_num = P54, 
    P5_num    = ifelse(suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P5)))) %in% c(98, 99), NA, suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P5))))),
    P6_1_num  = suppressWarnings(as.numeric(str_extract(as.character(P6_1), "^[1-5]"))),
    P6_2_num  = suppressWarnings(as.numeric(str_extract(as.character(P6_2), "^[1-5]"))),
    P6_3_num  = suppressWarnings(as.numeric(str_extract(as.character(P6_3), "^[1-5]"))),
    P6_4_num  = suppressWarnings(as.numeric(str_extract(as.character(P6_4), "^[1-5]"))),
    P6_6_num  = suppressWarnings(as.numeric(str_extract(as.character(P6_6), "^[1-5]"))),
    P6_7_num  = suppressWarnings(as.numeric(str_extract(as.character(P6_7), "^[1-5]"))),
    P19_1_num = suppressWarnings(as.numeric(str_extract(as.character(P19_1), "^[1-7]"))),
    P19_2_num = suppressWarnings(as.numeric(str_extract(as.character(P19_2), "^[1-7]"))),
    P35_num   = ifelse(suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P35)))) %in% c(98, 99), NA, suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P35)))))
  ) %>%
  filter(REGION %in% c("1 Región de Tarapacá","8 Región del Biobío"), EDAD >= 18, EDAD <= 29, ZONA == "1 Urbano")

#### Diagnostico NA de variables predictoras en observaciones a clasificar ####
#variables candidatas
vars_check <- c("REGION", "EDAD", "SEXO", "GSE", "P54_num", "P5_num", 
                "P6_1_num", "P6_2_num", "P6_3_num", "P6_4_num", "P6_6_num", "P6_7_num",
                "P19_1_num", "P19_2_num", "P35_num")
# 1. Depresión
faltan <- bd1 %>% filter(is.na(dep)); nrow(faltan)
print(colSums(is.na(faltan %>% dplyr::select(any_of(vars_check)))))

# 2. Cocaína
faltan <- bd1 %>% filter(is.na(coca_12m));nrow(faltan)
print(colSums(is.na(faltan %>% dplyr::select(any_of(vars_check)))))

# 3. Marihuana
faltan <- bd1 %>% filter(is.na(mari_12m));nrow(faltan)
print(colSums(is.na(faltan %>% dplyr::select(any_of(vars_check)))))

# 4. Alcohol
faltan <- bd1 %>% filter(is.na(alco_12m));nrow(faltan)
print(colSums(is.na(faltan %>% dplyr::select(any_of(vars_check)))))

# 5. Educación
vars_educ <- c(vars_check)
faltan <- bd1 %>% filter(is.na(educ_harmo));nrow(faltan)
print(colSums(is.na(faltan %>% dplyr::select(any_of(vars_educ)))))

# 6. Satisfacción Vida
vars_sat <- setdiff(vars_check, "P6_1_num") 
faltan <- bd1 %>% filter(is.na(satvida));nrow(faltan)
print(colSums(is.na(faltan %>% dplyr::select(any_of(vars_sat)))))

####Calcular perdida de datos####
# Listas de predictores usadas en los modelos finales
pred_dep    <- c("REGION","EDAD","SEXO","GSE","P54_num","P6_1_num","P6_2_num","P19_1_num")
pred_drogas <- c("REGION","EDAD","SEXO","GSE","P54_num","P6_1_num","P6_3_num","P6_4_num","P6_6_num","P6_7_num","P19_2_num","P35_num")
pred_alco   <- c("REGION","EDAD","SEXO","GSE","P54_num","P5_num","P6_1_num","P6_3_num","P6_4_num","P6_6_num","P6_7_num","P35_num")
pred_educ   <- c("REGION","EDAD","SEXO","GSE","P54_num","P5_num","P6_1_num","P6_2_num","P6_3_num","P6_4_num","P6_6_num","P6_7_num","P19_1_num","P19_2_num","P35_num") # Usa casi todo
pred_sat    <- c("REGION","EDAD","SEXO","GSE","P54_num","P35_num")

calc_loss <- function(nombre, target_col, preds, df) {
  base_bruta <- df %>% filter(!is.na(.data[[target_col]])) # Solo gente con target respondido
  n_orig <- nrow(base_bruta)
  
  # Filtramos NAs en predictores
  base_neta <- base_bruta %>% dplyr::select(all_of(c(target_col, preds))) %>% na.omit()
  n_model <- nrow(base_neta)
  
  perdidos <- n_orig - n_model
  porc <- round((perdidos/n_orig)*100, 1)
  
  return(c(nombre, n_orig, n_model, perdidos, paste0(porc,"%")))
}
# Crear Tabla
tabla_perdida <- data.frame(Target=character(), N_Original=character(), N_Modelo=character(), Perdidos=character(), Porc_Perdida=character(), stringsAsFactors=F)

tabla_perdida[1,] <- calc_loss("Depresión", "dep", pred_dep, bd1)
tabla_perdida[2,] <- calc_loss("Cocaína", "coca_12m", pred_drogas, bd1)
tabla_perdida[3,] <- calc_loss("Marihuana", "mari_12m", pred_drogas, bd1)
tabla_perdida[4,] <- calc_loss("Alcohol", "alco_12m", pred_alco, bd1)
tabla_perdida[5,] <- calc_loss("Educación", "educ_harmo", pred_educ, bd1)
tabla_perdida[6,] <- calc_loss("Sat.Vida", "satvida", pred_sat, bd1)
print(tabla_perdida)
