#### Paso 2: Clasificación NA en base ENSSEX ####
library(rstudioapi) 
library(writexl)
library(dplyr)
library(readxl)
library(stringr)
library(haven)
library(pROC)         # Para curvas ROC y AUC
library(e1071)        # SVM y función tune()
library(nnet)         # RNA y Multinomial
library(randomForest) # Random Forest

# Resetear
rm(list=ls())

#abrir directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#leer base
load("20241205_enssex_data.RData")
bd2 <- enssex4; rm(enssex4)

# 1. CREACIÓN DE VARIABLES (Para toda la base)
bd2 <- bd2 %>%
  mutate(
    id_fila = 1:nrow(bd2),
    edad_num = as.numeric(p4), 
    region_num = as.numeric(region),
    
    #Tratamiento depresión
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
    coca_12m = case_when(
      as.numeric(i_1_p25) == 2 ~ 0,
      as.numeric(i_1_p25) == 1 & as.numeric(i_1_p26) %in% c(1,2) ~ 1,
      as.numeric(i_1_p25) == 1 & as.numeric(i_1_p26) == 3 ~ 0,
      TRUE ~ NA_real_
    ),
    mari_12m = case_when(
      as.numeric(i_2_p25) == 2 ~ 0,
      as.numeric(i_2_p25) == 1 & as.numeric(i_2_p26) %in% c(1,2) ~ 1,
      as.numeric(i_2_p25) == 1 & as.numeric(i_2_p26) == 3 ~ 0,
      TRUE ~ NA_real_
    ),
    alco_12m = case_when(
      as.numeric(i_5_p25) == 2 ~ 0,
      as.numeric(i_5_p25) == 1 & as.numeric(i_5_p26) %in% c(1,2) ~ 1,
      as.numeric(i_5_p25) == 1 & as.numeric(i_5_p26) == 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Educación
    educ_harmo = case_when(
      as.numeric(p5) == 1 ~ "Nunca asistió",
      as.numeric(p5) %in% c(2,3,4) ~ "Preescolar",
      as.numeric(p5) == 5 ~ "Educación especial",
      as.numeric(p5) %in% c(6,7) ~ "Básica/Primaria",
      as.numeric(p5) %in% c(8,9) ~ "Media Científico-Humanista", 
      as.numeric(p5) %in% c(10,11) ~ "Media Técnico-Profesional",
      as.numeric(p5) == 12 ~ "Técnico sup. incompleto",
      as.numeric(p5) == 13 ~ "Técnico sup. completo",
      as.numeric(p5) == 14 ~ "Profesional incompleto",
      as.numeric(p5) == 15 ~ "Profesional completo",
      as.numeric(p5) %in% c(16,17) ~ "Postgrado",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # Satisfacción Vital
    satvida = ifelse(as.numeric(p8) %in% 1:5, as.numeric(p8), NA_real_),
    satvida = factor(satvida), # Factor para clasificación
    
    #variables predictoras a usar
    REGION = case_when(region_num == 1 ~ "1 Región de Tarapacá", region_num == 8 ~ "8 Región del Biobío", TRUE ~ NA_character_) %>% factor(),
    SEXO = case_when(as.numeric(p1) == 1 ~ "Hombre", as.numeric(p1) == 2 ~ "Mujer", TRUE ~ NA_character_) %>% factor(),
    EDAD = edad_num,
    p8_num = ifelse(as.numeric(p8) %in% 1:5, as.numeric(p8), NA_real_),    
    p10_num = ifelse(as.numeric(p10) %in% 1:5, as.numeric(p10), NA_real_),
    p22_num = ifelse(as.numeric(p22) > 20, as.numeric(p22), NA_real_), 
    p23_num = ifelse(as.numeric(p23) > 100, as.numeric(p23), NA_real_),
    i_1_p9_num = ifelse(as.numeric(i_1_p9) %in% 1:7, as.numeric(i_1_p9), NA_real_),
    i_2_p9_num = ifelse(as.numeric(i_2_p9) %in% 1:7, as.numeric(i_2_p9), NA_real_),
    i_3_p9_num = ifelse(as.numeric(i_3_p9) %in% 1:7, as.numeric(i_3_p9), NA_real_),
    i_4_p9_num = ifelse(as.numeric(i_4_p9) %in% 1:7, as.numeric(i_4_p9), NA_real_),
    i_5_p9_num = ifelse(as.numeric(i_5_p9) %in% 1:7, as.numeric(i_5_p9), NA_real_),
    i_1_p24_num = ifelse(as.numeric(i_1_p24) %in% 1:5, as.numeric(i_1_p24), NA_real_),
    i_2_p24_num = ifelse(as.numeric(i_2_p24) %in% 1:5, as.numeric(i_2_p24), NA_real_),
    i_3_p24_num = ifelse(as.numeric(i_3_p24) %in% 1:5, as.numeric(i_3_p24), NA_real_)
  ) %>%
  # FILTRAMOS SOLO POBLACIÓN OBJETIVO (18-29 años) 
  filter(edad_num >= 18, edad_num <= 29)

# Guardar base nacional para comparaciones posteriores
bd2_pais <- bd2
write_xlsx(bd2_pais, "bd2_pais.xlsx")

# Filtramos regiones de interes
bd2 <- bd2 %>%
  filter(region_num %in% c(1, 8))


##### 1. Tratamiento depresion ####
table(bd2$dep, useNA = "ifany") 
datos_dep <- bd2 %>% dplyr::select(id_fila, dep, REGION, EDAD, SEXO, p8_num, i_1_p9_num, i_2_p9_num, i_4_p9_num, i_5_p9_num, i_1_p24_num, i_2_p24_num, i_3_p24_num)
base_modelar <- datos_dep %>% filter(!is.na(dep)) %>% na.omit()
base_imputar <- datos_dep %>% filter(is.na(dep)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- dep ~ . -id_fila

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. LOGÍSTICA
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$dep, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$dep,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$dep,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM
t_lin <- tune(svm, as.factor(dep) ~ ., data = train, kernel = "linear", ranges = list(cost = c(0.1, 1, 10)), probability = TRUE)
t_rad <- tune(svm, as.factor(dep) ~ ., data = train, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100), gamma = c(0.01, 0.1, 1)), probability = TRUE)

if(t_lin$best.performance < t_rad$best.performance) {
  best_svm <- t_lin$best.model
  nom_svm <- paste0("SVM (Linear, C=", t_lin$best.parameters$cost, ")")
} else {
  best_svm <- t_rad$best.model
  nom_svm <- paste0("SVM (Radial, C=", t_rad$best.parameters$cost, ", G=", t_rad$best.parameters$gamma, ")")
}

p_tr <- attr(predict(best_svm, train, probability=TRUE), "probabilities")[,"1"]; r_tr <- roc(train$dep, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$dep,c(0,1)), factor(c_tr,c(0,1)))
p_va <- attr(predict(best_svm, valid, probability=TRUE), "probabilities")[,"1"]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$dep,c(0,1)), factor(c_va,c(0,1)))
resumen[2,] <- c(nom_svm, round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# C. RNA 
best_acc <- 0; best_size <- 5
for(s in c(5, 10, 15, 20)){
  mod_tmp <- nnet(as.factor(dep) ~ ., data=train, size=s, decay=0.1, maxit=1000, trace=FALSE)
  p_tmp   <- as.numeric(predict(mod_tmp, valid, type="raw"))
  acc_tmp <- mean(ifelse(p_tmp >= 0.5, 1, 0) == valid$dep)
  if(acc_tmp > best_acc){ best_acc <- acc_tmp; best_size <- s }
}
m_rna <- nnet(as.factor(dep) ~ ., data=train, size=best_size, decay=0.1, maxit=1000, trace=FALSE)
p_tr <- as.numeric(predict(m_rna, train, type="raw")); r_tr <- roc(train$dep, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$dep,c(0,1)), factor(c_tr,c(0,1)))
p_va <- as.numeric(predict(m_rna, valid, type="raw")); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$dep,c(0,1)), factor(c_va,c(0,1)))
resumen[3,] <- c(paste0("RNA (Size=",best_size,")"), round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# D. RANDOM FOREST
m_rf <- randomForest(as.factor(dep) ~ ., data=train, ntree=500)
p_tr <- predict(m_rf, train, type="prob")[,2]; r_tr <- roc(train$dep, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$dep,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_rf, valid, type="prob")[,2]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$dep,c(0,1)), factor(c_va,c(0,1)))
resumen[4,] <- c("Random Forest", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

resumen[,2:8] <- lapply(resumen[,2:8], as.numeric); resumen_ordenado <- resumen[order(-resumen$Acc_Valid), ]
resumen_ordenado

# Clasificación NA (Random Forest)
modelo_final <- randomForest(as.factor(dep) ~ ., data=base_modelar, ntree=500)
probs_imputar <- predict(modelo_final, newdata=base_imputar, type="prob")[,2]
roc_total <- roc(base_modelar$dep, predict(modelo_final, base_modelar, type="prob")[,2], quiet=TRUE); th <- coords(roc_total, "best", transpose=TRUE)["threshold"]
clases_imputadas <- ifelse(probs_imputar >= th, 1, 0)
base_imputar$dep_predicho <- clases_imputadas
table(base_imputar$dep_predicho)
bd2$dep[match(base_imputar$id_fila, bd2$id_fila)] <- clases_imputadas

##### 2. Consumo de substancias ####
###### 2.1 Cocaína ####
table(bd2$coca_12m, useNA = "ifany") 
datos_coca <- bd2 %>% dplyr::select(id_fila, target=coca_12m, REGION, EDAD, SEXO, p8_num, i_1_p9_num, i_2_p9_num, i_4_p9_num, i_1_p24_num)
base_modelar <- datos_coca %>% filter(!is.na(target)) %>% na.omit()
base_imputar <- datos_coca %>% filter(is.na(target)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- target ~ . -id_fila

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. Logística
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM
t_lin <- tune(svm, as.factor(target) ~ ., data = train, kernel = "linear", ranges = list(cost = c(0.1, 1, 10)), probability = TRUE)
t_rad <- tune(svm, as.factor(target) ~ ., data = train, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100), gamma = c(0.01, 0.1, 1)), probability = TRUE)

if(t_lin$best.performance < t_rad$best.performance) {
  best_svm <- t_lin$best.model
  nom_svm <- paste0("SVM (Linear, C=", t_lin$best.parameters$cost, ")")
} else {
  best_svm <- t_rad$best.model
  nom_svm <- paste0("SVM (Radial, C=", t_rad$best.parameters$cost, ", G=", t_rad$best.parameters$gamma, ")")
}

p_tr <- attr(predict(best_svm, train, probability=TRUE), "probabilities")[,"1"]; r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- attr(predict(best_svm, valid, probability=TRUE), "probabilities")[,"1"]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[2,] <- c(nom_svm, round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# C. RNA
best_acc <- 0; best_size <- 5
for(s in c(5, 10, 15)){
  mod_tmp <- nnet(as.factor(target) ~ ., data=train, size=s, decay=0.1, maxit=1000, trace=FALSE)
  p_tmp   <- as.numeric(predict(mod_tmp, valid, type="raw"))
  acc_tmp <- mean(ifelse(p_tmp >= 0.5, 1, 0) == valid$target)
  if(acc_tmp > best_acc){ best_acc <- acc_tmp; best_size <- s }
}
m_rna <- nnet(as.factor(target) ~ ., data=train, size=best_size, decay=0.1, maxit=1000, trace=FALSE)
p_tr <- as.numeric(predict(m_rna, train, type="raw")); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- as.numeric(predict(m_rna, valid, type="raw")); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[3,] <- c(paste0("RNA (Size=",best_size,")"), round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# D. RANDOM FOREST
m_rf <- randomForest(as.factor(target) ~ ., data=train, ntree=500)
p_tr <- predict(m_rf, train, type="prob")[,2]; r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_rf, valid, type="prob")[,2]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[4,] <- c("Random Forest", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

resumen[,2:8] <- lapply(resumen[,2:8], as.numeric); resumen_ordenado <- resumen[order(-resumen$Acc_Valid), ]
resumen_ordenado

# Clasificación NA (Random Forest)
modelo_final <- randomForest(as.factor(target) ~ ., data=base_modelar, ntree=500)
probs_imp <- predict(modelo_final, newdata=base_imputar, type="prob")[,2]
roc_total <- roc(base_modelar$target, predict(modelo_final, base_modelar, type="prob")[,2], quiet=TRUE); th <- coords(roc_total, "best", transpose=TRUE)["threshold"]
clases_imp <- ifelse(probs_imp >= th, 1, 0)
bd2$coca_12m[match(base_imputar$id_fila, bd2$id_fila)] <- clases_imp
table(clases_imp)

###### 2.2 Marihuana ####
table(bd2$mari_12m, useNA = "ifany") 
datos_mari <- bd2 %>% dplyr::select(id_fila, target=mari_12m, REGION, EDAD, SEXO, p8_num, i_1_p9_num, i_2_p9_num, i_4_p9_num, i_1_p24_num, i_2_p24_num, i_3_p24_num)
base_modelar <- datos_mari %>% filter(!is.na(target)) %>% na.omit()
base_imputar <- datos_mari %>% filter(is.na(target)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- target ~ . -id_fila

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. Logística
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM
t_lin <- tune(svm, as.factor(target) ~ ., data = train, kernel = "linear", ranges = list(cost = c(0.1, 1, 10)), probability = TRUE)
t_rad <- tune(svm, as.factor(target) ~ ., data = train, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100), gamma = c(0.01, 0.1, 1)), probability = TRUE)

if(t_lin$best.performance < t_rad$best.performance) {
  best_svm <- t_lin$best.model
  nom_svm <- paste0("SVM (Linear, C=", t_lin$best.parameters$cost, ")")
} else {
  best_svm <- t_rad$best.model
  nom_svm <- paste0("SVM (Radial, C=", t_rad$best.parameters$cost, ", G=", t_rad$best.parameters$gamma, ")")
}

p_tr <- attr(predict(best_svm, train, probability=TRUE), "probabilities")[,"1"]; r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- attr(predict(best_svm, valid, probability=TRUE), "probabilities")[,"1"]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[2,] <- c(nom_svm, round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# C. RNA
best_acc <- 0; best_size <- 5
for(s in c(5, 10, 15)){
  mod_tmp <- nnet(as.factor(target) ~ ., data=train, size=s, decay=0.1, maxit=1000, trace=FALSE)
  p_tmp   <- as.numeric(predict(mod_tmp, valid, type="raw"))
  acc_tmp <- mean(ifelse(p_tmp >= 0.5, 1, 0) == valid$target)
  if(acc_tmp > best_acc){ best_acc <- acc_tmp; best_size <- s }
}
m_rna <- nnet(as.factor(target) ~ ., data=train, size=best_size, decay=0.1, maxit=1000, trace=FALSE)
p_tr <- as.numeric(predict(m_rna, train, type="raw")); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- as.numeric(predict(m_rna, valid, type="raw")); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[3,] <- c(paste0("RNA (Size=",best_size,")"), round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# D. RANDOM FOREST
m_rf <- randomForest(as.factor(target) ~ ., data=train, ntree=500)
p_tr <- predict(m_rf, train, type="prob")[,2]; r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_rf, valid, type="prob")[,2]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[4,] <- c("Random Forest", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

resumen[,2:8] <- lapply(resumen[,2:8], as.numeric); resumen_ordenado <- resumen[order(-resumen$Acc_Valid), ]
resumen_ordenado

# Clasificación NA (GANADOR: RNA Size=5)
modelo_final <- nnet(as.factor(target) ~ ., data=base_modelar, size=5, decay=0.1, maxit=1000, trace=FALSE)
# Obtenemos la clase directamente
clases_imp <- predict(modelo_final, newdata=base_imputar, type="class")
bd2$mari_12m[match(base_imputar$id_fila, bd2$id_fila)] <- as.numeric(clases_imp)
table(clases_imp)

###### 2.3 Alcohol ####
table(bd2$alco_12m, useNA = "ifany") 
datos_alco <- bd2 %>% dplyr::select(id_fila, target=alco_12m, REGION, EDAD, SEXO, p8_num, i_1_p9_num, i_2_p9_num, i_1_p24_num, i_2_p24_num, i_3_p24_num)
base_modelar <- datos_alco %>% filter(!is.na(target)) %>% na.omit()
base_imputar <- datos_alco %>% filter(is.na(target)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- target ~ . -id_fila

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. Logística
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM
t_lin <- tune(svm, as.factor(target) ~ ., data = train, kernel = "linear", ranges = list(cost = c(0.1, 1, 10)), probability = TRUE)
t_rad <- tune(svm, as.factor(target) ~ ., data = train, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100), gamma = c(0.01, 0.1, 1)), probability = TRUE)

if(t_lin$best.performance < t_rad$best.performance) {
  best_svm <- t_lin$best.model
  nom_svm <- paste0("SVM (Linear, C=", t_lin$best.parameters$cost, ")")
} else {
  best_svm <- t_rad$best.model
  nom_svm <- paste0("SVM (Radial, C=", t_rad$best.parameters$cost, ", G=", t_rad$best.parameters$gamma, ")")
}

p_tr <- attr(predict(best_svm, train, probability=TRUE), "probabilities")[,"1"]; r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- attr(predict(best_svm, valid, probability=TRUE), "probabilities")[,"1"]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[2,] <- c(nom_svm, round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# C. RNA
best_acc <- 0; best_size <- 5
for(s in c(5, 10, 15)){
  mod_tmp <- nnet(as.factor(target) ~ ., data=train, size=s, decay=0.1, maxit=1000, trace=FALSE)
  p_tmp   <- as.numeric(predict(mod_tmp, valid, type="raw"))
  acc_tmp <- mean(ifelse(p_tmp >= 0.5, 1, 0) == valid$target)
  if(acc_tmp > best_acc){ best_acc <- acc_tmp; best_size <- s }
}
m_rna <- nnet(as.factor(target) ~ ., data=train, size=best_size, decay=0.1, maxit=1000, trace=FALSE)
p_tr <- as.numeric(predict(m_rna, train, type="raw")); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- as.numeric(predict(m_rna, valid, type="raw")); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[3,] <- c(paste0("RNA (Size=",best_size,")"), round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# D. RANDOM FOREST
m_rf <- randomForest(as.factor(target) ~ ., data=train, ntree=500)
p_tr <- predict(m_rf, train, type="prob")[,2]; r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_rf, valid, type="prob")[,2]; c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[4,] <- c("Random Forest", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

resumen[,2:8] <- lapply(resumen[,2:8], as.numeric); resumen_ordenado <- resumen[order(-resumen$Acc_Valid), ]
resumen_ordenado

# Clasificación NA (GANADOR: SVM)
kern_code <- best_svm$kernel 
bk <- switch(kern_code + 1, "linear", "polynomial", "radial", "sigmoid")
bc <- best_svm$cost
bg <- if(!is.null(best_svm$gamma)) best_svm$gamma else 1 

modelo_final <- svm(as.factor(target) ~ . -id_fila, 
                    data=base_modelar, 
                    kernel=bk, cost=bc, gamma=bg, 
                    probability=TRUE)
p_full <- attr(predict(modelo_final, base_modelar, probability=TRUE), "probabilities")[,"1"]
roc_total <- roc(base_modelar$target, p_full, quiet=TRUE)
th <- coords(roc_total, "best", transpose=TRUE)["threshold"]
base_sin_target <- base_imputar %>% dplyr::select(-target)
probs_imp <- attr(predict(modelo_final, newdata=base_sin_target, probability=TRUE), "probabilities")[,"1"]
clases_imp <- ifelse(probs_imp >= th, 1, 0)
base_imputar$alco_predicho <- clases_imp
table(base_imputar$alco_predicho)
bd2$alco_12m[match(base_imputar$id_fila, bd2$id_fila)] <- clases_imp

##### 3. Autoidentificación de género ####
table(bd2$genid,useNA = "always") #No hay datos faltantes

##### 4. Nivel educacional ####
table(bd2$educ_harmo,useNA = "always")

##### 5. Nivel satisfacción de vida ####
table(bd2$satvida, useNA = "ifany") #No hay datos faltantes

datos_sat <- bd2 %>% dplyr::select(id_fila, satvida, REGION, EDAD, SEXO, p22_num, p23_num, i_1_p9_num, i_2_p9_num, i_4_p9_num, i_5_p9_num, i_1_p24_num, i_2_p24_num, i_3_p24_num)
base_modelar <- datos_sat %>% filter(!is.na(satvida)) %>% na.omit()
base_imputar <- datos_sat %>% filter(is.na(satvida)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- satvida ~ . -id_fila

resumen <- data.frame(Modelo=character(), Acc_Train=numeric(), Acc_Valid=numeric())

# A. Logística Multinomial
m_mn <- multinom(f_mod, data=train, trace=FALSE)
acc_tr <- mean(predict(m_mn, train) == train$satvida)
acc_va <- mean(predict(m_mn, valid) == valid$satvida)
resumen[1,] <- c("Multinomial", round(acc_tr,3), round(acc_va,3))

# B. SVM (LINEAL VS RADIAL)
t_lin <- tune(svm, f_mod, data=train, kernel="linear", ranges=list(cost=c(0.1, 1, 10)))
t_rad <- tune(svm, f_mod, data=train, kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100), gamma=c(0.01, 0.1, 1)))

if(t_lin$best.performance < t_rad$best.performance) {
  best_svm <- t_lin$best.model
  nom_svm <- paste0("SVM (Linear, C=", t_lin$best.parameters$cost, ")")
} else {
  best_svm <- t_rad$best.model
  nom_svm <- paste0("SVM (Radial, C=", t_rad$best.parameters$cost, ", G=", t_rad$best.parameters$gamma, ")")
}

acc_tr <- mean(predict(best_svm, train) == train$satvida)
acc_va <- mean(predict(best_svm, valid) == valid$satvida)
resumen[2,] <- c(nom_svm, round(acc_tr,3), round(acc_va,3))

# C. Random Forest
m_rf <- randomForest(f_mod, data=train, ntree=500)
acc_tr <- mean(predict(m_rf, train) == train$satvida)
acc_va <- mean(predict(m_rf, valid) == valid$satvida)
resumen[3,] <- c("Random Forest", round(acc_tr,3), round(acc_va,3))

# D. RNA (OPTIMIZADA)
best_acc <- 0; best_size <- 5
for(s in c(5, 10)){
  mod_tmp <- nnet(f_mod, data=train, size=s, decay=0.1, maxit=500, trace=FALSE)
  acc_tmp <- mean(predict(mod_tmp, valid, type="class") == valid$satvida)
  if(acc_tmp > best_acc){ best_acc <- acc_tmp; best_size <- s }
}
m_rna <- nnet(f_mod, data=train, size=best_size, decay=0.1, maxit=500, trace=FALSE)
acc_tr <- mean(predict(m_rna, train, type="class") == train$satvida)
acc_va <- mean(predict(m_rna, valid, type="class") == valid$satvida)
resumen[4,] <- c(paste0("RNA (Size=",best_size,")"), round(acc_tr,3), round(acc_va,3))

resumen[,2:3] <- lapply(resumen[,2:3], as.numeric); resumen_ordenado <- resumen[order(-resumen$Acc_Valid), ]
resumen_ordenado

# Clasificación NA (GANADOR: RNA Size=5)
modelo_final <- nnet(f_mod, data=base_modelar, size=5, decay=0.1, maxit=1000, trace=FALSE)

# Predecimos usando type="class" para obtener la categoría directamente
prediccion_final <- predict(modelo_final, newdata=base_imputar, type="class")

# Asignamos
base_imputar$satvida_predicha <- prediccion_final
table(base_imputar$satvida_predicha)

bd2$satvida[match(base_imputar$id_fila, bd2$id_fila)] <- prediccion_final

# VERIFICACIÓN FINAL 
print(colSums(is.na(bd2 %>% dplyr::select(dep, coca_12m, mari_12m, alco_12m,genid, educ_harmo ,satvida))))

##### 6. Guardar Base sin NA ####
write_xlsx(bd2, "bd2_sNA.xlsx")

