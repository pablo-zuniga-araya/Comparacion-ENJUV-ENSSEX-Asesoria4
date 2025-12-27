####Paso 2: Clasificación NA en base ENJUV ####
library(rstudioapi)   # abrir directorio 
library(writexl)
library(dplyr)
library(readxl)
library(stringr)      
library(pROC)         # Para curvas ROC y AUC
library(e1071)        # SVM y función tune()
library(nnet)         # RNA y Multinomial
library(randomForest) # Random Forest

#resetear
rm(list=ls())

#abrir directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#leer base
bd1 <- read_xlsx("BBDD Respuesta - Encuesta Jóvenes.xlsx", sheet = 1)

# 1. CREACIÓN DE VARIABLES (Para toda la base)
bd1 <- bd1 %>%
  mutate(
    id_fila = 1:nrow(bd1),
    
    #tratamiento depresion
    dep = case_when(
      as.character(P103)=="NS/NR" | as.character(P104_1)=="NS/NR" ~ NA_real_,
      as.character(P104_1)=="Sí" ~ 1, as.character(P104_1)%in%c("No","No Aplica") ~ 0, TRUE ~ NA_real_
    ),
    
    #autoidentificación de genero
    genid = case_when(
      str_detect(as.character(P80), "^Masculino") ~ "Masculino",
      str_detect(as.character(P80), "^Femenina") ~ "Femenino",
      str_detect(as.character(P80), "^Transgénero masculino") ~ "Transmasculino u hombre trans",
      str_detect(as.character(P80), "^Transgénero femenina") ~ "Transfemenino o mujer trans",
      str_detect(as.character(P80), "^No binario") ~ "No binario",
      str_detect(as.character(P80), "^Aún estoy explorando") ~ "Otro. Especifique",
      as.character(P80) %in% c("NS/NR", "Prefiere no responder") ~ "Prefiere no responder",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    #consumo de substancias
    coca_12m = case_when(as.character(P76_4)=="Sí"~1, as.character(P76_4)=="No"~0, as.character(P76_4)=="NS/NR"~NA_real_, TRUE~NA_real_),
    mari_12m = case_when(as.character(P76_3)=="Sí"~1, as.character(P76_3)=="No"~0, as.character(P76_3)=="NS/NR"~NA_real_, TRUE~NA_real_),
    alco_12m = case_when(as.character(P76_1)=="Sí"~1, as.character(P76_1)=="No"~0, as.character(P76_1)=="NS/NR"~NA_real_, TRUE~NA_real_),
    
    #nivel educacional (CORREGIDO: Desagregado para coincidir con código antiguo)
    p14_num = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P14)))),
    educ_harmo = case_when(
      p14_num == 1 ~ "Nunca asistió",
      p14_num %in% c(2,3) ~ "Preescolar",
      p14_num == 4 ~ "Educación especial",
      p14_num %in% c(5,6) ~ "Básica/Primaria",
      p14_num %in% c(7,8) ~ "Media Científico-Humanista",
      p14_num %in% c(9,10) ~ "Media Técnico-Profesional",
      p14_num == 11 ~ "Técnico sup. incompleto",
      p14_num == 12 ~ "Técnico sup. completo",
      p14_num == 13 ~ "Profesional incompleto",
      p14_num == 14 ~ "Profesional completo",
      p14_num %in% c(15,16) ~ "Postgrado",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    #satisfaccion vida 
    satvida = suppressWarnings(as.numeric(str_extract(as.character(P6_1), "^[1-5]"))),
    satvida = ifelse(satvida %in% 1:5, satvida, NA_real_),
    satvida = factor(satvida),
    
    #variables predictoras a usar
    REGION = factor(REGION), SEXO = factor(SEXO), GSE = factor(GSE), EDAD = as.numeric(EDAD),
    P54_num = P54, 
    P5_num    = ifelse(suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P5)))) %in% c(98, 99), NA, suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P5))))),
    P6_1_num  = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P6_1)))),
    P6_2_num  = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P6_2)))),
    P6_3_num  = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P6_3)))),
    P6_4_num  = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P6_4)))),
    P6_6_num  = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P6_6)))),
    P6_7_num  = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P6_7)))),
    P19_1_num = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P19_1)))),
    P19_2_num = suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P19_2)))),
    P35_num   = ifelse(suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P35)))) %in% c(98, 99), NA, suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", as.character(P35)))))
  ) %>% 
  #  FILTRAMOS POBLACIÓN OBJETIVO (Jovenes Urbanos 18-29) PARA TODO EL PAÍS
  filter(EDAD >= 18, EDAD <= 29, ZONA == "1 Urbano")

# Guardar base nacional para comparaciones posteriores
bd1_pais <- bd1 
write_xlsx(bd1_pais, "bd1_pais.xlsx") 

# Filtramos regiones de interes para continuar con la imputación
bd1 <- bd1 %>%
  filter(REGION %in% c("1 Región de Tarapacá","8 Región del Biobío"))

##### 1. Tratamiento depresion####
table(bd1$dep,useNA = "ifany") #ver NAs
datos_dep <- bd1 %>% dplyr::select(id_fila, dep, REGION, EDAD, SEXO, GSE, P54_num, P6_1_num, P6_2_num, P19_1_num) #variables a usar
base_modelar <- datos_dep %>% filter(!is.na(dep)) %>% na.omit()
base_imputar <- datos_dep %>% filter(is.na(dep)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- dep ~ REGION + EDAD + SEXO + GSE + P54_num + P6_1_num + P6_2_num + P19_1_num

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. LOGÍSTICA
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$dep, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$dep,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$dep,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM (LINEAL Y RADIAL)
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
resumen_ordenado #tabla comparativa

# Clasificación NA (Random Forest)
modelo_final <- randomForest(as.factor(dep) ~ ., data=base_modelar, ntree=500)
probs_imputar <- predict(modelo_final, newdata=base_imputar, type="prob")[,2]
roc_total <- roc(base_modelar$dep, predict(modelo_final, base_modelar, type="prob")[,2], quiet=TRUE); th <- coords(roc_total, "best", transpose=TRUE)["threshold"]
clases_imputadas <- ifelse(probs_imputar >= th, 1, 0)
base_imputar$dep_predicho <- clases_imputadas
table(base_imputar$dep_predicho) #clasificacion por caregoria
bd1$dep[match(base_imputar$id_fila, bd1$id_fila)] <- clases_imputadas


##### 2. Consumo de substancias####
###### 2.1 Cocaina####
table(bd1$coca_12m,useNA = "ifany") #ver NAs
datos_coca <- bd1 %>% dplyr::select(id_fila, target=coca_12m, REGION, EDAD, SEXO, GSE, P54_num, P6_1_num, P6_3_num, P6_4_num, P6_6_num, P6_7_num, P19_2_num, P35_num)
base_modelar <- datos_coca %>% filter(!is.na(target)) %>% na.omit()
base_imputar <- datos_coca %>% filter(is.na(target)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- target ~ REGION + EDAD + SEXO + GSE + P54_num + P6_1_num + P6_3_num + P6_4_num + P6_6_num + P6_7_num + P19_2_num + P35_num

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. Logística
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM (LINEAL Y RADIAL)
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
bd1$coca_12m[match(base_imputar$id_fila, bd1$id_fila)] <- clases_imp
table(clases_imp)


###### 2.2 Marihuana ####
table(bd1$mari_12m,useNA = "ifany") #ver NAs
datos_mari <- bd1 %>% dplyr::select(id_fila, target=mari_12m, REGION, EDAD, SEXO, GSE, P54_num, P6_1_num, P6_3_num, P6_4_num, P6_6_num, P6_7_num, P19_2_num, P35_num)
base_modelar <- datos_mari %>% filter(!is.na(target)) %>% na.omit()
base_imputar <- datos_mari %>% filter(is.na(target)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- target ~ REGION + EDAD + SEXO + GSE + P54_num + P6_1_num + P6_3_num + P6_4_num + P6_6_num + P6_7_num + P19_2_num + P35_num

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. Logística
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM (LINEAL VS RADIAL)
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

# C. RNA (OPTIMIZADA)
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
probs_total <- as.numeric(predict(modelo_final, base_modelar, type="raw"))
roc_total   <- roc(base_modelar$target, probs_total, quiet=TRUE)
th          <- coords(roc_total, "best", transpose=TRUE)["threshold"]
probs_imp <- as.numeric(predict(modelo_final, newdata=base_imputar, type="raw"))
clases_imp <- ifelse(probs_imp >= th, 1, 0)
bd1$mari_12m[match(base_imputar$id_fila, bd1$id_fila)] <- clases_imp
table(clases_imp)

###### 2.3 Alcohol ####
table(bd1$alco_12m,useNA = "ifany") #ver NAs
datos_alco <- bd1 %>% dplyr::select(id_fila, target=alco_12m, REGION, EDAD, SEXO, GSE, P54_num, P5_num, P6_1_num, P6_3_num, P6_4_num, P6_6_num, P6_7_num, P35_num)
base_modelar <- datos_alco %>% filter(!is.na(target)) %>% na.omit()
base_imputar <- datos_alco %>% filter(is.na(target)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- target ~ REGION + EDAD + SEXO + GSE + P54_num + P5_num + P6_1_num + P6_3_num + P6_4_num + P6_6_num + P6_7_num + P35_num

resumen <- data.frame(Modelo=character(), AUC_Train=numeric(), Corte=numeric(), Sens_Train=numeric(), Spec_Train=numeric(), Sens_Valid=numeric(), Spec_Valid=numeric(), Acc_Valid=numeric())

# A. Logística
m_glm <- glm(f_mod, data=train, family=binomial())
p_tr <- predict(m_glm, train, type="response"); r_tr <- roc(train$target, p_tr, quiet=TRUE); th <- coords(r_tr, "best", transpose=TRUE)["threshold"]
c_tr <- ifelse(p_tr >= th, 1, 0); tab_tr <- table(factor(train$target,c(0,1)), factor(c_tr,c(0,1)))
p_va <- predict(m_glm, valid, type="response"); c_va <- ifelse(p_va >= th, 1, 0); tab_va <- table(factor(valid$target,c(0,1)), factor(c_va,c(0,1)))
resumen[1,] <- c("Logistica", round(as.numeric(r_tr$auc),3), round(th,3), round(tab_tr[2,2]/sum(tab_tr[2,]),3), round(tab_tr[1,1]/sum(tab_tr[1,]),3), round(tab_va[2,2]/sum(tab_va[2,]),3), round(tab_va[1,1]/sum(tab_va[1,]),3), round(sum(diag(tab_va))/sum(tab_va),3))

# B. SVM (LINEAL VS RADIAL)
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

# C. RNA (OPTIMIZADA)
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

# Clasificación NA (Logística)
modelo_final <- glm(f_mod, data=base_modelar, family=binomial())
probs_imp <- predict(modelo_final, newdata=base_imputar, type="response")
roc_total <- roc(base_modelar$target, predict(modelo_final, base_modelar, type="response"), quiet=TRUE); th <- coords(roc_total, "best", transpose=TRUE)["threshold"]
clases_imp <- ifelse(probs_imp >= th, 1, 0)
bd1$alco_12m[match(base_imputar$id_fila, bd1$id_fila)] <- clases_imp
table(clases_imp)

##### 3. Autoidentificación de genero ####
table(bd1$genid,useNA = "always") #No hay datos faltantes

##### 4. Nivel educacional ####
table(bd1$educ_harmo,useNA = "ifany") 
datos_educ <- bd1 %>% dplyr::select(id_fila, educ_harmo, REGION, EDAD, SEXO, GSE, P54_num, P5_num, P6_1_num, P6_2_num, P6_3_num, P6_4_num, P6_6_num, P6_7_num, P19_1_num, P19_2_num, P35_num)

# Separar base
base_modelar <- datos_educ %>% filter(!is.na(educ_harmo)) %>% na.omit()
base_imputar <- datos_educ %>% filter(is.na(educ_harmo)) 

set.seed(123)
idx   <- sample(1:nrow(base_modelar), size = 0.8 * nrow(base_modelar))
train <- base_modelar[idx, ]; valid <- base_modelar[-idx, ]
f_mod <- educ_harmo ~ . -id_fila

resumen <- data.frame(Modelo=character(), Acc_Train=numeric(), Acc_Valid=numeric())

# A. Logística Multinomial
m_mn <- multinom(f_mod, data=train, trace=FALSE)
acc_tr <- mean(predict(m_mn, train) == train$educ_harmo)
acc_va <- mean(predict(m_mn, valid) == valid$educ_harmo)
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

acc_tr <- mean(predict(best_svm, train) == train$educ_harmo)
acc_va <- mean(predict(best_svm, valid) == valid$educ_harmo)
resumen[2,] <- c(nom_svm, round(acc_tr,3), round(acc_va,3))

# C. Random Forest
m_rf <- randomForest(f_mod, data=train, ntree=500)
acc_tr <- mean(predict(m_rf, train) == train$educ_harmo)
acc_va <- mean(predict(m_rf, valid) == valid$educ_harmo)
resumen[3,] <- c("Random Forest", round(acc_tr,3), round(acc_va,3))

# D. RNA (OPTIMIZADA)
best_acc <- 0; best_size <- 5
for(s in c(5, 10, 15)){
  mod_tmp <- nnet(f_mod, data=train, size=s, decay=0.1, maxit=500, trace=FALSE)
  acc_tmp <- mean(predict(mod_tmp, valid, type="class") == valid$educ_harmo)
  if(acc_tmp > best_acc){ best_acc <- acc_tmp; best_size <- s }
}
m_rna <- nnet(f_mod, data=train, size=best_size, decay=0.1, maxit=500, trace=FALSE)
acc_tr <- mean(predict(m_rna, train, type="class") == train$educ_harmo)
acc_va <- mean(predict(m_rna, valid, type="class") == valid$educ_harmo)
resumen[4,] <- c(paste0("RNA (Size=",best_size,")"), round(acc_tr,3), round(acc_va,3))

resumen[,2:3] <- lapply(resumen[,2:3], as.numeric); resumen_ordenado <- resumen[order(-resumen$Acc_Valid), ]
resumen_ordenado

# Clasificación NA (GANADOR: RNA Size=5)
modelo_final <- nnet(f_mod, data=base_modelar, size=5, decay=0.1, maxit=500, trace=FALSE)

# Predicción directa
prediccion_final <- predict(modelo_final, newdata=base_imputar, type="class")

base_imputar$educ_predicha <- prediccion_final
table(base_imputar$educ_predicha)
bd1$educ_harmo[match(base_imputar$id_fila, bd1$id_fila)] <- prediccion_final

##### 5. Nivel satisfacción de vida ####
table(bd1$satvida,useNA = "ifany") #ver NAs
datos_sat <- bd1 %>% dplyr::select(id_fila, satvida, REGION, EDAD, SEXO, GSE, P54_num, P35_num)
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

# Clasifiación NA (Random Forest)
modelo_final <- randomForest(f_mod, data=base_modelar, ntree=500)
prediccion_final <- predict(modelo_final, newdata=base_imputar)
base_imputar$satvida_predicha <- prediccion_final
table(base_imputar$satvida_predicha)
bd1$satvida[match(base_imputar$id_fila, bd1$id_fila)] <- prediccion_final

# VERIFICACIÓN FINAL 
print(colSums(is.na(bd1 %>% dplyr::select(dep, coca_12m, mari_12m, alco_12m,genid, educ_harmo, satvida))))

##### 6. Guardar Base sin NA ####
write_xlsx(bd1, "bd1_sNA.xlsx")

