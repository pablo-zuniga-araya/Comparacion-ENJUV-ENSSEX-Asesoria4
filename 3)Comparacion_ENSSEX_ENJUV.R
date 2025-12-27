#### Paso 3: Análisis comparativo ENSSEX vs ENJUV ####
library(dplyr)
library(survey)
library(readxl)
library(stringr)

rm(list=ls())
options(survey.lonely.psu = "certainty")

# Leer bases regiones 
bd1 <- read_xlsx("bd1_sNA.xlsx") # ENJUV
bd2 <- read_xlsx("bd2_sNA.xlsx") # ENSSEX

# Leer bases nivel pais
b1_nac <- read_xlsx("bd1_pais.xlsx")
b2_nac <- read_xlsx("bd2_pais.xlsx")

# Diagnóstico previo
table(bd1$genid, useNA="always")      
table(bd2$genid, useNA="always")      
table(bd1$educ_harmo, useNA="always") 
table(bd2$educ_harmo, useNA="always") 

#### 1. Armonización de variables ####
# 1.1 ENJUV Regional 
bd1 <- bd1 %>% mutate(
  enc = "ENJUV",
  w = as.numeric(FACTOR), 
  id = as.numeric(CONGLOMERADO), 
  st = as.character(ESTRATO),
  
  # Binarias 
  dep_bin  = as.numeric(dep == 1),
  alco_bin = as.numeric(alco_12m == 1),
  mari_bin = as.numeric(mari_12m == 1),
  coca_bin = as.numeric(coca_12m == 1),
  
  # Sexo
  sexo_final = case_when(str_detect(SEXO, "Hombre")~"Hombre", str_detect(SEXO, "Mujer")~"Mujer", TRUE~NA_character_),
  
  # Género (3 cat)
  genero_3cat = case_when(
    str_detect(genid, "Masculino") ~ "Hombre",
    str_detect(genid, "Femenino") | str_detect(genid, "Femenina") ~ "Mujer",
    TRUE ~ "Otra identidad"
  ),
  
  # Educación (6 cat)
  educ_final = case_when(
    educ_harmo %in% c("Nunca asistió", "Educación especial", "Básica/Primaria") ~ "1. Básica o menos",
    educ_harmo == "Media Científico-Humanista" ~ "2. Media CH",
    educ_harmo == "Media Técnico-Profesional"  ~ "3. Media TP",
    educ_harmo %in% c("Técnico sup. incompleto", "Técnico sup. completo") ~ "4. Técnico Sup.",
    educ_harmo == "Profesional incompleto" ~ "5. Prof. Incompleto",
    educ_harmo %in% c("Profesional completo", "Postgrado") ~ "6. Prof. Completo/Post",
    TRUE ~ NA_character_
  ),
  
  # Satisfacción (Numérica)
  sat_num = as.numeric(as.character(satvida))
)

# 1.2 ENSSEX Regional 
bd2 <- bd2 %>% mutate(
  enc = "ENSSEX",
  w = as.numeric(w_personas_cal), 
  id = as.numeric(varunit), 
  st = as.character(varstrat),
  
  dep_bin  = as.numeric(dep == 1),
  alco_bin = as.numeric(alco_12m == 1),
  mari_bin = as.numeric(mari_12m == 1),
  coca_bin = as.numeric(coca_12m == 1),
  
  sexo_final = as.character(SEXO),
  
  genero_3cat = case_when(
    str_detect(genid, "Masculino") ~ "Hombre",
    str_detect(genid, "Femenino") ~ "Mujer",
    TRUE ~ "Otra identidad"
  ),
  
  educ_final = case_when(
    educ_harmo %in% c("Básica/Primaria") ~ "1. Básica o menos",
    educ_harmo == "Media Científico-Humanista" ~ "2. Media CH",
    educ_harmo == "Media Técnico-Profesional"  ~ "3. Media TP",
    educ_harmo %in% c("Técnico sup. incompleto", "Técnico sup. completo") ~ "4. Técnico Sup.",
    educ_harmo == "Profesional incompleto" ~ "5. Prof. Incompleto",
    educ_harmo %in% c("Profesional completo", "Postgrado") ~ "6. Prof. Completo/Post",
    TRUE ~ NA_character_
  ),
  
  sat_num = as.numeric(as.character(satvida))
)

# 1.3 Nacionales (Solo armonizar nombres para survey) 
b1_nac <- b1_nac %>% mutate(w=as.numeric(FACTOR), id=as.numeric(CONGLOMERADO), st=as.character(ESTRATO), 
                            dep_bin=as.numeric(dep==1), alco_bin=as.numeric(alco_12m==1), mari_bin=as.numeric(mari_12m==1), coca_bin=as.numeric(coca_12m==1), sat_num=as.numeric(as.character(satvida)))
b2_nac <- b2_nac %>% mutate(w=as.numeric(w_personas_cal), id=as.numeric(varunit), st=as.numeric(varstrat), 
                            dep_bin=as.numeric(dep==1), alco_bin=as.numeric(alco_12m==1), mari_bin=as.numeric(mari_12m==1), coca_bin=as.numeric(coca_12m==1), sat_num=as.numeric(as.character(satvida)))

# Diagnóstico variables colapsadas
table(bd1$genero_3cat, useNA="always")
table(bd2$genero_3cat, useNA="always")
table(bd1$educ_final, useNA="always")
table(bd2$educ_final, useNA="always")

# Diseños Muestrales
des1 <- svydesign(ids=~id, strata=~st, weights=~w, data=bd1, nest=TRUE)
des2 <- svydesign(ids=~id, strata=~st, weights=~w, data=bd2, nest=TRUE)
des1_nac <- svydesign(ids=~id, strata=~st, weights=~w, data=b1_nac, nest=TRUE)
des2_nac <- svydesign(ids=~id, strata=~st, weights=~w, data=b2_nac, nest=TRUE)

# Stack para Tests Wald 
cols <- c("dep_bin", "alco_bin", "mari_bin", "coca_bin", "genero_3cat", "educ_final", "sat_num", "REGION", "sexo_final", "enc", "w", "id", "st")
b1_s <- bd1 %>% dplyr::select(all_of(cols))
b2_s <- bd2 %>% dplyr::select(all_of(cols))
stack <- bind_rows(b1_s, b2_s) %>% mutate(w_norm = w*(n()/sum(w)), id_s=interaction(enc,id), st_s=interaction(enc,st))
des_stack <- svydesign(ids=~id_s, strata=~st_s, weights=~w_norm, data=stack, nest=TRUE)

#### 2. Tratamiento de depresión ####
# Referencia Nacional
mn_dep1 <- svymean(~dep_bin, des1_nac, na.rm=T) # ENJUV Nac
mn_dep2 <- svymean(~dep_bin, des2_nac, na.rm=T) # ENSSEX Nac

##### 2.1 Tarapacá ####
reg <- "1 Región de Tarapacá"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~dep_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~dep_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(dep_bin ~ enc, sS) 

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100) # ENJUV Reg
c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_dep1)*100, IC_Pais=confint(mn_dep1)*100) # ENJUV Pais
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100) # ENSSEX Reg
c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_dep2)*100, IC_Pais=confint(mn_dep2)*100) # ENSSEX Pais

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~dep_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~dep_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(dep_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~dep_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~dep_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(dep_bin ~ enc, sSm)

##### 2.2 Biobio ####
reg <- "8 Región del Biobío"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~dep_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~dep_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(dep_bin ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100) # ENJUV Reg
c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_dep1)*100, IC_Pais=confint(mn_dep1)*100) # ENJUV Pais
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100) # ENSSEX Reg
c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_dep2)*100, IC_Pais=confint(mn_dep2)*100) # ENSSEX Pais

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~dep_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~dep_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(dep_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~dep_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~dep_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(dep_bin ~ enc, sSm)

#### 3. Consumo de substancias (Cocaína) ####
# Referencia Nacional
mn_coc1 <- svymean(~coca_bin, des1_nac, na.rm=T) # ENJUV Nac
mn_coc2 <- svymean(~coca_bin, des2_nac, na.rm=T) # ENSSEX Nac

##### 3.1 Tarapacá (Cocaína) ####
reg <- "1 Región de Tarapacá"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~coca_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~coca_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(coca_bin ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100); c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_coc1)*100, IC_Pais=confint(mn_coc1)*100) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100); c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_coc2)*100, IC_Pais=confint(mn_coc2)*100) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~coca_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~coca_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(coca_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~coca_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~coca_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(coca_bin ~ enc, sSm)

##### 3.2 Biobío (Cocaína) ####
reg <- "8 Región del Biobío"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~coca_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~coca_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(coca_bin ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100); c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_coc1)*100, IC_Pais=confint(mn_coc1)*100) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100); c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_coc2)*100, IC_Pais=confint(mn_coc2)*100) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~coca_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~coca_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(coca_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~coca_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~coca_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(coca_bin ~ enc, sSm)

#### 4. Consumo de substancias (Marihuana) ####
# Referencia Nacional
mn_mar1 <- svymean(~mari_bin, des1_nac, na.rm=T) # ENJUV Nac
mn_mar2 <- svymean(~mari_bin, des2_nac, na.rm=T) # ENSSEX Nac

##### 4.1 Tarapacá (Marihuana) ####
reg <- "1 Región de Tarapacá"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~mari_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~mari_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(mari_bin ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100); c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_mar1)*100, IC_Pais=confint(mn_mar1)*100) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100); c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_mar2)*100, IC_Pais=confint(mn_mar2)*100) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~mari_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~mari_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(mari_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~mari_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~mari_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(mari_bin ~ enc, sSm)

##### 4.2 Biobío (Marihuana) ####
reg <- "8 Región del Biobío"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~mari_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~mari_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(mari_bin ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100); c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_mar1)*100, IC_Pais=confint(mn_mar1)*100) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100); c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_mar2)*100, IC_Pais=confint(mn_mar2)*100) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~mari_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~mari_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(mari_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~mari_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~mari_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(mari_bin ~ enc, sSm)

#### 5. Consumo de substancias (Alcohol) ####
# Referencia Nacional
mn_alc1 <- svymean(~alco_bin, des1_nac, na.rm=T) # ENJUV Nac
mn_alc2 <- svymean(~alco_bin, des2_nac, na.rm=T) # ENSSEX Nac

##### 5.1 Tarapacá (Alcohol) ####
reg <- "1 Región de Tarapacá"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~alco_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~alco_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(alco_bin ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100); c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_alc1)*100, IC_Pais=confint(mn_alc1)*100) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100); c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_alc2)*100, IC_Pais=confint(mn_alc2)*100) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~alco_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~alco_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(alco_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~alco_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~alco_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(alco_bin ~ enc, sSm)

##### 5.2 Biobío (Alcohol) ####
reg <- "8 Región del Biobío"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~alco_bin, s1, na.rm=T); c(n=sum(weights(s1)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~alco_bin, s2, na.rm=T); c(n=sum(weights(s2)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(alco_bin ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Prop_Reg=coef(m1)*100, IC_Reg=confint(m1)*100); c(n_Pais=sum(weights(des1_nac)!=0), Prop_Pais=coef(mn_alc1)*100, IC_Pais=confint(mn_alc1)*100) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Prop_Reg=coef(m2)*100, IC_Reg=confint(m2)*100); c(n_Pais=sum(weights(des2_nac)!=0), Prop_Pais=coef(mn_alc2)*100, IC_Pais=confint(mn_alc2)*100) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~alco_bin, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~alco_bin, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(alco_bin ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~alco_bin, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Prop=coef(m1)*100, SE=SE(m1)*100, IC=confint(m1)*100) # ENJUV
m2 <- svymean(~alco_bin, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Prop=coef(m2)*100, SE=SE(m2)*100, IC=confint(m2)*100) # ENSSEX
svyttest(alco_bin ~ enc, sSm)

#### 6. Autoidentificación de género ####
##### 6.1 Tarapacá ####
reg <- "1 Región de Tarapacá"
sS <- subset(des_stack, REGION==reg)
round(prop.table(svytable(~genero_3cat + enc, sS), margin=2)*100, 2)
svychisq(~genero_3cat + enc, sS)

##### 6.2 Biobío ####
reg <- "8 Región del Biobío"
sS <- subset(des_stack, REGION==reg)
round(prop.table(svytable(~genero_3cat + enc, sS), margin=2)*100, 2)
svychisq(~genero_3cat + enc, sS)

#### 7. Nivel educacional ####
##### 7.1 Tarapacá ####
reg <- "1 Región de Tarapacá"
sS <- subset(des_stack, REGION==reg)
# A) Total
round(prop.table(svytable(~educ_final + enc, sS), margin=2)*100, 2)
svychisq(~educ_final + enc, sS)
# C) Hombres
sSh <- subset(sS, sexo_final=="Hombre")
round(prop.table(svytable(~educ_final + enc, sSh), margin=2)*100, 2)
svychisq(~educ_final + enc, sSh)
# D) Mujeres
sSm <- subset(sS, sexo_final=="Mujer")
round(prop.table(svytable(~educ_final + enc, sSm), margin=2)*100, 2)
svychisq(~educ_final + enc, sSm)

##### 7.2 Biobío ####
reg <- "8 Región del Biobío"
sS <- subset(des_stack, REGION==reg)
# A) Total
round(prop.table(svytable(~educ_final + enc, sS), margin=2)*100, 2)
svychisq(~educ_final + enc, sS)
# C) Hombres
sSh <- subset(sS, sexo_final=="Hombre")
round(prop.table(svytable(~educ_final + enc, sSh), margin=2)*100, 2)
svychisq(~educ_final + enc, sSh)
# D) Mujeres
sSm <- subset(sS, sexo_final=="Mujer")
round(prop.table(svytable(~educ_final + enc, sSm), margin=2)*100, 2)
svychisq(~educ_final + enc, sSm)

#### 8. Nivel satisfacción de vida ####
# Referencia Nacional
mn_sat1 <- svymean(~sat_num, des1_nac, na.rm=T) # ENJUV Nac
mn_sat2 <- svymean(~sat_num, des2_nac, na.rm=T) # ENSSEX Nac

##### 8.1 Tarapacá ####
reg <- "1 Región de Tarapacá"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~sat_num, s1, na.rm=T); c(n=sum(weights(s1)!=0), Media=coef(m1), SE=SE(m1), IC=confint(m1)) # ENJUV
m2 <- svymean(~sat_num, s2, na.rm=T); c(n=sum(weights(s2)!=0), Media=coef(m2), SE=SE(m2), IC=confint(m2)) # ENSSEX
svyttest(sat_num ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Media_Reg=coef(m1), IC_Reg=confint(m1)); c(n_Pais=sum(weights(des1_nac)!=0), Media_Pais=coef(mn_sat1), IC_Pais=confint(mn_sat1)) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Media_Reg=coef(m2), IC_Reg=confint(m2)); c(n_Pais=sum(weights(des2_nac)!=0), Media_Pais=coef(mn_sat2), IC_Pais=confint(mn_sat2)) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~sat_num, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Media=coef(m1), SE=SE(m1), IC=confint(m1)) # ENJUV
m2 <- svymean(~sat_num, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Media=coef(m2), SE=SE(m2), IC=confint(m2)) # ENSSEX
svyttest(sat_num ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~sat_num, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Media=coef(m1), SE=SE(m1), IC=confint(m1)) # ENJUV
m2 <- svymean(~sat_num, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Media=coef(m2), SE=SE(m2), IC=confint(m2)) # ENSSEX
svyttest(sat_num ~ enc, sSm)

##### 8.2 Biobío ####
reg <- "8 Región del Biobío"
s1 <- subset(des1, REGION==reg); s2 <- subset(des2, REGION==reg); sS <- subset(des_stack, REGION==reg)

# A) Comparación entre encuestas (Total)
m1 <- svymean(~sat_num, s1, na.rm=T); c(n=sum(weights(s1)!=0), Media=coef(m1), SE=SE(m1), IC=confint(m1)) # ENJUV
m2 <- svymean(~sat_num, s2, na.rm=T); c(n=sum(weights(s2)!=0), Media=coef(m2), SE=SE(m2), IC=confint(m2)) # ENSSEX
svyttest(sat_num ~ enc, sS)

# B) Comparación Regional vs País
c(n_Reg=sum(weights(s1)!=0), Media_Reg=coef(m1), IC_Reg=confint(m1)); c(n_Pais=sum(weights(des1_nac)!=0), Media_Pais=coef(mn_sat1), IC_Pais=confint(mn_sat1)) # ENJUV
c(n_Reg=sum(weights(s2)!=0), Media_Reg=coef(m2), IC_Reg=confint(m2)); c(n_Pais=sum(weights(des2_nac)!=0), Media_Pais=coef(mn_sat2), IC_Pais=confint(mn_sat2)) # ENSSEX

# C) Por Género (Hombres)
s1h <- subset(s1, sexo_final=="Hombre"); s2h <- subset(s2, sexo_final=="Hombre"); sSh <- subset(sS, sexo_final=="Hombre")
m1 <- svymean(~sat_num, s1h, na.rm=T); c(n=sum(weights(s1h)!=0), Media=coef(m1), SE=SE(m1), IC=confint(m1)) # ENJUV
m2 <- svymean(~sat_num, s2h, na.rm=T); c(n=sum(weights(s2h)!=0), Media=coef(m2), SE=SE(m2), IC=confint(m2)) # ENSSEX
svyttest(sat_num ~ enc, sSh)

# D) Por Género (Mujeres)
s1m <- subset(s1, sexo_final=="Mujer"); s2m <- subset(s2, sexo_final=="Mujer"); sSm <- subset(sS, sexo_final=="Mujer")
m1 <- svymean(~sat_num, s1m, na.rm=T); c(n=sum(weights(s1m)!=0), Media=coef(m1), SE=SE(m1), IC=confint(m1)) # ENJUV
m2 <- svymean(~sat_num, s2m, na.rm=T); c(n=sum(weights(s2m)!=0), Media=coef(m2), SE=SE(m2), IC=confint(m2)) # ENSSEX
svyttest(sat_num ~ enc, sSm)