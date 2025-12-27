## Evaluación de Consistencia: ENJUV vs ENSSEX (Tarapacá y Biobío)
Este repositorio contiene el código fuente y los datos necesarios para reproducir el análisis estadístico comparativo entre la 10ª Encuesta Nacional de Juventudes (ENJUV) y la Encuesta Nacional de Salud, Sexualidad y Género (ENSSEX).

### Archivos de Código
1.  **`1)Variables_predictoras_NA_ENJUV.R`**: Seleccion de variables predictoras ENJUV.
2.  **`1)Variables_predictoras_NA_ENSSEX.R`**: Seleccion de variables predictoras ENSSEX.
3.  **`2)Clasificacion_NA_ENJUV.R`**: Imputación de datos faltantes para ENJUV.
4.  **`2)Clasificacion_NA_ENSSEX.R`**: Imputación de datos faltantes para ENSSEX.
5.  **`3)Comparacion_ENSSEX_ENJUV.R`**: evaluacion de consistencia y comparaciones entre ENSSEX y ENJUV .

### Datos de Entrada
* `BBDD Respuesta - Encuesta Jóvenes.xlsx`
* `20241205_enssex_data.RData`

##  Instrucciones de Reproducibilidad
Para replicar los resultados del informe, siga estos pasos estrictos:

### 1. Requisitos
* **Software:** RStudio.
* **Paquetes:** Instalar `tidyverse`, `survey`, `haven`, `readxl`, `writexl`, `randomForest`, `e1071`, `nnet`.

### 2. Ejecución
Descargue este repositorio y extraiga la carpeta. Abra los scripts en RStudio y ejecútelos en el orden numérico estricto (1, 2 y 3).

**Nota:** No mueva los archivos de su carpeta original para garantizar que el código encuentre las bases de datos.

---
**Autor:** Pablo Zúñiga Araya
**Institución:** Universidad de Santiago de Chile
