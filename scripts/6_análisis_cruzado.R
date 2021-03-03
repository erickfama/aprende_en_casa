### Cross Tabulation Analysis ====

# Librerías
library(tidyverse)


# Lectura de datos 
encuesta_raw <- readxl::read_xlsx("./data/1_raw/COVID19_Docentes_results_text.xlsx")
encuesta <- readxl::read_xlsx("./data/3_final/encuesta.xlsx")  

# Frecuencia tabulación cruzada

crosstab_frec("X56", "X10")


