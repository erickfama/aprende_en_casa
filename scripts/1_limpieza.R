### Lectura y limpieza ====

# Librerías ====
library(readxl)
library(tidyverse)
library(knitr)
library(rlang)

# Lectura # ====

encuesta_raw <- suppressWarnings(read_xlsx("./data/1_raw/COVID19_Docentes_results_text.xlsx"))

# Limpieza # ----

encuesta <- encuesta_raw %>% filter(CVDCSINF1 == "Sí") %>% select(SbjNum, c(27:192)) 

# Convertir variables de chr a factores
encuesta <- as.data.frame(unclass(encuesta), stringsAsFactors = T)

 # Cambiar nombre preguntas que no sean de opción múltiple
no_option <- which(is.na(str_extract(names(encuesta), "_O[0-9]+|SbjNum")))
no_option_names <- c("3_acuerdo", "5_alum_num", "6_mismo_alum_num", "7_mismo_grupo",
                     "9_internet_vel", "12_prep_dist", "13_doc_cap", "15_doc_cap_suf",
                     "20_doc_carp", "22_doc_carp_func", "23_calif_part_padres", "24_calif_part_aut",
                     "25_1_cal_tv", "25_2_cal_radio", "25_3_cal_conx_temas_tv", "25_4_cal_conx_temas_radio",
                     "25_5_cal_hora", "25_6_cal_rep_tv", "25_7_cal_rep_radio", "26_doc_hrs_clase",
                     "27_percep_jornada", "28_adap_cont", "29_desafio_impartir_otro", "30_opc_prom_apr_otro",
                     "31_porc_real_act", "32_porc_entr_trab", "33_porc_comun", "34_1_freq_cont_dudas",
                     "34_2_freq_cont_clases", "34_3_freq_cont_emoc", "35_6_medio_cont_otro", "38_imp_socioemoc",
                     "39_brind_acopemoc", "40_calif_su", "41_acuer_aprcasa", "42_efec_aprcasa",
                     "43_porc_logro_apr", "44_1_acceso_tv", "44_2_acceso_yt", "44_3_acceso_radio",
                     "44_4_acceso_cuad", "44_5_acceso_apr.mx", "44_6_acceso_educatel", "44_7_acceso_correoelec",
                     "45_princ_estr", "45_s_princ_estr_otra", "46_acuer_clases_dist", "47_eval_apr_dist",
                     "48_ajus_apr_esp", "49_sexo", "50_edad", "51_edo_civ", 
                     "52_hijos", "53_ult_g_esc", "54_ult_g_esc_otro", "55_años_exp",
                     "56_nombramiento", "57_grado_clases", "58_ZONAESC", "59_SISAAE")
no_option_names <- paste0("X", no_option_names, sep = "")

atención <- "Preguntas 29, 30, 35 , 45 abiertas"
names(encuesta)[no_option] <- no_option_names

# Cambiar nombre a preguntas de opción múltiple
option_mul <- unique(na.exclude(str_extract(names(encuesta), "CVD[A-Z]+")))
option_mul_names <- c("8_rec_disp", "10_doc_tipo_problm", "11_est_tipo_problm",
                      "14_tipo_cap_reci", "16_tipo_cap_req", "17_lin_acc_reco",
                      "18_lin_acc_uti", "19_lin_acc_fun", "21_carp_man_uti",
                      "29_desafio_impartir", "30_opc_prom_apr", "35_medio_cont",
                      "36_sit_preoc", "37_edo_animo")
option_mul_names <- paste0("X", option_mul_names, sep = "")

names_encuesta <- names(encuesta)

for(i in c(1:14)){
  replacement <- str_replace(names_encuesta, option_mul[i], option_mul_names[i])
  names_encuesta <- replacement
}

# Variable X11 presentaba un problema de similitud con el nombre de la variable X10
names_encuesta[19:24] <- str_replace(names_encuesta[19:24], "X10_doc_tipo_problmEST", "X11_est_tipo_problm")
names(encuesta) <- names_encuesta

rm(i, names_encuesta, no_option, no_option_names, option_mul, option_mul_names, replacement)

# Escritura # ====

# Se crea el archivo encuesta que contiene las variables limpias
if(file.exists("./data/3_final/encuesta.xlsx") == F){
  writexl::write_xlsx(encuesta, "./data/3_final/encuesta.xlsx", col_names = T)
}






