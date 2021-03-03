### Descriptivos ====

# Lectura ==== 
encuesta_raw <- readxl::read_xlsx("./data/1_raw/COVID19_Docentes_results_text.xlsx")
encuesta <- readxl::read_xlsx("./data/3_final/encuesta.xlsx")

# X3_acuerdo - Docentes que accedieron a contestar la encuesta
encuesta_raw %>%
  count(CVDCSINF1) %>%
  mutate(prop = n/sum(n)) %>%
  rename(respuesta = CVDCSINF1) %>%
  ggplot(aes(x = "", y = prop, fill = respuesta)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(y = sort(prop, decreasing = T), label = scales::percent(prop)), 
            position = position_fill(vjust = 1.05)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.ticks.x = element_blank()) + 
  labs(title = "Porcentaje de docentes que aceptaron contestar\nla encuesta", 
       subtitle = "n = 3626",
       x = "",
       y = "%",
       fill = "Respuesta")
encuesta_raw %>%
  count(CVDCSINF1) %>%
  mutate(prop = n/sum(n)) %>%
  rename(respuesta = CVDCSINF1)

# X5_alum_num - Número de alumnos por docente en el ciclo Marzo-Junio 2020
colors <- c("Mediana" = "red", "Media" = "black")
ggplot(encuesta, aes(X5_alum_num)) +
  geom_histogram(binwidth = 5, col = "blue", fill = "white") +
  geom_vline(aes(xintercept = median(X5_alum_num), col = "Mediana"), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(X5_alum_num), col = "Media"), linetype = "dashed") +
  scale_x_continuous(breaks = c(0, round(median(encuesta$X5_alum_num)), 
                                round(mean(encuesta$X5_alum_num)), 60, 100, 150, 200)) +
  labs(title = "Número de alumnos por docente\nen el ciclo Marzo-Junio 2020",
       subtitle = paste("Total de alumnos =", sum(encuesta$X5_alum_num), sep = ""),
       x = "Número de alumnos",
       y = "Frecuencia",
       color = "Medidas\nCentrales") +
  scale_color_manual(values = colors)

# X6_mismo_alum_num
op_one_plot("X6_mismo_alum_num", titulo = "Porcentaje de docentes que reportaron la\nmisma cantidad de alumnos en el período Marzo-Junio 2020")

# x7_mismo_grupo 
op_one_plot("X7_mismo_grupo", titulo = "Porcentaje de docentes que reportaron tener el\nmismo grupo que en el ciclo 2020-2021")

# X8_rec_disp - Recursos disponibles de los docentes para impartir clases en línea
op_mul_plot(6, 11, titulo = "Recursos disponibles de los docentes\npara impartir clases en línea")

# x9_internet_vel - Velocidad de internet de los docentes
op_one_plot("X9_internet_vel", "Velocidad de internet reportada por los docentes")

# X10_doc_tipo_prblm - Tipos de problemas que enfrentaron los docentes #################
op_mul_plot(13, 18, "Tipos de problemas que enfrentaron los docentes", ocur = T)

# X11_est_tipo_prbl - Tipos de problemas que enfrentaron los estudiantes
op_mul_plot(19, 24, "Tipos de problemas que enfrentaron los estudiantes", ocur  = T)

# X12_prep_dist - Percepción de los docentes sobre su preparación para impartir clases a distancia
op_one_plot("X12_prep_dist", "Percepción de los docentes sobre su preparación para\nimpartir clases a distancia")

# X13_doc_cap - Docentes que recibieron capacitación
op_one_plot("X13_doc_cap", "Porcentaje de docentes que\nrecibieron capacitación")

# X14_tipo_cap_reci - Tipo de capacitación que recibieron los docentes
op_mul_plot(27, 30, "Tipo de capacitación que recibieron los docentes", ocur  = T)

# X15_doc_cap_suf - Percepción de los docentes sobre la calidad de la capacitación
op_one_plot("X15_doc_cap_suf", "Percepción de los docentes\nsobre la calidad de la capacitación")

# X16_tipo_cap_req - Tipo de capacitación que los docentes consideran requerir ####################
op_mul_plot(32, 38, "Tipo de capacitación que los\ndocentes consideran requerir", ocur = T)

# X17_lin_acc_reco - Líneas de acción que reconocen los docentes sobre la estrategia "Aprende en Casa"
op_mul_plot(39, 46, 'Líneas de acción que reconocen los docentes\nsobre la estrategia "Aprende en Casa"')

# X18_lin_acc_uti - Líneas de acción que utilizaron los docentes
op_mul_plot(47, 54, "Líneas de acción que utilizaron los docentes")

# X19_lin_acc_fun - Líneas de acción que resultaron más funcionales
op_mul_plot(55, 62, "Líneas de acción que resultaron más funcionales")

# X20_doc_carp - Porcentaje de docentes que solicitó a los alumnos una carpeta de experiencias
op_one_plot("X20_doc_carp", "Porcentaje de docentes que solicitó a los\nalumnos una carpeta de experiencias")

# X21_carp_man_uti - Maneras en que los docentes utilizaron la carpeta de experiencias
op_mul_plot(64, 70, "Maneras en que los docentes utilizaron\nla carpeta de experiencias")

# X22_doc_carp_func - Percepción de los docentes sobre la funcionalidad de la carpeta de experiencias para el nuevo ciclo escolar
op_one_plot("X22_doc_carp_func", "Percepción de los docentes sobre la\nfuncionalidad de la carpeta de experiencias\npara el nuevo ciclo escolar")

# X23_calif_part_padres - Percepción participación padres
op_one_plot("X23_calif_part_padres", "Percepción de los docentes sobre la\nparticipación de los padres")

# X24_calif_part_aut - Percepción participación autoridades
op_one_plot("X24_calif_part_aut", "Percepción de los docentes sobre la\nparticipación de las autoridades")

# X25_1_cal_tv -> X25_7_cal_rep_radio - Aspectos estrategia
encuesta %>%
  select(c(74:80)) %>%
  gather("Reactivo", "Percepción") %>%
  ggplot(aes(x = Reactivo, fill = Percepción)) +
  geom_bar(position = "fill") +
  geom_text(data = . %>% 
              group_by(Reactivo, Percepción) %>%
              tally() %>%
              mutate(prop = round(n / sum(n), 2)) %>%
              ungroup(), aes(y = prop, label = scales::percent(prop)), position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = scales::percent_format()) +
  scale_x_discrete(labels = c("TV", "Radio", "Temas\nTV", "Temas\nRadio", "Horarios", "Repetición\nTV", "Repetición\nRadio")) + 
  labs(title = 'Percepción de los docentes sobre los diferentes\naspectos de la estrategia "Aprende en casa"',
       x = "",
       y = "%")

X25 <- encuesta %>%
  select(c(74:80)) %>%
  gather("Reactivo", "Respuesta")
reactivo <- unique(X25$Reactivo)
respuesta <- unique(X25$Respuesta) %>% sort(.)

X25_frec <- as.data.frame(sapply(reactivo, function(i){
  reactivo_x <- X25 %>% filter(Reactivo == i)
  sapply(respuesta, function(j){
    opciones_x <- sum(reactivo_x == j)
  })
})) %>%
  rename(Calidad.contenido.TV = 1, Calidad.contenido.Radio = 2, 
         Conexión.contenido.clases.TV = 3, Conexión.contenido.clases.Radio = 4,
         Horarios.abordan.temas = 5, Opciones.repetición.TV = 6,
         Opciones.repetición.Radio = 7) 
kable(X25_frec)

# X26_doc_hrs_clase - Horas a la semana para impartir clases
encuesta %>%
  mutate(menos_dos_hrs = ifelse(X26_doc_hrs_clase < 2, 1, 0),
         dos_hrs = ifelse(X26_doc_hrs_clase == 2, 1, 0),
         tres_cuatro_hrs = ifelse(X26_doc_hrs_clase >= 3 & X26_doc_hrs_clase <= 4, 1, 0),
         cinco_seis_hrs = ifelse(X26_doc_hrs_clase >= 5 & X26_doc_hrs_clase <= 6, 1, 0),
         siete_ocho_hrs = ifelse(X26_doc_hrs_clase >= 7 & X26_doc_hrs_clase <= 8, 1, 0),
         mas_ocho_hrs = ifelse(X26_doc_hrs_clase > 8, 1, 0)) %>%
  select(c(168:173)) %>%
  gather("cat", "hrs") %>%
  filter(hrs != 0) %>%
  select(cat) %>%
  do(as.data.frame(table(.$cat))) %>%
  mutate(prop = round(Freq/3588, 2)) %>%
  ggplot(aes(x = prop, y = reorder(Var1, -prop), fill = Var1)) + 
  geom_bar(stat = "identity", show.legend = F) + 
  geom_text(aes(x = prop, label = scales::percent(prop)), position = position_nudge(x = 0.08), size = 3) + 
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_discrete(labels = c("Más de 8 hrs", "5 a 6 hrs", "7 a 8 hrs", 
                              "3 a 4 hrs", "2 hrs", "Menos de 2 hrs")) +
  labs(title = "Horas por semana que dedicaron los docentes\na la impartición de clases",
       x = "%",
       y = "")

# X27_percep_jornada - Percepción de los docentes sobre la jornada laboral
op_one_plot("X27_percep_jornada", "Percepción de los docentes sobre la jornada laboral")

# X28_adap_cont - Percepción de los docentes sobre la adaptación de los contenidos
op_one_plot("X28_adap_cont", "Percepción de los docentes sobre la adaptación de los contenidos")

# X29_desafio_impartir - Desafíos para la impartición de clases #######################################################
op_mul_plot(84, 94, "Desafíos para la impartición de clases")

# X30_opc_prom_apr - Recursos que promovieron el aprendizaje según docentes
op_mul_plot(96, 102, "Recursos que promovieron el aprendizaje según docentes")

# X31_porc_real_act - Porcentaje de estudiantes que realizaron actividades
encuesta %>%
  ggplot(aes(X31_porc_real_act)) +
  geom_histogram(bins = 10, color = "blue", fill = "white") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Porcentaje de estudiantes que realizaron actividades",
       y = "Frecuencia",
       x = "% de estudiantes")

data.frame(Porcentaje.de.alumnos = sort(unique(encuesta$X31_porc_real_act)),
           Frecuencia = sapply(sort(unique(encuesta$X31_porc_real_act)), function(i){
             freq = sum(encuesta$X31_porc_real_act == i)
           })) %>% 
  mutate(Proporción = round(Frecuencia/sum(Frecuencia), 4))

as.data.frame(cbind(table(cut(encuesta$X31_porc_real_act, 
                              breaks= c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))))) %>%
  rename(Frecuencias = V1) %>%
  mutate(Porcentaje.de.alumnos = c("0 a 10%", "11 a 20%", "21 a 30%", "31 a 40%", "41 a 50%", 
                                   "51 a 60%", "61 a 70%", "71 a 80%", "81 a 90%", "91 a 100%"),
         Proporción = round(Frecuencias/sum(Frecuencias), 4)) %>%
  select(Porcentaje.de.alumnos, Frecuencias, Proporción)

# X32_porc_entr_trab - Porcentaje de estudiantes que entregaron trabajos
encuesta %>%
  ggplot(aes(X32_porc_entr_trab)) +
  geom_histogram(bins = 10, color = "blue", fill = "white") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Porcentaje de estudiantes que entregaron trabajos",
       y = "Frecuencia",
       x = "% de estudiantes")

# X33_porc_comun - Porcentaje de estudiantes que se comunicaron con los docentes
encuesta %>%
  ggplot(aes(X33_porc_comun)) +
  geom_histogram(bins = 10, color = "blue", fill = "white") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Porcentaje de estudiantes que se comunicaron con el docente",
       y = "Frecuencia",
       x = "% de estudiantes")

# X34_freq_cont_dudas - Frecuencia de contacto de los alumnos hacia el docente sobre diferentes aspectos
encuesta %>%
  select(c(107:109)) %>%
  gather("Reactivo", "Percepción") %>%
  ggplot(aes(x = Reactivo, fill = Percepción)) +
  geom_bar(position = "fill") +
  geom_text(data = . %>% 
              group_by(Reactivo, Percepción) %>%
              tally() %>%
              mutate(prop = round(n / sum(n), 2)) %>%
              ungroup(), aes(y = prop, label = scales::percent(prop)), position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
  scale_x_discrete(labels = c("Dudas", "Impartir\nclases", "Acompañamiento\nemocional")) + 
  labs(title = 'Frecuencia de contacto de los alumnos hacia el docente\nsobre diferentes aspectos',
       x = "",
       y = "%")

X34 <- as.data.frame(encuesta %>%
                       select(c(107:109)) %>%
                       gather("Reactivo", "Percepción"))
opciones <- unique(X34$Percepción)
reactivos <- unique(X34$Reactivo)
# c("Dudas", "Impartir.clases", "Acompañamiento.emocional")

as.data.frame(sapply(reactivos, function(i){
  reactivo_x <- X34 %>% filter(Reactivo == i)
  sapply(opciones, function(j){
    opciones_x <- sum(reactivo_x == j)
  })
})) %>%
  rename(Dudas = X34_1_freq_cont_dudas,
         Impartir.clases = X34_2_freq_cont_clases,
         Acompañamiento.emocional = X34_3_freq_cont_emoc)


# X35_medio_cont - Herramientas que usaron los docentes como medio de contacto ##############################
op_mul_plot(110, 115, "Herramientas que usaron los docentes como medio de contacto", ocur = T)

# X36_sit_preoc - Situaciones que han preocupado más a los docentes #########################################
op_mul_plot(117, 128, "Situaciones que han preocupado más a los docentes", ocur = T)

# X37_edo_animo - Estado de ánimo de los docentes ##########################
op_mul_plot(129, 128, "Estado de ánimo de los docentes", ocur = T)

# X38_imp_socioemoc - Percepción de los docentes sobre la importancia del acompañamiento emocional de los alumnos contra el contenido de las asignaturas
op_one_plot("X38_imp_socioemoc", "Percepción de los docentes sobre la importancia\ndel acompañamiento emocional de los alumnos contra el contenido de las asignaturas")

# X39_brind_acopemoc - Percepción de los docentes sobre el acompañamiento emocional que dieron a sus alumnos 
op_one_plot("X39_brind_acopemoc", "Percepción de los docentes sobre el\nacompañamiento emocional que dieron a sus alumnos")

# X40_calif_su - Autocalificación de los docentes sobre la calidad de sus enseñanzas
op_one_plot("X40_calif_su", "Autocalificación de los docentes sobre la calidad de sus enseñanzas")

# X41_acuer_aprcasa - Aprobación de los docentes sobre la estrategia "Aprende en Casa"
op_one_plot("X41_acuer_aprcasa", 'Aprobación de los docentes sobre la estrategia "Aprende en Casa"')

# X42_efec_aprcasa - Efectividad de la estrategia "Aprende en casa" según los docentes
op_one_plot("X42_efec_aprcasa", 'Efectividad de la estrategia "Aprende en casa" según los docentes')

# X43_porc_logro_apr - Porcentaje de logro de los estudiantes
op_one_plot("X43_porc_logro_apr", "Porcentaje de logro de los estudiantes")

# X44_1_acceso -> X44_7_acceso - Acceso de los estudiantes a diferentes recursos
encuesta %>%
  select(c(145:151)) %>%
  gather("Reactivo", "Percepción") %>%
  ggplot(aes(x = Reactivo, fill = Percepción)) +
  geom_bar(position = "fill") +
  geom_text(data = . %>%
              group_by(Reactivo, Percepción) %>%
              tally() %>%
              mutate(prop = round(n/sum(n), 2)) %>%
              ungroup(), aes(y = prop, label = scales::percent(prop)), position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
  scale_x_discrete(labels = c("Programas de TV", "Youtube", "Redio", "Distribución\ncuadernillos\ntrabajo", "Sitio de\ninternet", "Educatel", "Correo\nelectrónico")) + 
  labs(title = 'Frecuencia de contacto de los alumnos hacia el docente\nsobre diferentes aspectos',
       x = "",
       y = "%")

X44 <- encuesta %>%
  select(c(145:151)) %>%
  gather("Reactivo", "Respuesta")
reactivo <- unique(X44$Reactivo)
respuesta <- unique(X44$Respuesta) %>% sort(.)

X44_frec <- as.data.frame(sapply(reactivo, function(i){
  reactivo_x <- X44 %>% filter(Reactivo == i)
  sapply(respuesta, function(j){
    opciones_x <- sum(reactivo_x == j)
  })
})) %>%
  rename(Acceso.TV = 1, Acceso.Youtube = 2, Acceso.Radio = 3,
         Acceso.cuadernillos.trabajo = 4, Acceso.sitio.internet = 5,
         Acceso.educatel = 6, Acceso.correo.electrónico = 7)

summarise_all(X44_frec, funs(round(./sum(.), 4))) %>% mutate(Respuesta = c(respuesta)) %>% select(8, 1:7)
# X45_princ_estr - Principal estrategia a seguir que consideran los docentes 
op_one_plot("X45_princ_estr", "Principal estrategia a seguir que consideran los docentes")

# X46_acuer_clases_dist - Grado de acuerdo con las clases a distancia
op_one_plot("X46_acuer_clases_dist", "Grado de acuerdo con las clases a distancia")

# X47_eval_apr_dist - Grado de acuerdo con la evaluación del aprendizaje de los alumnos durante el período de educación a distancia
op_one_plot("X47_eval_apr_dist", "Grado de acuerdo con la evaluación del aprendizaje de los\nalumnos durante el período de educación a distancia")

# X48_ajus_apr_esp - ¿Se deberían ajustar los aprendizajes esperados?
op_one_plot("X48_ajus_apr_esp", "Se deberían ajustar los aprendizajes esperados?")

# X49_sexo - Proporci+on de género
op_one_plot("X49_sexo", "Proporción de género")

# X50_edad - Distribución de edad
encuesta %>%
  ggplot(aes(X50_edad)) +
  geom_histogram(bins = 20, color = "blue", fill = "white") + 
  labs(title = "Distribución de edad de los docentes",
       x = "Edad",
       y = "Frecuencia")

# X51_edo_civ - Estado Civil
op_one_plot("X51_edo_civ", "Estado Civil")

# X52_hijos - Hijos
op_one_plot("X52_hijos", "Hijos")

# X53_ult_g_esc - Último grado de estudios
op_one_plot("X53_ult_g_esc", "Último grado de estudios")

# X54_ult_g_esc_otro - 
op_one_plot("X54_ult_g_esc_otro", "Último grado de estudios (otro)")

# X55_años_exp
encuesta %>%
  ggplot(aes(X55_años_exp)) +
  geom_histogram(bins = 20, color = "blue", fill = "white") + 
  labs(title = "Años de experiencia",
       x = "Años",
       y = "Frecuencia")

# X56_grado_clases
op_one_plot("X56_grado_clases", "Grado al que imparten clases")

# X57_ZONAESC
op_one_plot("X57_ZONAESC", "Zona escolar")

#58_CCT
op_one_plot("X58_CCT", "CCT")

# Rangos ----

# Edad 
rangos <- cut(encuesta$X50_edad, breaks = c(20, 30, 40, 50, 60, 100), right = T)

as.data.frame(cbind(table(rangos))) %>% 
  rename(Frecuencia = V1) %>% 
  mutate(Proporción = round(Frecuencia / sum(Frecuencia), 4), 
         Edad = c("20 a 30", "31 a 40", "41 a 50", "51 a 60", "   > 61")) %>%
  select(Edad, Frecuencia, Proporción)

round(mean(encuesta$X50_edad))

# Experiencia 
rangos <- cut(encuesta$X55_años_exp, breaks = c(0, 5, 10, 20, 30, 40, 100), right = T)

as.data.frame(cbind(table(rangos))) %>%
  rename(Frecuencia = V1) %>%
  mutate(Proporción = round(Frecuencia/sum(Frecuencia), 4),
         "Años de experiencia" = c("0 a  5", "6 a 10", "11 a 20", "21 a 30", "31 a 40", "   > 41")) %>%
  select("Años de experiencia", Frecuencia, Proporción)

round(mean(encuesta$X55_años_exp))

# Estudiantes por grado
grados <- as.character(unique(encuesta$X56_grado_clases))
grados <- sort(paste(c(4, 6, 2, 1, 5, 3), grados, sep = "_")) %>%
  str_remove("[0-9]_")

alumn_num <- sapply(grados, function(i){
  alum_num <- encuesta %>% 
    filter(encuesta$X56_grado_clases == i) %>%
    select(X5_alum_num) %>%
    sum() 
})

as.data.frame(cbind(alumn_num)) %>% 
  mutate(Proporción = round(alumn_num/sum(alumn_num), 4),
         Grado = grados) %>% 
  rename("Número de alumnos" = alumn_num) %>% 
  select(Grado, "Número de alumnos", Proporción)