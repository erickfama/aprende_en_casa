library(tidyverse)
library(knitr)

### Zonas 

# df con las preguntas solicitadas ----

# Claves de las preguntas
vars <- c("CVDHDREC", "CVDCDPROB", "CVDCDCT", "CVDCDCTOTH",
          "CVDCDNCE", "CVDRCLINEAS", "CVDRCLNFUN", "CVDRCARPETA",
          "CVDRCARPETAOT", "CVDCDAPREN/Q_30_S", "CVDINTSTMCT/Q_35_S",
          "CVDSEMPREOC", "CVDSEMEDOAN", "CVDIMPACALIDAD", "T_CVDIMPACEQUIT_1",
          "CVDNCEVAPREN")

# Se extrae el df de ("encuesta") con las claves
sisaae <- encuesta %>%
  select(6:11, 13:18, 27:30, 32:38, 47:70, 96:102, 110:115, 117:118, 141, 145:151, 155, 167)

# Función para crear tabla con preguntas de opción múltiple ----
op_mul_table <- function(p_inicial, p_final, ocur = F){
  df <- sapply(c(p_inicial:p_final), function(n){
    sapply(encuesta[n], function(i){
      str_replace(i, "\\s*\\([^\\)]+\\)", "")
    })
  }) %>%
    data.frame(.) %>%
    mutate(CCT = encuesta$X58_CCT) %>%
    gather(var, respuesta, -CCT) %>%
    select(CCT, respuesta) %>%
    filter(respuesta != -1) %>%
    group_by(CCT, respuesta) %>%
    summarise(n = n()) %>%
    mutate(prop = round(n/sum(n),2))
  df
}

# Función para crear tabla con preguntas de una sola opción. ----

op_one_plot <- function(variable, titulo){
v <- sym(variable)

prop <- encuesta %>%
  group_by(X58_CCT) %>%
  count(!!v) %>%
  rename(respuesta = !!v) %>%
  mutate(prop = (round(n/sum(n), 2)))
prop$respuesta <- with(prop, reorder(respuesta, prop))

prop
}

# 8 - Recursos disponibles
op_mul_plot(6, 11)

# 47 
op_one_plot("X47_eval_apr_dist", "Grado de acuerdo con la evaluación del aprendizaje de los\nalumnos durante el período de educación a distancia")

op_mul_plot <- function(p_inicial, p_final, titulo, ocur = F){
  df <- sapply(c(p_inicial:p_final), function(n){
    sapply(encuesta[n], function(i){
      str_replace(i, "\\s*\\([^\\)]+\\)", "")
    })
  }) %>%
    data.frame(.) %>%
    mutate(SISAAE = encuesta$X58_CCT) %>%
    gather(var, Respuesta, -SISAAE) %>%
    select(SISAAE, Respuesta) %>%
    filter(Respuesta != -1) %>%
    group_by(SISAAE, Respuesta) %>%
    summarise(Frecuencia = n()) %>%
    mutate(Proporción = round(Frecuencia/sum(Frecuencia), 4))
  
  kable(df, booktabs = TRUE, format = "latex", longtable=TRUE)
}

# 44

# Se extraen y da forma a los reactivos y respuestas, además se añade el SISAAE
X44 <- encuesta %>%
  select(c(145:151, 167)) %>%
  rename(SISAAE = X58_CCT) %>%
  gather("Reactivo", "Respuesta", -SISAAE)

# Valores únicos que se usarán para iterar
reactivo <- unique(X44$Reactivo)
respuesta <- unique(X44$Respuesta) %>% sort(.)
sisaae <- unique(X44$SISAAE) %>% sort(.)

# Función para generar las frecuencias
X44_frec <- as.data.frame(sapply(reactivo, function(i){ # Filtrado por Reactivo
    reactivo_x <- X44 %>% filter(Reactivo == i)
      sapply(respuesta, function(j){ # Filtrado por Respuesta
      opciones_x <- sum(reactivo_x == j) # suma de las respuestas por reactivo y SISAAE
  })
})) %>% rename(Acc.TV = 1, Acc.Youtube = 2, Acc.Radio = 3,
         Acc.cuad.trabajo = 4, Acc.sitio.intrnt = 5,
         Acc.educatel = 6, Acc.corr.elect = 7)

kable(X44_frec)

kable(summarise_all(X44_frec, funs(round(./sum(.), 4))) %>% 
        mutate(Respuesta = c(respuesta)) %>% 
        select(8, 1:7))
