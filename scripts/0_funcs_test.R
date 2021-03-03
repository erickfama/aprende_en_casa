specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

tables <- sapply(seq(6,11), function(i){
  table(encuesta[i])
}) %>%
  lapply(as.data.frame)

freqs <- tables %>%
  reduce(full_join,  by = "Var1", suffix = c(".x", ".y")) %>%
  replace(is.na(.), 0) %>%
  mutate(Count = rowSums(.[2:length(.)])) %>%
  rename(Respuesta = Var1) %>%
  filter(Respuesta != -1) %>%
  mutate(Prop = round(Count/nrow(encuesta), 2)) %>%
  select(Respuesta, Count, Prop) %>%
  arrange(-Count, Respuesta)

freqs %>%
  ggplot(aes(x = reorder(Respuesta, - Prop), y = Prop, fill = Respuesta)) + 
  geom_bar(position = "stack", stat = "identity", show.legend = T) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "bottom") +
  coord_flip() + 
  labs(title = titulo, subtitle = "n = 3588", x = "", y = "%")


  vari <- enquo(X6_mismo_alum_num)
  ggplot(data = encuesta, aes(x = "", fill = !!vari)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.ticks.x = element_blank()) + 
  labs(title = "lul", 
       subtitle = "n = 3588",
       x = "",
       y = "%",
       fill = "Respuesta")

  
### Función mul varias respuestas
  
p_inicial <- 74
p_final <- 80
titulo <- ""

encuesta %>%
  select(c(74:80)) %>%
  gather("Reactivo", "Percepción") %>%
  ggplot(aes(x = Reactivo, fill = Percepción)) +
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
  scale_x_discrete(labels = c("TV", "Radio", "Temas TV", "Temas Radio", "Horarios", "Repetición TV", "Repetición Radio")) + 
  labs(title = 'Percepción de los docentes sobre los diferentes\naspectos de la estrategia "Aprende en casa"',
       x = "",
       y = "%")


lul <- sapply(c(110:115), function(n){
  sapply(encuesta[n], function(i){
    str_replace(i, "\\s*\\([^\\)]+\\)", "")
  })
}) %>%
  data.frame(.)

lul <- sapply(encuesta[110:115], function(n){
  sapply(n, function(i){
    str_replace(i, "\\s*\\([^\\)]+\\)", "")
  })
}) %>%
  data.frame(.)

##############################################################
p_inicial <- 13
p_final <- 18
ocur = T
titulo = ""
df1 <- sapply(c(p_inicial:p_final), function(n){
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

df1 %>%
  ggplot(aes(x = CCT, y = prop, fill = str_wrap(respuesta, 20))) +
  geom_bar(stat = "identity", position = "fill", width = 0.3) +
  scale_y_continuous(breaks = c(0, 1), labels = scales::percent_format()) + 
  theme(axis.ticks.x = element_blank()) + 
  geom_text(aes(y = prop, label= scales::percent(prop)),
            position = position_fill(vjust = 0.5), size = 2) +
  coord_flip() +
  labs(title = titulo, 
       subtitle = "n = 3588",
       x = "",
       y = "%",
       fill = "Respuesta")

  if(ocur == T){
    df1[df1 != "-1"] <- 1
    df1[df1 == "-1"] <- 0
    sapply(df1, as.numeric) %>%
      data.frame(.) %>%
      mutate(oc = rowSums(.)) %>%
      count(oc) %>%
      mutate(prop = round(n/sum(n), 2)) %>%
      ggplot(aes(x = as.factor(oc), y = prop, fill = as.factor(oc))) +
      geom_bar(stat = "identity", show.legend = F) + 
      geom_text(aes(y = prop, label = scales::percent(prop)), size = 3, 
                position = position_nudge(y = 0.06)) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
      labs(title = "Porcentaje del número de situaciones que acontecieron los docentes",
           y = "%",
           x = "Número de ocurrencias")
    
    gridExtra::grid.arrange(p, p2)
}

##################################################################
variable = "X47_eval_apr_dist"
titulo = "Grado de acuerdo con la evaluación del aprendizaje de los\nalumnos durante el período de educación a distancia"
v <- sym(variable)

prop <- encuesta %>%
  group_by(X58_CCT) %>%
  count(!!v) %>%
  rename(respuesta = !!v) %>%
  mutate(prop = (round(n/sum(n), 2)))
prop$respuesta <- with(prop, reorder(respuesta, prop))

prop %>%
  ggplot(aes(x = X58_CCT, y = prop, fill = respuesta)) +
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.ticks.x = element_blank()) + 
  geom_text(data = prop, aes(y = prop, label= scales::percent(prop)),
            position = position_fill(vjust = 0.5)) +
  coord_flip() +
  labs(title = titulo,
       x = "",
       y = "%",
       fill = "Respuesta")

arrange(prop, respuesta, -prop)

# Función para obtener las respuestas únicas de las preguntas

resp_uniq <- function(var_num){
  encuesta %>% select(starts_with(var_num)) %>% select(1) %>% unique() %>% pull()
}

# Función para obtener las frecuencias entre dos variables ====

var1_num <- "X56"
var2_num <- "X10"

var1_name <- encuesta %>% select(starts_with(var1_num)) %>% names()
var2_name <- encuesta %>% select(starts_with(var2_num)) %>% names()

df_sel <- encuesta %>%
  select(var1_name, var2_name)

df_sel %>%
  gather(var, Respuesta, -!!rlang::sym(var1_name)) %>%
  filter(Respuesta != -1) %>%
  select(1, 3) %>%
  group_by(!!rlang::sym(var1_name)) %>%
  count(Respuesta, sort = T) %>%
  spread(key = var1_name, value = n) %>%
  replace(is.na(.), 0)