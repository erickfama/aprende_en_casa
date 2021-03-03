### Funciones ====

# Descriptivos ====

# Función para graficar respuestas de una opción ====
op_one_plot <- function(variable, titulo){
  v <- sym(variable)
  
  prop <- encuesta %>%
    count(!!v) %>%
    rename(respuesta = !!v) %>%
    mutate(prop = (round(n/sum(n), 2)))
  prop$respuesta <- with(prop, reorder(respuesta, prop))
  
  p <- prop %>%
    ggplot(aes(x = "", y = prop, fill = respuesta)) +
    geom_bar(stat = "identity", position = "fill", width = 0.8) +
    scale_y_continuous(labels = scales::percent_format()) + 
    theme(axis.ticks.x = element_blank()) + 
    geom_text(data = prop, aes(y = prop, label= scales::percent(prop)),
              position = position_fill(vjust = 0.5), ) +
    labs(title = titulo, 
         subtitle = "n = 3588",
         x = "",
         y = "%",
         fill = "Respuesta")
  plot(p)
  kable(arrange(prop, respuesta, -prop))
}

# Función para graficar respuestas de opción múltiple ====
op_mul_plot <- function(p_inicial, p_final, titulo, ocur = F){
  df <- sapply(c(p_inicial:p_final), function(n){
    sapply(encuesta[n], function(i){
      str_replace(i, "\\s*\\([^\\)]+\\)", "")
    })
  }) %>%
    data.frame(.)
  
  tables <- sapply(seq(1, length(df)), function(i){
    table(df[i])
  }) %>%
    lapply(as.data.frame)
  
  freqs <- tables %>%
    reduce(full_join,  by = "Var1", suffix = c(".x", ".y")) %>%
    replace(is.na(.), 0) %>%
    mutate(Count = rowSums(.[2:length(.)])) %>%
    rename(Respuesta = Var1) %>%
    filter(Respuesta != -1) %>%
    mutate(Prop = round(Count/nrow(encuesta), 2)) %>%
    select(Respuesta, Count, Prop) 
  
  freqs$Respuesta <-  with(freqs, reorder(Respuesta, -Prop))
  
  p <- freqs %>%
    ggplot(aes(x = Respuesta, y = Prop, fill = Respuesta)) + 
    geom_bar(position = "stack", stat = "identity", show.legend = T) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          legend.position = "bottom") +
    coord_flip() + 
    geom_text(aes(label = scales::percent(Prop), y = Prop), 
              position = position_nudge(y = 0.06), 
              size = 3) +
    labs(title = titulo, subtitle = "n = 3588", x = "", y = "%")
  
  if(ocur == T){
    df[df != "-1"] <- 1
    df[df == "-1"] <- 0
    df <- sapply(df, as.numeric) %>%
      data.frame(.) %>%
      mutate(oc = rowSums(.)) %>%
      count(oc) %>%
      mutate(prop = round(n/sum(n), 2))
    
    p2 <- df %>%       ggplot(aes(x = as.factor(oc), y = prop, fill = as.factor(oc))) +
      geom_bar(stat = "identity", show.legend = F) + 
      geom_text(aes(y = prop, label = scales::percent(prop)), size = 3, 
                position = position_nudge(y = 0.06)) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
      labs(title = "Porcentaje del número de situaciones que presentaron los individuos",
           y = "%",
           x = "Número de ocurrencias")
    
    gridExtra::grid.arrange(p, p2)
    kable(arrange(freqs, Respuesta, -Prop))
  } else {
    plot(p)
    kable(arrange(freqs, Respuesta, -Prop))
  }
}

# Tabulación cruzada ====

# Función para obtener las frecuencias entre dos variables ====

crosstab_frec <- function(var1_num, var2_num){
  var1_name <- encuesta %>% select(starts_with(var1_num)) %>% names()
  var2_name <- encuesta %>% select(starts_with(var2_num)) %>% names()
  
  df_sel <- encuesta %>%
    select(var1_name, var2_name)
  
  df_sel %>%
    gather(var, Respuesta, -!!rlang::sym(var1_name)) %>%
    filter(Respuesta != -1) %>%
    select(1, 3) %>%
    group_by(!!rlang::sym(var1_name)) %>%
    count(Respuesta) %>%
    spread(key = var1_name, value = n) %>%
    replace(is.na(.), 0)
}