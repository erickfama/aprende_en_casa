#******************************************************************************# 
# Código para crear gráficas de la encuesta aplicada a docentes                #
# sobre el programa Aprende en Casa                                            # 
#                                                                              # 
# Depends on:                                                                  #
# Authors: - Manuel Cardona Arias <mcardona@poverty-action.org>                # 
#******************************************************************************# 

# rm(list = ls()) # to clean the workspace
getwd()
# setwd("../Dropbox/Aprende en Casa/")

# *****************************************************************************
#### 01 Load packages and functions ####
# *****************************************************************************
### Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(kableExtra)
library(pixiedust)
library(psych)
library(readxl)
library(likert) 
library(treemap)
library(viridis)

# *****************************************************************************
#### 02 Load data ####
# *****************************************************************************
docentes <- read_excel("./COVID19_Docentes_results_code.xlsx")

dim(docentes)

# *****************************************************************************
#### 03 Templates  ####
# *****************************************************************************

# Lollipop graphs ----

# Create data
data <- data.frame(
  x=LETTERS[1:26],
  y=abs(rnorm(26))
)

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")

# Likert bar graphs ----

# Use a provided dataset
data(pisaitems) 
items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"] 

# Build plot
p <- likert(items28) 
plot(p)

# Treemap graphs -----
  
  # Create data
  group <- c("group-1","group-2","group-3")
  value <- c(13,5,22)
  data <- data.frame(group,value)
  
  # treemap
  # Create data
  group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
  subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
  value <- c(13,5,22,12,11,7,3,1,23)
  data <- data.frame(group,subgroup,value)
  
  # Custom labels:
  treemap(data, index=c("group","subgroup"),     vSize="value", type="index",
          
          fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
          fontcolor.labels=c("white","orange"),    # Color of labels
          fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
          bg.labels=c("transparent"),              # Background color of labels
          align.labels=list(
            c("center", "center"), 
            c("right", "bottom")
          ),                                   # Where to place labels in the rectangle?
          overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
          inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
          
  )
  
# Doughnut chart ----

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")
# Circular stacked barplot ----
# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)



b_runner <- c("#946c86", "#1f6c64", "#65507b", "#69c0b8",
              "#3c2e48", "#94b4b1", "#182132", "#547fa2")
scale <- c("#69c0b8", "#94b4b1", "#1f6c64", "#211525", "#223f6b")

school <- c("#8661A5", "#5ABB9A", "#25583E", "#358DA9", "#F34C49")
  scale_s <- c("#246073", "#3388a3", "#3ca2c2", "#46bbe0", "#4bd0fa")
teacher <- c("#FF0052", "#59AC5A", "#334A96", "#F4F099", "#F26E23")
  scale_t <- c("#f05d0a", "#f57b36", "#f08c54", "#f2a980", "#f5c6ab")

legend_g <- paste0("Fuente: Elaboración propia con datos de la encuesta Diagnóstico del modelo pedagógico Aprende en Casa en educación primaria \naplicada en el estado de Aguascalientes, México. La encuesta fue aplicada a 3,588 docentes de educación primaria.") 

# *****************************************************************************
#### 04 Graphs  ####
# *****************************************************************************

# CVDMGPO ------
# Tipo de variable: Categórica

# ¿Durante el nuevo ciclo escolar 2020-2021 impartirá clases al mismo grupo de 
# estudiantes que atendió el ciclo 2019-2020 en un grado distinto? 

Código <- c("CVDMGPO",
            " ")
Variable <- c("¿Durante el nuevo ciclo escolar 2020-2021 impartirá clases al mismo grupo de 
                    estudiantes que atendió el ciclo 2019-2020 en un grado distinto? ",
              " ")
Respuesta <- c("Sí",
               "No")

N <- sum(docentes$CVDMGPO != -1)

freq <- c(nrow(docentes[docentes$CVDMGPO==1,]),
          nrow(docentes[docentes$CVDMGPO==2,]))
perc <- c(round((nrow(docentes[docentes$CVDMGPO==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDMGPO==2,])/N)*100, 2))

t_CVDMGPO <- data.frame(Código, Variable, Respuesta, freq, perc)
t_CVDMGPO$ymax <- cumsum(t_CVDMGPO$perc)
t_CVDMGPO$ymin <- c(0, head(t_CVDMGPO$ymax, n=-1))
t_CVDMGPO$labelPosition <- (t_CVDMGPO$ymax + t_CVDMGPO$ymin) / 2
t_CVDMGPO$label <- paste0(t_CVDMGPO$Respuesta, "\n", t_CVDMGPO$perc, "%")

# Make the plot

titles <- paste0("¿Durante el nuevo ciclo escolar 2020-2021 impartirá clases al mismo \ngrupo de estudiantes que atendió el ciclo 2019-2020 en un grado distinto?")

ggplot(t_CVDMGPO, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=6) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=school) +
  scale_color_manual(values=school) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                   breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("./figures/","CVDMGPO_s_v2", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDMGPO, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=6) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=teacher) +
  scale_color_manual(values=teacher) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                     breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("05 Figures/01 Docentes/","CVDMGPO_t", ".jpeg"), 
         width = 14, height = 10)

# CVDHDREC ------
# Tipo de variable: Categórica - Múltiple

# De los siguientes recursos, ¿cuáles tenía disponible para apoyar el desarrollo 
# de sus clases a distancia?  


# Create dummy variables
docentes$CVDHDREC_c_01 <- case_when(docentes$CVDHDREC_O1 == 1 |
                                      docentes$CVDHDREC_O2 == 1 |
                                      docentes$CVDHDREC_O3 == 1 |
                                      docentes$CVDHDREC_O4 == 1 |
                                      docentes$CVDHDREC_O5 == 1 |
                                      docentes$CVDHDREC_O6 == 1 ~ 1)
docentes$CVDHDREC_c_01[is.na(docentes$CVDHDREC_c_01)] <- 0

docentes$CVDHDREC_c_02 <- case_when(docentes$CVDHDREC_O1 == 2 |
                                      docentes$CVDHDREC_O2 == 2 |
                                      docentes$CVDHDREC_O3 == 2 |
                                      docentes$CVDHDREC_O4 == 2 |
                                      docentes$CVDHDREC_O5 == 2 |
                                      docentes$CVDHDREC_O6 == 2 ~ 1)
docentes$CVDHDREC_c_02[is.na(docentes$CVDHDREC_c_02)] <- 0

docentes$CVDHDREC_c_03 <- case_when(docentes$CVDHDREC_O1 == 3 |
                                      docentes$CVDHDREC_O2 == 3 |
                                      docentes$CVDHDREC_O3 == 3 |
                                      docentes$CVDHDREC_O4 == 3 |
                                      docentes$CVDHDREC_O5 == 3 |
                                      docentes$CVDHDREC_O6 == 3 ~ 1)
docentes$CVDHDREC_c_03[is.na(docentes$CVDHDREC_c_03)] <- 0

docentes$CVDHDREC_c_04 <- case_when(docentes$CVDHDREC_O1 == 4 |
                                      docentes$CVDHDREC_O2 == 4 |
                                      docentes$CVDHDREC_O3 == 4 |
                                      docentes$CVDHDREC_O4 == 4 |
                                      docentes$CVDHDREC_O5 == 4 |
                                      docentes$CVDHDREC_O6 == 4 ~ 1)
docentes$CVDHDREC_c_04[is.na(docentes$CVDHDREC_c_04)] <- 0

docentes$CVDHDREC_c_05 <- case_when(docentes$CVDHDREC_O1 == 5 |
                                      docentes$CVDHDREC_O2 == 5 |
                                      docentes$CVDHDREC_O3 == 5 |
                                      docentes$CVDHDREC_O4 == 5 |
                                      docentes$CVDHDREC_O5 == 5 |
                                      docentes$CVDHDREC_O6 == 5 ~ 1)
docentes$CVDHDREC_c_05[is.na(docentes$CVDHDREC_c_05)] <- 0

docentes$CVDHDREC_c_07 <- case_when(docentes$CVDHDREC_O1 == 7 |
                                      docentes$CVDHDREC_O2 == 7 |
                                      docentes$CVDHDREC_O3 == 7 |
                                      docentes$CVDHDREC_O4 == 7 |
                                      docentes$CVDHDREC_O5 == 7 |
                                      docentes$CVDHDREC_O6 == 7 ~ 1)
docentes$CVDHDREC_c_07[is.na(docentes$CVDHDREC_c_07)] <- 0    




Código <- c("CVDHDREC",
            " ",
            " ",
            " ",
            " ",
            " ")
Variable <- c("¿Durante el nuevo ciclo escolar 2020-2021 impartirá clases al mismo grupo de 
                      estudiantes que atendió el ciclo 2019-2020 en un grado distinto? ",
              " ",
              " ",
              " ",
              " ",
              " ")
Respuesta <- c("Televisión",
               "Acceso de internet desde casa",
               "Mi propia computadora y/o tableta",
               "Una computadora/tableta compartida",
               "Un ambiente adecuado ",
               "Ninguna")



N <- sum(docentes$CVDHDREC_O1 != -1)

freq <- c(nrow(docentes[docentes$CVDHDREC_c_01==1,]),
          nrow(docentes[docentes$CVDHDREC_c_02==1,]),
          nrow(docentes[docentes$CVDHDREC_c_03==1,]),
          nrow(docentes[docentes$CVDHDREC_c_04==1,]),
          nrow(docentes[docentes$CVDHDREC_c_05==1,]),
          nrow(docentes[docentes$CVDHDREC_c_07==1,]))

perc <- c(round((nrow(docentes[docentes$CVDHDREC_c_01==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDHDREC_c_02==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDHDREC_c_03==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDHDREC_c_04==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDHDREC_c_05==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDHDREC_c_07==1,])/N)*100, 2))

t_CVDHDREC <- data.frame(Código, Variable, Respuesta, freq, perc)
t_CVDHDREC$keep <- case_when(t_CVDHDREC$Respuesta == "Televisión" | 
                               t_CVDHDREC$Respuesta == "Acceso de internet desde casa" | 
                               t_CVDHDREC$Respuesta == "Mi propia computadora y/o tableta" ~ 1)
t_CVDHDREC <- t_CVDHDREC[complete.cases(t_CVDHDREC), ]

t_CVDHDREC$label <- paste0(t_CVDHDREC$Respuesta, "\n", t_CVDHDREC$perc, "%")
t_CVDHDREC$legend <- factor(t_CVDHDREC$label, levels = c("Acceso de internet desde casa\n82.75%", "Televisión\n70.62%", "Mi propia computadora y/o tableta\n67.64%"))

# Plot

titles <- "De los siguientes recursos, ¿cuáles tenía disponible para apoyar el \ndesarrollo de sus clases a distancia?"

ggplot(t_CVDHDREC, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=school[5], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDHDREC$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDHDREC_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDHDREC, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=teacher[5], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDHDREC$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDHDREC_t", ".jpeg"), 
         width = 14, height = 10)





# CVDCDPROB ------
# Tipo de variable: Categórica - Múltiple

# ¿Qué tipo de problemas enfrentó con mayor frecuencia para adaptar
# sus actividades docentes a una modalidad de educación a distancia 
# el ciclo escolar 2019-2020?

# Create dummy variables

docentes$CVDCDPROB_O1[is.na(docentes$CVDCDPROB_O1)] <- -2
docentes$CVDCDPROB_O2[is.na(docentes$CVDCDPROB_O2)] <- -2
docentes$CVDCDPROB_O3[is.na(docentes$CVDCDPROB_O3)] <- -2
docentes$CVDCDPROB_O4[is.na(docentes$CVDCDPROB_O4)] <- -2
docentes$CVDCDPROB_O5[is.na(docentes$CVDCDPROB_O5)] <- -2
docentes$CVDCDPROB_O6[is.na(docentes$CVDCDPROB_O6)] <- -2

docentes <- docentes %>% 
  mutate(CVDCDPROB_c_01 = case_when(docentes$CVDCDPROB_O1== 1 | docentes$CVDCDPROB_O2 == 1 | docentes$CVDCDPROB_O3 == 1 | docentes$CVDCDPROB_O4 == 1 | docentes$CVDCDPROB_O5 == 1 | docentes$CVDCDPROB_O6 == 1 ~ 1),
         CVDCDPROB_c_02 = case_when(docentes$CVDCDPROB_O1== 2 | docentes$CVDCDPROB_O2 == 2 | docentes$CVDCDPROB_O3 == 2 | docentes$CVDCDPROB_O4 == 2 | docentes$CVDCDPROB_O5 == 2 | docentes$CVDCDPROB_O6 == 2 ~ 1),
         CVDCDPROB_c_03 = case_when(docentes$CVDCDPROB_O1== 3 | docentes$CVDCDPROB_O2 == 3 | docentes$CVDCDPROB_O3 == 3 | docentes$CVDCDPROB_O4 == 3 | docentes$CVDCDPROB_O5 == 3 | docentes$CVDCDPROB_O6 == 3 ~ 1),
         CVDCDPROB_c_04 = case_when(docentes$CVDCDPROB_O1== 4 | docentes$CVDCDPROB_O2 == 4 | docentes$CVDCDPROB_O3 == 4 | docentes$CVDCDPROB_O4 == 4 | docentes$CVDCDPROB_O5 == 4 | docentes$CVDCDPROB_O6 == 4 ~ 1),
         CVDCDPROB_c_05 = case_when(docentes$CVDCDPROB_O1== 5 | docentes$CVDCDPROB_O2 == 5 | docentes$CVDCDPROB_O3 == 5 | docentes$CVDCDPROB_O4 == 5 | docentes$CVDCDPROB_O5 == 5 | docentes$CVDCDPROB_O6 == 5 ~ 1),
         CVDCDPROB_c_06 = case_when(docentes$CVDCDPROB_O1== 6 | docentes$CVDCDPROB_O2 == 6 | docentes$CVDCDPROB_O3 == 6 | docentes$CVDCDPROB_O4 == 6 | docentes$CVDCDPROB_O5 == 6 | docentes$CVDCDPROB_O6 == 6 ~ 1))


docentes$CVDCDPROB_c_01[is.na(docentes$CVDCDPROB_c_01)] <- 0
docentes$CVDCDPROB_c_02[is.na(docentes$CVDCDPROB_c_02)] <- 0
docentes$CVDCDPROB_c_03[is.na(docentes$CVDCDPROB_c_03)] <- 0
docentes$CVDCDPROB_c_04[is.na(docentes$CVDCDPROB_c_04)] <- 0
docentes$CVDCDPROB_c_05[is.na(docentes$CVDCDPROB_c_05)] <- 0
docentes$CVDCDPROB_c_06[is.na(docentes$CVDCDPROB_c_06)] <- 0   


Código <- c("CVDCDPROB",
            " ",
            " ",
            " ",
            " ",
            " ")
Variable <- c("¿Qué tipo de problemas enfrentó con mayor frecuencia para adaptar
                    sus actividades docentes a una modalidad de educación a distancia 
                    el ciclo escolar 2019-2020?",
              " ",
              " ",
              " ",
              " ",
              " ")
Respuesta <- c("Competencias digitales",
               "Aprendizaje",
               "Socioafectivos",
               "Económicos",
               "No tengo información",
               "Ninguno")


N <- sum(docentes$CVDCDPROB_O1 != -1)

freq <- c(nrow(docentes[docentes$CVDCDPROB_c_01==1,]),
          nrow(docentes[docentes$CVDCDPROB_c_02==1,]),
          nrow(docentes[docentes$CVDCDPROB_c_03==1,]),
          nrow(docentes[docentes$CVDCDPROB_c_04==1,]),
          nrow(docentes[docentes$CVDCDPROB_c_05==1,]),
          nrow(docentes[docentes$CVDCDPROB_c_06==1,]))

perc <- c(round((nrow(docentes[docentes$CVDCDPROB_c_01==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDPROB_c_02==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDPROB_c_03==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDPROB_c_04==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDPROB_c_05==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDPROB_c_06==1,])/N)*100, 2))

t_CVDCDPROB <- data.frame(Código, Variable, Respuesta, freq, perc)

t_CVDCDPROB$keep <- case_when(t_CVDCDPROB$Respuesta == "Competencias digitales" | 
                                t_CVDCDPROB$Respuesta == "Aprendizaje" | 
                                t_CVDCDPROB$Respuesta == "Socioafectivos" | 
                                t_CVDCDPROB$Respuesta == "Económicos" ~ 1)
t_CVDCDPROB <- t_CVDCDPROB[complete.cases(t_CVDCDPROB), ]

t_CVDCDPROB$label <- paste0(t_CVDCDPROB$Respuesta, "\n", t_CVDCDPROB$perc, "%")
t_CVDCDPROB$legend <- factor(t_CVDCDPROB$label, levels = c("Socioafectivos\n68.2%", "Competencias digitales\n63.88%", "Económicos\n38.1%", "Aprendizaje\n32.36%"))


# Plot

titles <- "¿Qué tipo de problemas enfrentó con mayor frecuencia para adaptar sus actividades \ndocentes a una modalidad de educación a distancia el ciclo escolar 2019-2020?"

ggplot(t_CVDCDPROB, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=school[5], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDCDPROB$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDPROB_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDCDPROB, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=teacher[5], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDCDPROB$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDPROB_t", ".jpeg"), 
         width = 14, height = 10)


# CVDCDCT ------
# Tipo de variable: Categórica

# ¿Recibió capacitación de su centro de trabajo para manejar recursos
# o herramientas que le facilitaran llevar a cabo una educación a distancia
# durante el ciclo escolar 2019-2020?

Código <- c("CVDCDCT",
            " ")
Variable <- c("¿Recibió capacitación de su centro de trabajo para manejar recursos
                    o herramientas que le facilitaran llevar a cabo una educación a distancia
                    durante el ciclo escolar 2019-2020?",
              " ")
Respuesta <- c("Sí",
               "No")

N <- sum(docentes$CVDCDCT != -1)

freq <- c(nrow(docentes[docentes$CVDCDCT==1,]),
          nrow(docentes[docentes$CVDCDCT==2,]))
perc <- c(round((nrow(docentes[docentes$CVDCDCT==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDCT==2,])/N)*100, 2))

t_CVDCDCT <- data.frame(Código, Variable, Respuesta, freq, perc)
t_CVDCDCT$ymax <- cumsum(t_CVDCDCT$perc)
t_CVDCDCT$ymin <- c(0, head(t_CVDCDCT$ymax, n=-1))
t_CVDCDCT$labelPosition <- (t_CVDCDCT$ymax + t_CVDCDCT$ymin) / 2
t_CVDCDCT$label <- paste0(t_CVDCDCT$Respuesta, "\n", t_CVDCDCT$perc, "%")

# Make the plot

titles <- paste0("¿Recibió capacitación de su centro de trabajo para manejar recursos \no herramientas que le facilitaran llevar a cabo una educación a distancia \ndurante el ciclo escolar 2019-2020?")

ggplot(t_CVDCDCT, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=6) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=school) +
  scale_color_manual(values=school) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                     breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDCT_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDCDCT, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=6) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=teacher) +
  scale_color_manual(values=teacher) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                     breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDCT_t", ".jpeg"), 
         width = 14, height = 10)

# CVDCDCTOTH ------
# Tipo de variable: Categórica - Múltiple

# ¿Qué tipo de capacitación recibió?  

# Create dummy variables

docentes$CVDCDCTOTH_O1[is.na(docentes$CVDCDCTOTH_O1)] <- -2
docentes$CVDCDCTOTH_O2[is.na(docentes$CVDCDCTOTH_O2)] <- -2
docentes$CVDCDCTOTH_O3[is.na(docentes$CVDCDCTOTH_O3)] <- -2
docentes$CVDCDCTOTH_O4[is.na(docentes$CVDCDCTOTH_O4)] <- -2


docentes <- docentes %>% 
  mutate(CVDCDCTOTH_c_01 = case_when(docentes$CVDCDCTOTH_O1== 1 | docentes$CVDCDCTOTH_O2 == 1 | docentes$CVDCDCTOTH_O3 == 1 | docentes$CVDCDCTOTH_O4 == 1 ~ 1),
         CVDCDCTOTH_c_02 = case_when(docentes$CVDCDCTOTH_O1== 2 | docentes$CVDCDCTOTH_O2 == 2 | docentes$CVDCDCTOTH_O3 == 2 | docentes$CVDCDCTOTH_O4 == 2 ~ 1),
         CVDCDCTOTH_c_03 = case_when(docentes$CVDCDCTOTH_O1== 3 | docentes$CVDCDCTOTH_O2 == 3 | docentes$CVDCDCTOTH_O3 == 3 | docentes$CVDCDCTOTH_O4 == 3 ~ 1),
         CVDCDCTOTH_c_04 = case_when(docentes$CVDCDCTOTH_O1== 4 | docentes$CVDCDCTOTH_O2 == 4 | docentes$CVDCDCTOTH_O3 == 4 | docentes$CVDCDCTOTH_O4 == 4  ~ 1))


docentes$CVDCDCTOTH_c_01[is.na(docentes$CVDCDCTOTH_c_01)] <- 0
docentes$CVDCDCTOTH_c_02[is.na(docentes$CVDCDCTOTH_c_02)] <- 0
docentes$CVDCDCTOTH_c_03[is.na(docentes$CVDCDCTOTH_c_03)] <- 0
docentes$CVDCDCTOTH_c_04[is.na(docentes$CVDCDCTOTH_c_04)] <- 0



Código <- c("CVDCDCTOTH",
            " ",
            " ",
            " ")
Variable <- c("¿Qué tipo de capacitación recibió?",
              " ",
              " ",
              " ")
Respuesta <- c("Tecnológica (plataformas digitales, etc.)",
               "Didáctica (transición a enseñanza en modalidad a distancia)",
               "Socioemocional (talleres para abordar el tema de la contingencia)",
               "Ninguna")


N <- sum(docentes$CVDCDCTOTH_O1 != -1)

freq <- c(nrow(docentes[docentes$CVDCDCTOTH_c_01==1,]),
          nrow(docentes[docentes$CVDCDCTOTH_c_02==1,]),
          nrow(docentes[docentes$CVDCDCTOTH_c_03==1,]),
          nrow(docentes[docentes$CVDCDCTOTH_c_04==1,]))

perc <- c(round((nrow(docentes[docentes$CVDCDCTOTH_c_01==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDCTOTH_c_02==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDCTOTH_c_03==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDCTOTH_c_04==1,])/N)*100, 2))

t_CVDCDCTOTH <- data.frame(Código, Variable, Respuesta, freq, perc)
t_CVDCDCTOTH$legend <- case_when(t_CVDCDCTOTH$Respuesta == "Tecnológica (plataformas digitales, etc.)" ~ "Tecnológica",
                                 t_CVDCDCTOTH$Respuesta == "Didáctica (transición a enseñanza en modalidad a distancia)" ~ "Didáctica",
                                 t_CVDCDCTOTH$Respuesta == "Socioemocional (talleres para abordar el tema de la contingencia)" ~ "Socioemocional",
                                 t_CVDCDCTOTH$Respuesta == "Ninguna" ~ "Ninguna")
t_CVDCDCTOTH$legend <- paste0(t_CVDCDCTOTH$legend, "\n", t_CVDCDCTOTH$perc, "%")

t_CVDCDCTOTH$legend <- factor(t_CVDCDCTOTH$legend, levels = c("Tecnológica\n94.17%", "Didáctica\n35.17%", "Socioemocional\n20.38%", "Ninguna\n0.4%"))


# Plot

titles <- "¿Qué tipo de capacitación recibió?"

ggplot(t_CVDCDCTOTH, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=school[5], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDCDPROB$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDCTOTH_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDCDCTOTH, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=teacher[5], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDCDPROB$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDCTOTH_t", ".jpeg"), 
         width = 14, height = 10)


 
# CVDCDATL ------
# Tipo de variable: Categórica

# ¿Considera que la orientación/capacitación durante la contingencia
# sanitaria por parte de las autoridades educativas locales (director,
# supervisor, jefe de sector) fue suficiente durante el ciclo escolar 
# 2019-2020?

Código <- c("CVDCDATL",
            " ",
            " ",
            " ")
Variable <- c("¿Considera que la orientación/capacitación durante la contingencia
                    sanitaria por parte de las autoridades educativas locales (director,
                    supervisor, jefe de sector) fue suficiente durante el ciclo escolar 
                    2019-2020?",
              " ",
              " ",
              " ")
Respuesta <- c("Sí, suficiente",
               "Sí, parcialmente",
               "Escasamente",
               "No, fue insuficiente")

N <- sum(docentes$CVDCDATL != -1)

freq <- c(nrow(docentes[docentes$CVDCDATL==1,]),
          nrow(docentes[docentes$CVDCDATL==2,]),
          nrow(docentes[docentes$CVDCDATL==3,]),
          nrow(docentes[docentes$CVDCDATL==4,]))
perc <- c(round((nrow(docentes[docentes$CVDCDATL==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDATL==2,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDATL==3,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDATL==4,])/N)*100, 2))

t_CVDCDATL <- data.frame(Código, Variable, Respuesta, freq, perc)

t_CVDCDATL$ymax <- cumsum(t_CVDCDATL$perc)
t_CVDCDATL$ymin <- c(0, head(t_CVDCDATL$ymax, n=-1))
t_CVDCDATL$labelPosition <- (t_CVDCDATL$ymax + t_CVDCDATL$ymin) / 2
t_CVDCDATL$label <- paste0(t_CVDCDATL$Respuesta, "\n", t_CVDCDATL$perc, "%")

# Make the plot

titles <- paste0("¿Considera que la orientación/capacitación durante la contingencia \nsanitaria por parte de las autoridades educativas locales (director, \nsupervisor, jefe de sector) fue suficiente durante el ciclo escolar 2019-2020?")

ggplot(t_CVDCDATL, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=4) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=scale_s) +
  scale_color_manual(values=scale_s) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                     breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDATL_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDCDATL, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=4) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=rev(scale_t)) +
  scale_color_manual(values=rev(scale_t)) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                     breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDATL_t", ".jpeg"), 
         width = 14, height = 10)

# CVDCDNCE ------
# Tipo de variable: Categórica - Múltiple

# ¿Qué tipo de capacitación requerirá para llevar a cabo sus clases
# a distancia en el nuevo ciclo escolar 2020-2021?  

# Create dummy variables

docentes$CVDCDNCE_O1[is.na(docentes$CVDCDNCE_O1)] <- -2
docentes$CVDCDNCE_O2[is.na(docentes$CVDCDNCE_O2)] <- -2
docentes$CVDCDNCE_O3[is.na(docentes$CVDCDNCE_O3)] <- -2
docentes$CVDCDNCE_O4[is.na(docentes$CVDCDNCE_O4)] <- -2
docentes$CVDCDNCE_O5[is.na(docentes$CVDCDNCE_O5)] <- -2
docentes$CVDCDNCE_O6[is.na(docentes$CVDCDNCE_O6)] <- -2
docentes$CVDCDNCE_O7[is.na(docentes$CVDCDNCE_O7)] <- -2

docentes <- docentes %>% 
  mutate(CVDCDNCE_c_01 = case_when(docentes$CVDCDNCE_O1== 1 | docentes$CVDCDNCE_O2 == 1 | docentes$CVDCDNCE_O3 == 1 | docentes$CVDCDNCE_O4 == 1 | docentes$CVDCDNCE_O5 == 1 | docentes$CVDCDNCE_O6 == 1 | docentes$CVDCDNCE_O7 == 1 ~ 1),
         CVDCDNCE_c_02 = case_when(docentes$CVDCDNCE_O1== 2 | docentes$CVDCDNCE_O2 == 2 | docentes$CVDCDNCE_O3 == 2 | docentes$CVDCDNCE_O4 == 2 | docentes$CVDCDNCE_O5 == 2 | docentes$CVDCDNCE_O6 == 2 | docentes$CVDCDNCE_O7 == 2 ~ 1),
         CVDCDNCE_c_03 = case_when(docentes$CVDCDNCE_O1== 3 | docentes$CVDCDNCE_O2 == 3 | docentes$CVDCDNCE_O3 == 3 | docentes$CVDCDNCE_O4 == 3 | docentes$CVDCDNCE_O5 == 3 | docentes$CVDCDNCE_O6 == 3 | docentes$CVDCDNCE_O7 == 3 ~ 1),
         CVDCDNCE_c_04 = case_when(docentes$CVDCDNCE_O1== 4 | docentes$CVDCDNCE_O2 == 4 | docentes$CVDCDNCE_O3 == 4 | docentes$CVDCDNCE_O4 == 4 | docentes$CVDCDNCE_O5 == 4 | docentes$CVDCDNCE_O6 == 4 | docentes$CVDCDNCE_O7 == 4 ~ 1),
         CVDCDNCE_c_05 = case_when(docentes$CVDCDNCE_O1== 5 | docentes$CVDCDNCE_O2 == 5 | docentes$CVDCDNCE_O3 == 5 | docentes$CVDCDNCE_O4 == 5 | docentes$CVDCDNCE_O5 == 5 | docentes$CVDCDNCE_O6 == 5 | docentes$CVDCDNCE_O7 == 5 ~ 1),
         CVDCDNCE_c_06 = case_when(docentes$CVDCDNCE_O1== 6 | docentes$CVDCDNCE_O2 == 6 | docentes$CVDCDNCE_O3 == 6 | docentes$CVDCDNCE_O4 == 6 | docentes$CVDCDNCE_O5 == 6 | docentes$CVDCDNCE_O6 == 6 | docentes$CVDCDNCE_O7 == 6 ~ 1),
         CVDCDNCE_c_07 = case_when(docentes$CVDCDNCE_O1== 7 | docentes$CVDCDNCE_O2 == 7 | docentes$CVDCDNCE_O3 == 7 | docentes$CVDCDNCE_O4 == 7 | docentes$CVDCDNCE_O5 == 7 | docentes$CVDCDNCE_O6 == 7 | docentes$CVDCDNCE_O7 == 7 ~ 1))


docentes$CVDCDNCE_c_01[is.na(docentes$CVDCDNCE_c_01)] <- 0
docentes$CVDCDNCE_c_02[is.na(docentes$CVDCDNCE_c_02)] <- 0
docentes$CVDCDNCE_c_03[is.na(docentes$CVDCDNCE_c_03)] <- 0
docentes$CVDCDNCE_c_04[is.na(docentes$CVDCDNCE_c_04)] <- 0
docentes$CVDCDNCE_c_05[is.na(docentes$CVDCDNCE_c_05)] <- 0
docentes$CVDCDNCE_c_06[is.na(docentes$CVDCDNCE_c_06)] <- 0   
docentes$CVDCDNCE_c_07[is.na(docentes$CVDCDNCE_c_07)] <- 0   


Código <- c("CVDCDNCE",
            " ",
            " ",
            " ",
            " ",
            " ",
            " ")
Variable <- c("¿Qué tipo de capacitación requerirá para llevar a cabo sus clases
                    a distancia en el nuevo ciclo escolar 2020-2021?",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ")
Respuesta <- c("Uso de plataformas digitales (Google Classroom, Zoom, etc.)",
               "Diseño de  materiales en línea",
               "Priorización de contenidos curriculares",
               "Estrategias de evaluación de aprendizajes",
               "Estrategias de apoyo socioemocional para estudiantes",
               "Estrategias de apoyo socioemocional para padres",
               "Ninguna")


N <- sum(docentes$CVDCDNCE_O1 != -1)

freq <- c(nrow(docentes[docentes$CVDCDNCE_c_01==1,]),
          nrow(docentes[docentes$CVDCDNCE_c_02==1,]),
          nrow(docentes[docentes$CVDCDNCE_c_03==1,]),
          nrow(docentes[docentes$CVDCDNCE_c_04==1,]),
          nrow(docentes[docentes$CVDCDNCE_c_05==1,]),
          nrow(docentes[docentes$CVDCDNCE_c_06==1,]),
          nrow(docentes[docentes$CVDCDNCE_c_07==1,]))

perc <- c(round((nrow(docentes[docentes$CVDCDNCE_c_01==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDNCE_c_02==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDNCE_c_03==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDNCE_c_04==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDNCE_c_05==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDNCE_c_06==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDCDNCE_c_07==1,])/N)*100, 2))

t_CVDCDNCE <- data.frame(Código, Variable, Respuesta, freq, perc)

t_CVDCDNCE$keep <- case_when(t_CVDCDNCE$Respuesta == "Uso de plataformas digitales (Google Classroom, Zoom, etc.)" | 
                               t_CVDCDNCE$Respuesta == "Diseño de  materiales en línea" | 
                               t_CVDCDNCE$Respuesta == "Priorización de contenidos curriculares" | 
                               t_CVDCDNCE$Respuesta == "Estrategias de evaluación de aprendizajes" ~ 1)
t_CVDCDNCE <- t_CVDCDNCE[complete.cases(t_CVDCDNCE), ]

t_CVDCDNCE$legend <- case_when(t_CVDCDNCE$Respuesta == "Uso de plataformas digitales (Google Classroom, Zoom, etc.)" ~ "Uso de plataformas digitales",
                               t_CVDCDNCE$Respuesta == "Diseño de  materiales en línea" ~ "Diseño de  materiales \nen línea",
                               t_CVDCDNCE$Respuesta == "Priorización de contenidos curriculares" ~ "Priorización de \ncontenidos curriculares",
                               t_CVDCDNCE$Respuesta == "Estrategias de evaluación de aprendizajes" ~ "Estrategias de evaluación \nde aprendizajes") 


t_CVDCDNCE$legend <- paste0(t_CVDCDNCE$legend, "\n", t_CVDCDNCE$perc, "%")

t_CVDCDNCE$legend <- factor(t_CVDCDNCE$legend, levels = c("Diseño de  materiales \nen línea\n65.3%",
                                                          "Estrategias de evaluación \nde aprendizajes\n64.99%",
                                                          "Uso de plataformas digitales\n61.04%",
                                                          "Priorización de \ncontenidos curriculares\n53.29%"))
# Plot

titles <- "¿Qué tipo de capacitación requerirá para llevar a cabo sus clases \na distancia en el nuevo ciclo escolar 2020-2021?"

ggplot(t_CVDCDNCE, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=school[4], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDCDNCE$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDNCE_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDCDNCE, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=teacher[5], size=6) +
  scale_y_continuous(breaks = seq(0, max(t_CVDCDNCE$freq), 200)) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDCDNCE_t", ".jpeg"), 
         width = 14, height = 10)

# CVDRCLINEAS ------
# Tipo de variable: Categórica - Múltiple

# ¿Cuáles de las siguientes líneas de acción utilizó como recursos para 
# impartir sus clases a distancia durante el ciclo escolar 2019-2020?

# Create dummy variables

docentes$CVDRCLINEAS_O1[is.na(docentes$CVDRCLINEAS_O1)] <- -2
docentes$CVDRCLINEAS_O2[is.na(docentes$CVDRCLINEAS_O2)] <- -2
docentes$CVDRCLINEAS_O3[is.na(docentes$CVDRCLINEAS_O3)] <- -2
docentes$CVDRCLINEAS_O4[is.na(docentes$CVDRCLINEAS_O4)] <- -2
docentes$CVDRCLINEAS_O5[is.na(docentes$CVDRCLINEAS_O5)] <- -2
docentes$CVDRCLINEAS_O6[is.na(docentes$CVDRCLINEAS_O6)] <- -2
docentes$CVDRCLINEAS_O7[is.na(docentes$CVDRCLINEAS_O7)] <- -2
docentes$CVDRCLINEAS_O8[is.na(docentes$CVDRCLINEAS_O8)] <- -2

docentes <- docentes %>% 
  mutate(CVDRCLINEAS_c_01 = case_when(docentes$CVDRCLINEAS_O1== 1 | docentes$CVDRCLINEAS_O2 == 1 | docentes$CVDRCLINEAS_O3 == 1 | docentes$CVDRCLINEAS_O4 == 1 | docentes$CVDRCLINEAS_O5 == 1 | docentes$CVDRCLINEAS_O6 == 1 | docentes$CVDRCLINEAS_O7 == 1 | docentes$CVDRCLINEAS_O8 == 1 ~ 1),
         CVDRCLINEAS_c_02 = case_when(docentes$CVDRCLINEAS_O1== 2 | docentes$CVDRCLINEAS_O2 == 2 | docentes$CVDRCLINEAS_O3 == 2 | docentes$CVDRCLINEAS_O4 == 2 | docentes$CVDRCLINEAS_O5 == 2 | docentes$CVDRCLINEAS_O6 == 2 | docentes$CVDRCLINEAS_O7 == 2 | docentes$CVDRCLINEAS_O8 == 2 ~ 1),
         CVDRCLINEAS_c_03 = case_when(docentes$CVDRCLINEAS_O1== 3 | docentes$CVDRCLINEAS_O2 == 3 | docentes$CVDRCLINEAS_O3 == 3 | docentes$CVDRCLINEAS_O4 == 3 | docentes$CVDRCLINEAS_O5 == 3 | docentes$CVDRCLINEAS_O6 == 3 | docentes$CVDRCLINEAS_O7 == 3 | docentes$CVDRCLINEAS_O8 == 3 ~ 1),
         CVDRCLINEAS_c_04 = case_when(docentes$CVDRCLINEAS_O1== 4 | docentes$CVDRCLINEAS_O2 == 4 | docentes$CVDRCLINEAS_O3 == 4 | docentes$CVDRCLINEAS_O4 == 4 | docentes$CVDRCLINEAS_O5 == 4 | docentes$CVDRCLINEAS_O6 == 4 | docentes$CVDRCLINEAS_O7 == 4 | docentes$CVDRCLINEAS_O8 == 4 ~ 1),
         CVDRCLINEAS_c_05 = case_when(docentes$CVDRCLINEAS_O1== 5 | docentes$CVDRCLINEAS_O2 == 5 | docentes$CVDRCLINEAS_O3 == 5 | docentes$CVDRCLINEAS_O4 == 5 | docentes$CVDRCLINEAS_O5 == 5 | docentes$CVDRCLINEAS_O6 == 5 | docentes$CVDRCLINEAS_O7 == 5 | docentes$CVDRCLINEAS_O8 == 5 ~ 1),
         CVDRCLINEAS_c_06 = case_when(docentes$CVDRCLINEAS_O1== 6 | docentes$CVDRCLINEAS_O2 == 6 | docentes$CVDRCLINEAS_O3 == 6 | docentes$CVDRCLINEAS_O4 == 6 | docentes$CVDRCLINEAS_O5 == 6 | docentes$CVDRCLINEAS_O6 == 6 | docentes$CVDRCLINEAS_O7 == 6 | docentes$CVDRCLINEAS_O8 == 6 ~ 1),
         CVDRCLINEAS_c_07 = case_when(docentes$CVDRCLINEAS_O1== 7 | docentes$CVDRCLINEAS_O2 == 7 | docentes$CVDRCLINEAS_O3 == 7 | docentes$CVDRCLINEAS_O4 == 7 | docentes$CVDRCLINEAS_O5 == 7 | docentes$CVDRCLINEAS_O6 == 7 | docentes$CVDRCLINEAS_O7 == 7 | docentes$CVDRCLINEAS_O8 == 7 ~ 1),
         CVDRCLINEAS_c_08 = case_when(docentes$CVDRCLINEAS_O1== 8 | docentes$CVDRCLINEAS_O2 == 8 | docentes$CVDRCLINEAS_O3 == 8 | docentes$CVDRCLINEAS_O4 == 8 | docentes$CVDRCLINEAS_O5 == 8 | docentes$CVDRCLINEAS_O6 == 8 | docentes$CVDRCLINEAS_O7 == 8 | docentes$CVDRCLINEAS_O8 == 8 ~ 1))


docentes$CVDRCLINEAS_c_01[is.na(docentes$CVDRCLINEAS_c_01)] <- 0
docentes$CVDRCLINEAS_c_02[is.na(docentes$CVDRCLINEAS_c_02)] <- 0
docentes$CVDRCLINEAS_c_03[is.na(docentes$CVDRCLINEAS_c_03)] <- 0
docentes$CVDRCLINEAS_c_04[is.na(docentes$CVDRCLINEAS_c_04)] <- 0
docentes$CVDRCLINEAS_c_05[is.na(docentes$CVDRCLINEAS_c_05)] <- 0
docentes$CVDRCLINEAS_c_06[is.na(docentes$CVDRCLINEAS_c_06)] <- 0   
docentes$CVDRCLINEAS_c_07[is.na(docentes$CVDRCLINEAS_c_07)] <- 0   
docentes$CVDRCLINEAS_c_08[is.na(docentes$CVDRCLINEAS_c_08)] <- 0   


Código <- c("CVDRCLINEAS",
            " ",
            " ",
            " ",
            " ",
            " ",
            " ",
            " ")
Variable <- c("¿Cuáles de las siguientes líneas de acción de la estrategia 
                    “Aprende en casa” conoce?",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ")
Respuesta <- c("Programas televisivos (Ingenio TV, Once Niñas y Niños, otros)",
               "Canales de Youtube",
               "Programas radiofónicos",
               "Distribución de cuadernillos de trabajo",
               "Sitio de internet (aprendeencasa.sep.gob.mx)",
               "Orientación telefónica EDUCATEL  (55 36 01 75 99 y 800 288 66 88)",
               "Correo electrónico aprende_en_casa@nube.sep.gob.mx",
               "Ninguna de las anteriores")


N <- sum(docentes$CVDRCLINEAS_O1 != -1)

freq <- c(nrow(docentes[docentes$CVDRCLINEAS_c_01==1,]),
          nrow(docentes[docentes$CVDRCLINEAS_c_02==1,]),
          nrow(docentes[docentes$CVDRCLINEAS_c_03==1,]),
          nrow(docentes[docentes$CVDRCLINEAS_c_04==1,]),
          nrow(docentes[docentes$CVDRCLINEAS_c_05==1,]),
          nrow(docentes[docentes$CVDRCLINEAS_c_06==1,]),
          nrow(docentes[docentes$CVDRCLINEAS_c_07==1,]),
          nrow(docentes[docentes$CVDRCLINEAS_c_08==1,]))

perc <- c(round((nrow(docentes[docentes$CVDRCLINEAS_c_01==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLINEAS_c_02==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLINEAS_c_03==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLINEAS_c_04==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLINEAS_c_05==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLINEAS_c_06==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLINEAS_c_07==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLINEAS_c_08==1,])/N)*100, 2))

t_CVDRCLINEAS <- data.frame(Código, Variable, Respuesta, freq, perc)

t_CVDRCLINEAS$keep <- case_when(t_CVDRCLINEAS$Respuesta == "Programas televisivos (Ingenio TV, Once Niñas y Niños, otros)" | 
                                  t_CVDRCLINEAS$Respuesta == "Canales de Youtube" | 
                                  t_CVDRCLINEAS$Respuesta == "Distribución de cuadernillos de trabajo" | 
                                  t_CVDRCLINEAS$Respuesta == "Sitio de internet (aprendeencasa.sep.gob.mx)" ~ 1)
t_CVDRCLINEAS <- t_CVDRCLINEAS[complete.cases(t_CVDRCLINEAS), ]

t_CVDRCLINEAS$legend <- case_when(t_CVDRCLINEAS$Respuesta == "Programas televisivos (Ingenio TV, Once Niñas y Niños, otros)" ~ "Programas televisivos",
                                  t_CVDRCLINEAS$Respuesta == "Canales de Youtube" ~ "Canales de Youtube",
                                  t_CVDRCLINEAS$Respuesta == "Distribución de cuadernillos de trabajo" ~ "Distribución de \ncuadernillos de trabajo",
                                  t_CVDRCLINEAS$Respuesta == "Sitio de internet (aprendeencasa.sep.gob.mx)" ~ "Sitio de internet \n(aprendeencasa.sep.gob.mx)") 

t_CVDRCLINEAS$legend <- paste0(t_CVDRCLINEAS$legend, "\n", t_CVDRCLINEAS$perc, "%")

t_CVDRCLINEAS$legend <- factor(t_CVDRCLINEAS$legend, levels = c("Distribución de \ncuadernillos de trabajo\n62.9%",
                                                                "Programas televisivos\n51.53%",
                                                                "Canales de Youtube\n40.55%",
                                                                "Sitio de internet \n(aprendeencasa.sep.gob.mx)\n22.32%"))
# Plot

titles <- "¿Cuáles de las siguientes líneas de acción de la estrategia “Aprende en casa” conoce??"

ggplot(t_CVDRCLINEAS, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=school[4], size=6) +
  scale_y_continuous(breaks = c(seq(0, max(t_CVDRCLINEAS$freq), 200))) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCLINEAS_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDRCLINEAS, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=teacher[5], size=6) +
  scale_y_continuous(breaks = c(seq(0, max(t_CVDRCLINEAS$freq), 200))) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCLINEAS_t", ".jpeg"), 
         width = 14, height = 10)

# CVDRCLNFUN ------
# Tipo de variable: Categórica - Múltiple

# ¿Cuáles de las siguientes líneas de acción le resultaron más 
# funcionales para IMPARTIR SUS clases a distancia durante el 
# ciclo escolar 2019-2020?  

# Create dummy variables

docentes$CVDRCLNFUN_O1[is.na(docentes$CVDRCLNFUN_O1)] <- -2
docentes$CVDRCLNFUN_O2[is.na(docentes$CVDRCLNFUN_O2)] <- -2
docentes$CVDRCLNFUN_O3[is.na(docentes$CVDRCLNFUN_O3)] <- -2
docentes$CVDRCLNFUN_O4[is.na(docentes$CVDRCLNFUN_O4)] <- -2
docentes$CVDRCLNFUN_O5[is.na(docentes$CVDRCLNFUN_O5)] <- -2
docentes$CVDRCLNFUN_O6[is.na(docentes$CVDRCLNFUN_O6)] <- -2
docentes$CVDRCLNFUN_O7[is.na(docentes$CVDRCLNFUN_O7)] <- -2
docentes$CVDRCLNFUN_O8[is.na(docentes$CVDRCLNFUN_O8)] <- -2

docentes <- docentes %>% 
  mutate(CVDRCLNFUN_c_01 = case_when(docentes$CVDRCLNFUN_O1== 1 | docentes$CVDRCLNFUN_O2 == 1 | docentes$CVDRCLNFUN_O3 == 1 | docentes$CVDRCLNFUN_O4 == 1 | docentes$CVDRCLNFUN_O5 == 1 | docentes$CVDRCLNFUN_O6 == 1 | docentes$CVDRCLNFUN_O7 == 1 | docentes$CVDRCLNFUN_O8 == 1 ~ 1),
         CVDRCLNFUN_c_02 = case_when(docentes$CVDRCLNFUN_O1== 2 | docentes$CVDRCLNFUN_O2 == 2 | docentes$CVDRCLNFUN_O3 == 2 | docentes$CVDRCLNFUN_O4 == 2 | docentes$CVDRCLNFUN_O5 == 2 | docentes$CVDRCLNFUN_O6 == 2 | docentes$CVDRCLNFUN_O7 == 2 | docentes$CVDRCLNFUN_O8 == 2 ~ 1),
         CVDRCLNFUN_c_03 = case_when(docentes$CVDRCLNFUN_O1== 3 | docentes$CVDRCLNFUN_O2 == 3 | docentes$CVDRCLNFUN_O3 == 3 | docentes$CVDRCLNFUN_O4 == 3 | docentes$CVDRCLNFUN_O5 == 3 | docentes$CVDRCLNFUN_O6 == 3 | docentes$CVDRCLNFUN_O7 == 3 | docentes$CVDRCLNFUN_O8 == 3 ~ 1),
         CVDRCLNFUN_c_04 = case_when(docentes$CVDRCLNFUN_O1== 4 | docentes$CVDRCLNFUN_O2 == 4 | docentes$CVDRCLNFUN_O3 == 4 | docentes$CVDRCLNFUN_O4 == 4 | docentes$CVDRCLNFUN_O5 == 4 | docentes$CVDRCLNFUN_O6 == 4 | docentes$CVDRCLNFUN_O7 == 4 | docentes$CVDRCLNFUN_O8 == 4 ~ 1),
         CVDRCLNFUN_c_05 = case_when(docentes$CVDRCLNFUN_O1== 5 | docentes$CVDRCLNFUN_O2 == 5 | docentes$CVDRCLNFUN_O3 == 5 | docentes$CVDRCLNFUN_O4 == 5 | docentes$CVDRCLNFUN_O5 == 5 | docentes$CVDRCLNFUN_O6 == 5 | docentes$CVDRCLNFUN_O7 == 5 | docentes$CVDRCLNFUN_O8 == 5 ~ 1),
         CVDRCLNFUN_c_06 = case_when(docentes$CVDRCLNFUN_O1== 6 | docentes$CVDRCLNFUN_O2 == 6 | docentes$CVDRCLNFUN_O3 == 6 | docentes$CVDRCLNFUN_O4 == 6 | docentes$CVDRCLNFUN_O5 == 6 | docentes$CVDRCLNFUN_O6 == 6 | docentes$CVDRCLNFUN_O7 == 6 | docentes$CVDRCLNFUN_O8 == 6 ~ 1),
         CVDRCLNFUN_c_07 = case_when(docentes$CVDRCLNFUN_O1== 7 | docentes$CVDRCLNFUN_O2 == 7 | docentes$CVDRCLNFUN_O3 == 7 | docentes$CVDRCLNFUN_O4 == 7 | docentes$CVDRCLNFUN_O5 == 7 | docentes$CVDRCLNFUN_O6 == 7 | docentes$CVDRCLNFUN_O7 == 7 | docentes$CVDRCLNFUN_O8 == 7 ~ 1),
         CVDRCLNFUN_c_08 = case_when(docentes$CVDRCLNFUN_O1== 8 | docentes$CVDRCLNFUN_O2 == 8 | docentes$CVDRCLNFUN_O3 == 8 | docentes$CVDRCLNFUN_O4 == 8 | docentes$CVDRCLNFUN_O5 == 8 | docentes$CVDRCLNFUN_O6 == 8 | docentes$CVDRCLNFUN_O7 == 8 | docentes$CVDRCLNFUN_O8 == 8 ~ 1))


docentes$CVDRCLNFUN_c_01[is.na(docentes$CVDRCLNFUN_c_01)] <- 0
docentes$CVDRCLNFUN_c_02[is.na(docentes$CVDRCLNFUN_c_02)] <- 0
docentes$CVDRCLNFUN_c_03[is.na(docentes$CVDRCLNFUN_c_03)] <- 0
docentes$CVDRCLNFUN_c_04[is.na(docentes$CVDRCLNFUN_c_04)] <- 0
docentes$CVDRCLNFUN_c_05[is.na(docentes$CVDRCLNFUN_c_05)] <- 0
docentes$CVDRCLNFUN_c_06[is.na(docentes$CVDRCLNFUN_c_06)] <- 0   
docentes$CVDRCLNFUN_c_07[is.na(docentes$CVDRCLNFUN_c_07)] <- 0   
docentes$CVDRCLNFUN_c_08[is.na(docentes$CVDRCLNFUN_c_08)] <- 0   


Código <- c("CVDRCLNFUN",
            " ",
            " ",
            " ",
            " ",
            " ",
            " ",
            " ")
Variable <- c("¿Cuáles de las siguientes líneas de acción le resultaron más funcionales para IMPARTIR SUS clases a distancia durante el ciclo escolar 2019-2020?",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ")
Respuesta <- c("Programas televisivos (Ingenio TV, Once Niñas y Niños, otros)",
               "Canales de Youtube",
               "Programas radiofónicos",
               "Distribución de cuadernillos de trabajo",
               "Sitio de internet (aprendeencasa.sep.gob.mx)",
               "Orientación telefónica EDUCATEL  (55 36 01 75 99 y 800 288 66 88)",
               "Correo electrónico aprende_en_casa@nube.sep.gob.mx",
               "Ninguna de las anteriores")


N <- sum(docentes$CVDRCLNFUN_O1 != -1)

freq <- c(nrow(docentes[docentes$CVDRCLNFUN_c_01==1,]),
          nrow(docentes[docentes$CVDRCLNFUN_c_02==1,]),
          nrow(docentes[docentes$CVDRCLNFUN_c_03==1,]),
          nrow(docentes[docentes$CVDRCLNFUN_c_04==1,]),
          nrow(docentes[docentes$CVDRCLNFUN_c_05==1,]),
          nrow(docentes[docentes$CVDRCLNFUN_c_06==1,]),
          nrow(docentes[docentes$CVDRCLNFUN_c_07==1,]),
          nrow(docentes[docentes$CVDRCLNFUN_c_08==1,]))

perc <- c(round((nrow(docentes[docentes$CVDRCLNFUN_c_01==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLNFUN_c_02==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLNFUN_c_03==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLNFUN_c_04==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLNFUN_c_05==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLNFUN_c_06==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLNFUN_c_07==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCLNFUN_c_08==1,])/N)*100, 2))

t_CVDRCLNFUN <- data.frame(Código, Variable, Respuesta, freq, perc)

t_CVDRCLNFUN$keep <- case_when(t_CVDRCLNFUN$Respuesta == "Programas televisivos (Ingenio TV, Once Niñas y Niños, otros)" | 
                                 t_CVDRCLNFUN$Respuesta == "Canales de Youtube" | 
                                 t_CVDRCLNFUN$Respuesta == "Sitio de internet (aprendeencasa.sep.gob.mx)" ~ 1)
t_CVDRCLNFUN <- t_CVDRCLNFUN[complete.cases(t_CVDRCLNFUN), ]

t_CVDRCLNFUN$legend <- case_when(t_CVDRCLNFUN$Respuesta == "Programas televisivos (Ingenio TV, Once Niñas y Niños, otros)" ~ "Programas televisivos",
                                 t_CVDRCLNFUN$Respuesta == "Canales de Youtube" ~ "Canales de Youtube",
                                 t_CVDRCLNFUN$Respuesta == "Sitio de internet (aprendeencasa.sep.gob.mx)" ~ "Sitio de internet \n(aprendeencasa.sep.gob.mx)") 

t_CVDRCLNFUN$legend <- paste0(t_CVDRCLNFUN$legend, "\n", t_CVDRCLNFUN$perc, "%")

t_CVDRCLNFUN$legend <- factor(t_CVDRCLNFUN$legend, levels = c(unique(t_CVDRCLNFUN$legend)))
# Plot

titles <- "¿Cuáles de las siguientes líneas de acción le resultaron más funcionales para IMPARTIR SUS clases \na distancia durante el ciclo escolar 2019-2020?"

ggplot(t_CVDRCLNFUN, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=school[4], size=6) +
  scale_y_continuous(breaks = c(seq(0, max(t_CVDRCLNFUN$freq), 200))) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCLNFUN_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDRCLNFUN, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=teacher[5], size=6) +
  scale_y_continuous(breaks = c(seq(0, max(t_CVDRCLNFUN$freq), 200))) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCLNFUN_t", ".jpeg"), 
         width = 14, height = 10)
# CVDRCARPETA ------
# Tipo de variable: Categórica

# Durante el tiempo que impartió clases a distancia en el ciclo
# escolar 2019-2020, ¿solicitó a los estudiantes que organizaran 
# una carpeta de experiencias?

Código <- c("CVDRCARPETA",
            " ",
            " ")
Variable <- c("Durante el tiempo que impartió clases a distancia en el ciclo
                    escolar 2019-2020, ¿solicitó a los estudiantes que organizaran 
                    una carpeta de experiencias?",
              " ",
              " ")
Respuesta <- c("Sí, y todos o la mayoría de los estudiantes lo hicieron",
               "Sí, pero solo una minoría de los estudiantes lo hicieron",
               "No la solicité")

N <- sum(docentes$CVDRCARPETA != -1)

freq <- c(nrow(docentes[docentes$CVDRCARPETA==1,]),
          nrow(docentes[docentes$CVDRCARPETA==2,]),
          nrow(docentes[docentes$CVDRCARPETA==3,]))
perc <- c(round((nrow(docentes[docentes$CVDRCARPETA==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETA==2,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETA==3,])/N)*100, 2))

t_CVDRCARPETA <- data.frame(Código, Variable, Respuesta, freq, perc)

t_CVDRCARPETA$ymax <- cumsum(t_CVDRCARPETA$perc)
t_CVDRCARPETA$ymin <- c(0, head(t_CVDRCARPETA$ymax, n=-1))
t_CVDRCARPETA$labelPosition <- (t_CVDRCARPETA$ymax + t_CVDRCARPETA$ymin) / 2

t_CVDRCARPETA$legend <- case_when(t_CVDRCARPETA$Respuesta == "Sí, y todos o la mayoría de los estudiantes lo hicieron" ~ "Sí. Todos/la mayoría \nde los estudiantes \nlo hicieron",
                                  t_CVDRCARPETA$Respuesta == "Sí, pero solo una minoría de los estudiantes lo hicieron" ~ "Sí. Solo una \nminoría de los \nestudiantes lo \nhicieron",
                                  t_CVDRCARPETA$Respuesta == "No la solicité" ~ "No la solicité") 
t_CVDRCARPETA$legend <- factor(t_CVDRCARPETA$legend, levels = c(unique(t_CVDRCARPETA$legend)))

t_CVDRCARPETA$label <- paste0(t_CVDRCARPETA$legend, "\n", t_CVDRCARPETA$perc, "%")

# Make the plot

titles <- paste0("Durante el tiempo que impartió clases a distancia en el ciclo escolar 2019-2020, \nsolicitó a los estudiantes que organizaran una carpeta de experiencias?")

ggplot(t_CVDRCARPETA, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=4) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=scale_s) +
  scale_color_manual(values=scale_s) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                     breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCARPETA_s", ".jpeg"), 
         width = 14, height = 10)


ggplot(t_CVDRCARPETA, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Respuesta)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Respuesta), size=4) +
  guides(color = FALSE) + # x here controls label position (inner / outer)
  scale_fill_manual(values=scale_t) +
  scale_color_manual(values=scale_t) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) + #Transform y scale to ln
  scale_y_continuous(limits = c(NA, NA), #Features for the x axis
                     breaks = seq(0, 100, 10)) + 
  labs(title = titles, #Text options
       x = " ",
       y = " ",
       hjust = 0,
       caption = legend_g) +
  theme_minimal()+ #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 12,
                                   family = "Open Sans"),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCARPETA_t", ".jpeg"), 
         width = 14, height = 10)

# CVDRCARPETAOT ------
# Tipo de variable: Categórica - Múltiple

# ¿De qué manera ha utilizado la carpeta de experiencias?

# Create dummy variables

docentes$CVDRCARPETAOT_O1[is.na(docentes$CVDRCARPETAOT_O1)] <- -2
docentes$CVDRCARPETAOT_O2[is.na(docentes$CVDRCARPETAOT_O2)] <- -2
docentes$CVDRCARPETAOT_O3[is.na(docentes$CVDRCARPETAOT_O3)] <- -2
docentes$CVDRCARPETAOT_O4[is.na(docentes$CVDRCARPETAOT_O4)] <- -2
docentes$CVDRCARPETAOT_O5[is.na(docentes$CVDRCARPETAOT_O5)] <- -2
docentes$CVDRCARPETAOT_O6[is.na(docentes$CVDRCARPETAOT_O6)] <- -2
docentes$CVDRCARPETAOT_O7[is.na(docentes$CVDRCARPETAOT_O7)] <- -2

docentes <- docentes %>% 
  mutate(CVDRCARPETAOT_c_01 = case_when(docentes$CVDRCARPETAOT_O1== 1 | docentes$CVDRCARPETAOT_O2 == 1 | docentes$CVDRCARPETAOT_O3 == 1 | docentes$CVDRCARPETAOT_O4 == 1 | docentes$CVDRCARPETAOT_O5 == 1 | docentes$CVDRCARPETAOT_O6 == 1 | docentes$CVDRCARPETAOT_O7 == 1 ~ 1),
         CVDRCARPETAOT_c_02 = case_when(docentes$CVDRCARPETAOT_O1== 2 | docentes$CVDRCARPETAOT_O2 == 2 | docentes$CVDRCARPETAOT_O3 == 2 | docentes$CVDRCARPETAOT_O4 == 2 | docentes$CVDRCARPETAOT_O5 == 2 | docentes$CVDRCARPETAOT_O6 == 2 | docentes$CVDRCARPETAOT_O7 == 2 ~ 1),
         CVDRCARPETAOT_c_03 = case_when(docentes$CVDRCARPETAOT_O1== 3 | docentes$CVDRCARPETAOT_O2 == 3 | docentes$CVDRCARPETAOT_O3 == 3 | docentes$CVDRCARPETAOT_O4 == 3 | docentes$CVDRCARPETAOT_O5 == 3 | docentes$CVDRCARPETAOT_O6 == 3 | docentes$CVDRCARPETAOT_O7 == 3 ~ 1),
         CVDRCARPETAOT_c_04 = case_when(docentes$CVDRCARPETAOT_O1== 4 | docentes$CVDRCARPETAOT_O2 == 4 | docentes$CVDRCARPETAOT_O3 == 4 | docentes$CVDRCARPETAOT_O4 == 4 | docentes$CVDRCARPETAOT_O5 == 4 | docentes$CVDRCARPETAOT_O6 == 4 | docentes$CVDRCARPETAOT_O7 == 4 ~ 1),
         CVDRCARPETAOT_c_05 = case_when(docentes$CVDRCARPETAOT_O1== 5 | docentes$CVDRCARPETAOT_O2 == 5 | docentes$CVDRCARPETAOT_O3 == 5 | docentes$CVDRCARPETAOT_O4 == 5 | docentes$CVDRCARPETAOT_O5 == 5 | docentes$CVDRCARPETAOT_O6 == 5 | docentes$CVDRCARPETAOT_O7 == 5 ~ 1),
         CVDRCARPETAOT_c_06 = case_when(docentes$CVDRCARPETAOT_O1== 6 | docentes$CVDRCARPETAOT_O2 == 6 | docentes$CVDRCARPETAOT_O3 == 6 | docentes$CVDRCARPETAOT_O4 == 6 | docentes$CVDRCARPETAOT_O5 == 6 | docentes$CVDRCARPETAOT_O6 == 6 | docentes$CVDRCARPETAOT_O7 == 6 ~ 1),
         CVDRCARPETAOT_c_07 = case_when(docentes$CVDRCARPETAOT_O1== 7 | docentes$CVDRCARPETAOT_O2 == 7 | docentes$CVDRCARPETAOT_O3 == 7 | docentes$CVDRCARPETAOT_O4 == 7 | docentes$CVDRCARPETAOT_O5 == 7 | docentes$CVDRCARPETAOT_O6 == 7 | docentes$CVDRCARPETAOT_O7 == 7 ~ 1))


docentes$CVDRCARPETAOT_c_01[is.na(docentes$CVDRCARPETAOT_c_01)] <- 0
docentes$CVDRCARPETAOT_c_02[is.na(docentes$CVDRCARPETAOT_c_02)] <- 0
docentes$CVDRCARPETAOT_c_03[is.na(docentes$CVDRCARPETAOT_c_03)] <- 0
docentes$CVDRCARPETAOT_c_04[is.na(docentes$CVDRCARPETAOT_c_04)] <- 0
docentes$CVDRCARPETAOT_c_05[is.na(docentes$CVDRCARPETAOT_c_05)] <- 0
docentes$CVDRCARPETAOT_c_06[is.na(docentes$CVDRCARPETAOT_c_06)] <- 0   
docentes$CVDRCARPETAOT_c_07[is.na(docentes$CVDRCARPETAOT_c_07)] <- 0   


Código <- c("CVDRCARPETAOT",
            " ",
            " ",
            " ",
            " ",
            " ",
            " ")
Variable <- c("¿De qué manera ha utilizado la carpeta de experiencias?",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ")
Respuesta <- c("Para realizar un diagnóstico de los aprendizajes",
               "Para retomar contenidos que requieran refuerzo",
               "Para evaluar el progreso de los aprendizajes con la enseñanza a distancia",
               "Para analizar el tipo de actividades que funcionaron de mejor manera",
               "Para que los propios estudiantes tengan conocimiento de su desempeño",
               "Para rendir cuentas del trabajo realizado por los alumnos",
               "No la pienso utilizar")


N <- sum(docentes$CVDRCARPETAOT_O1 != -1)

freq <- c(nrow(docentes[docentes$CVDRCARPETAOT_c_01==1,]),
          nrow(docentes[docentes$CVDRCARPETAOT_c_02==1,]),
          nrow(docentes[docentes$CVDRCARPETAOT_c_03==1,]),
          nrow(docentes[docentes$CVDRCARPETAOT_c_04==1,]),
          nrow(docentes[docentes$CVDRCARPETAOT_c_05==1,]),
          nrow(docentes[docentes$CVDRCARPETAOT_c_06==1,]),
          nrow(docentes[docentes$CVDRCARPETAOT_c_07==1,]))

perc <- c(round((nrow(docentes[docentes$CVDRCARPETAOT_c_01==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETAOT_c_02==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETAOT_c_03==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETAOT_c_04==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETAOT_c_05==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETAOT_c_06==1,])/N)*100, 2),
          round((nrow(docentes[docentes$CVDRCARPETAOT_c_07==1,])/N)*100, 2))

t_CVDRCARPETAOT <- data.frame(Código, Variable, Respuesta, freq, perc)

t_CVDRCARPETAOT$keep <- case_when(t_CVDRCARPETAOT$Respuesta == "Para retomar contenidos que requieran refuerzo" | 
                                    t_CVDRCARPETAOT$Respuesta == "Para evaluar el progreso de los aprendizajes con la enseñanza a distancia" | 
                                    t_CVDRCARPETAOT$Respuesta == "Para que los propios estudiantes tengan conocimiento de su desempeño" | 
                                    t_CVDRCARPETAOT$Respuesta == "Para rendir cuentas del trabajo realizado por los alumnos" ~ 1)
t_CVDRCARPETAOT <- t_CVDRCARPETAOT[complete.cases(t_CVDRCARPETAOT), ]

t_CVDRCARPETAOT$legend <- case_when(t_CVDRCARPETAOT$Respuesta == "Para retomar contenidos que requieran refuerzo" ~ "Para retomar contenidos \nque requieran refuerzo",
                                    t_CVDRCARPETAOT$Respuesta == "Para evaluar el progreso de los aprendizajes con la enseñanza a distancia" ~ "Para evaluar el progreso \nde los aprendizajes con la \nenseñanza a distancia",
                                    t_CVDRCARPETAOT$Respuesta == "Para que los propios estudiantes tengan conocimiento de su desempeño" ~ "Para que los propios \nestudiantes tengan \nconocimiento de su desempeño",
                                    t_CVDRCARPETAOT$Respuesta == "Para rendir cuentas del trabajo realizado por los alumnos" ~ "Para rendir cuentas \ndel trabajo realizado por \nlos alumnos") 

t_CVDRCARPETAOT$legend <- paste0(t_CVDRCARPETAOT$legend, "\n", t_CVDRCARPETAOT$perc, "%")

t_CVDRCARPETAOT$legend <- factor(t_CVDRCARPETAOT$legend, levels = c(unique(t_CVDRCARPETAOT$legend)))

# Plot

titles <- "¿De qué manera ha utilizado la carpeta de experiencias"

ggplot(t_CVDRCARPETAOT, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=school[4], size=6) +
  scale_y_continuous(breaks = c(seq(0, max(t_CVDRCARPETAOT$freq), 200))) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCARPETAOT_s", ".jpeg"), 
         width = 14, height = 10)

ggplot(t_CVDRCARPETAOT, aes(x=legend, y=freq)) +
  geom_segment( aes(x=legend, xend=legend, y=0, yend=freq), color="grey", size=1) +
  geom_point( color=teacher[5], size=6) +
  scale_y_continuous(breaks = c(seq(0, max(t_CVDRCARPETAOT$freq), 200))) +
  labs(title = titles, #Text options
       x = " ",
       y = "Frecuencia",
       hjust = 0,
       caption = legend_g) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,
                                  family = "Open Sans",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     face = "plain", 
                                     family = "Open Sans"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Open Sans",
                                    size = 10,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        legend.text = element_text(size = 22,
                                   family = "Open Sans"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 14, 
                                   family = "Open Sans"),
        legend.position = "none") +
  ggsave(paste0("05 Figures/01 Docentes/","CVDRCARPETAOT_t", ".jpeg"), 
         width = 14, height = 10)