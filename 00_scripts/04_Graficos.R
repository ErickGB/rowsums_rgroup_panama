# **********************************************
# R course - By Erick Gordon
# **********************************************
# Tema 3: Carga y manipulacion de datos
# **********************************************

library(ggplot2)
library(RColorBrewer) 

library(corrplot)
library(GGally)
# **********************************************
# carga de datos
data <- read.csv("2015.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
str(data)
head(data, 10)
# **********************************************

# bar plot
ggplot(data, aes(x=Region, y=Happiness.Score)) + 
  geom_bar(stat = "identity")

# trabajar la data antes para depurarla, filtrarla y ordenarla
library(dplyr)
temp <- data %>%
  group_by(Region) %>%
  summarize(Happiness.Score=mean(Happiness.Score), Total=n()) %>%
  arrange(desc(Happiness.Score))
temp
# plotea modificado
ggplot(temp, aes(x=factor(Region), y=Happiness.Score)) + 
  geom_bar(stat = "identity") 

# coloca en horizontal 
ggplot(temp, aes(x=factor(Region), y=Happiness.Score)) + 
  geom_bar(stat = "identity") + coord_flip() 

# agrega color 
ggplot(temp, aes(x=factor(Region), y=Happiness.Score, fill=Region)) + 
  geom_bar(stat = "identity") + coord_flip()  


# cambia colores 
ggplot(temp, aes(x=factor(Region), y=Happiness.Score, fill=Region)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  scale_fill_brewer(palette = "Pastel1")

temp$Region <- factor(temp$Region, c("Australia and New Zealand", "North America", 
                        "Western Europe", "Latin America and Caribbean", 
                        "Eastern Asia", "Middle East and Northern Africa", 
                        "Central and Eastern Europe", "Southeastern Asia", 
                        "Southern Asia", "Sub-Saharan Africa"), ordered = TRUE)


# cambia colores 
ggplot(temp, aes(x=factor(Region), y=Happiness.Score, fill=Region)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  scale_fill_brewer(palette = "RdBu")

ggplot(temp, aes(x=factor(Region), y=Happiness.Score, fill=Region)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  scale_x_discrete(limits=rev(levels(temp$Region))) + 
  scale_fill_brewer(palette = "RdBu")

# cambia la leyenda arriba y el nombre de las etiquetas 
ggplot(temp, aes(x=factor(Region), y=Happiness.Score, fill=Region)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  scale_x_discrete(limits=rev(levels(temp$Region))) + 
  scale_fill_brewer(palette = "RdBu") + theme(legend.position = "top") + 
  labs(fill="Regiones según la OCDE") + 
  guides(fill=guide_legend(reverse = TRUE))

# ****************************************************************-
# scatter plot 
str(data)
# basico 
ggplot(data, aes(x=Happiness.Score, y=Health..Life.Expectancy.)) + 
  geom_point()
# agrega color por region
ggplot(data, aes(x=Happiness.Score, y=Health..Life.Expectancy., colour=Region)) + 
  geom_point()
# area 
ggplot(data, aes(x=Happiness.Score, y=Health..Life.Expectancy., 
                 colour=Region, size=Happiness.Score)) + 
  geom_point()

# corrección para utilizar radio r = raiz cuadrada de A entre pi 
data %>%
  mutate(radio=sqrt((Happiness.Score/pi))) %>%
ggplot(aes(x=Happiness.Score, y=Health..Life.Expectancy., 
                 colour=Region, size=radio)) + 
  geom_point()

# ****************************************************
# box plot:  Muestra la dispersión

ggplot(data, aes(x=factor(Region), y = Happiness.Score, fill=Region)) + 
  geom_boxplot() 
  
# modificado
ggplot(data, aes(x=factor(Region), y = Happiness.Score, fill=Region)) + 
  geom_boxplot() +
  ggtitle("Dispersión por región") + 
  xlab("Region") + ylab("Score") +
  coord_flip() +  
  #scale_fill_brewer(palette = "Set1")
  scale_fill_manual(values = c("#1a1a1a", "#4d4d4d", "#878787", "#bababa", 
                               "#e0e0e0", "#ffffff", "#fddbc7", "#f4a582", 
                               "#d6604d", "#b2182b", "#67001f"))

display.brewer.all()

# ****************************************************
# face grid : vertical sub panels
# face wrap como palabras de una hoja


# cambia la leyenda arriba y el nombre de las etiquetas 
data %>%
  filter(Region %in% c("Latin America and Caribbean", "North America")) %>%
ggplot( aes(x=factor(Country), y=Happiness.Score, fill=Country)) + 
  geom_bar(stat = "identity") + coord_flip() +  
  #scale_fill_brewer(palette = "Spectral") + 
  facet_grid(Region ~ .)


# ****************************************************
# coorrelation
corrplot.mixed(cor(data[4:12]))

ggcorr(cor(data[4:12]), nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')








