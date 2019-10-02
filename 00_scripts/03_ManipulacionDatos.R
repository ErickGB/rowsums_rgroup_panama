# **********************************************
# R course - By Erick Gordon
# **********************************************
# Tema 2: Carga y manipulacion de datos
# **********************************************
library(tidyverse)
# **********************************************
# carga de archivo
getwd()
data <- read.csv("./Data/2015.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
str(data)
head(data, 10)
View(data)
# **********************************************
#
# operadores lógicos 
# &   (y)
# |   (o)
# ==  (igual a)
# !=  (distinto de)
# **********************************************
#
# dplyr library

# 1. top 10 
data[1:10, ]

data %>%
  top_n(10)


data %>%
  glimpse()

datos_2 <-data %>%
  filter(Region == "Latin America and Caribbean") %>%
  select(Country, Region, Dystopia.Residual)

data %>%
  group_by(Region) %>%
  summarize(
    media = mean(Happiness.Score), 
    mediana = median(Happiness.Score),
    total = n()) %>%
  arrange(desc(media))


maximo_score <- max(data$Happiness.Score)
maximo_score

data %>% 
  filter(Happiness.Score == maximo_score)


data %>% 
  filter(Happiness.Score == max(Happiness.Score))





# ********************
# filter : filtra los registros según las condiciones indicadas
# el país más feliz  
datos2 <- data %>%
filter(Region == "Latin America and Caribbean" & Country == "Panama") %>%
group_by(Region, Country) %>%
summarize(
Media= mean(Happiness.Score),
Minimo= min(Happiness.Score),
Maximo= max(Happiness.Score),
Desviacion =sd(Happiness.Score))%>%
arrange(desc(Media))

print("mi salida ... ")

write.csv(datos2, "./Data/salida.csv")





data %>%
  filter(Happiness.Rank == min(data$Happiness.Rank))
# el menos feliz 
data %>%
  filter(Happiness.Rank == min(data$Happiness.Rank)) %>%
  select(Country, Happiness.Score)

# ********************
# arrange : ordena 
data %>%
  arrange(Happiness.Score) %>%
  select(Country, Happiness.Score)

data %>%
  arrange(desc(Happiness.Score)) %>%
  select(Country, Happiness.Score)

# ********************
# distinct: selecciona valores únicos, es decir si la variable tiene 10 registros pero solo cuatro
# categorías, listará las 4. 
data %>%
  distinct(Region)

# ********************
# mutate: Transforma una variable en otra agregandola al data.frame actual
data %>%
  mutate(se = Standard.Error * 100, otra = 'constante') %>%
  top_n(10, wt = se)

data %>%
  mutate(condicion = ifelse(Standard.Error > 0.05, 'mayor', 'menor')) %>%
  mutate(texto = paste(Region, Country, sep = "-")) %>%
  filter(Standard.Error  > 0.048 ) %>%
  head(10) %>%
  select(texto, Standard.Error, condicion)

# transmute: transforma a una nueva estructura, la anterior desaparece
data %>%
  transmute(se = Standard.Error * 100) %>%
  top_n(10)

# rename: cambia el nombre de una columna por otro nombre
data %>%
  rename(se = Standard.Error) %>%
  top_n(10)


# ********************
# sumarize: Suma los valores de una variable
data %>%
  summarize(Total=n(), Maximo=max(Happiness.Score) )

# group_by: agrupa 
data %>%
  group_by(Region) %>%
  summarize(Total=n(), Maximo=max(Happiness.Score) )

data %>%
  group_by(Region) %>%
  summarize(Total=n(), Maximo=max(Happiness.Score) ) %>%
  arrange(desc(Total))

# ********************
# sample 
# muestra aleatoria por cantidad 
data %>%
  sample_n(10)

# muestra aleatoria por fracción
data %>%
  sample_frac(0.10)




maximo_region <- function(region) {
  pais <- data %>% 
    filter(Region == region) %>% 
    filter(Happiness.Score == max(Happiness.Score)) %>% 
    select(Country)
  return(pais$Country[1])
  }

maximo_region("Western Europe")
maximo_region("Southern Asia")


resumen <- data %>% 
  summary_col_by_group(Region, col = Happiness.Score)


    data %>% 
    filter(Region == "Western Europe") %>% 
    filter(Happiness.Score == max(Happiness.Score)) %>% 
    select(Country) %>% 
      bind_rows(
          data %>% 
    filter(Region == "Southern Asia") %>% 
    filter(Happiness.Score == max(Happiness.Score)) %>% 
    select(Country)
      )

    
    

    data_2016 <- read.csv("./Data/2016.csv", 
                     header = TRUE, 
                     stringsAsFactors = FALSE)   
    head(data_2016)
    
    temp_2015 <-  data %>%
      mutate(year = 2015) %>%
      select(Region, Country, Happiness.Rank, Happiness.Score, year)
    
    temp_2016 <-  data_2016 %>%
      mutate(year = 2016) %>%
      select(Region, Country, Happiness.Rank, Happiness.Score, year)
    
    temp_total <- rbind(temp_2015, temp_2016)
    
    ?top_n()
    
    table(temp_total$year)
    
    temp_total %>%
      count(year)
    
    
    