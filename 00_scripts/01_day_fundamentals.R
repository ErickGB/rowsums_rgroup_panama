# ************************************************
# R course - By Erick Gordon
# ************************************************
# Date: 
# Tema 1: Instalacion y fundamentos
# ************************************************
# install libraries
# ************************************************
install.packages("tidyverse")
install.packages("janitor")
install.packages("DataExplorer")


#install.packages("lubridate")  
#install.packages("coorplot")
#install.packages("GGally")
#install.packages("RColorBrewer") 
#install.packages("bookdown")
# ************************************************
# Fundamentos
# ************************************************
# carga de librerias 
library(tidyverse) 

# **************************
# comandos 
ls()       # lista los objetos del espacio de trabjao 
getwd()    # ubicación actual 
# setwd(getwd())  # asigna nueva ubicación 
# rm # remueve objetos de memoria 
gc() # garbage collector, limpia la memoria. 

# **************************
# 1. operaciones matemáticas ----
1 + 1

20 * 10 

4/3

2 * 3 + 5

5 * (2 + 3)

5 ^ 2

# ************************
# 2. asignación de variables ----

x <- 2
y = 3

x
y

x * y
# ************************
# 3. remover una variable ----
rm(y)
y
# ************************
# 4. tipos de datos ----
# integer: entero
# numeric: decimales y flotantes
# character: cadenas de caracters (nombres)
# Date/POSIXct : tipo de datos fecha
# logical valores TRUE o FALSE 

is.numeric(x) # es automáticamente asumido por el motor

is.integer(x)

x <- 5L
is.integer(x)

class(5L)

class(10L*2L)

class(10L/2L)


# caracteres
a <- "Hola Mundo!"
a

# factor
a <- factor("Hola Mundo!")
b <- as.factor(c("A", "B", "C", "A"))
b

lista <- c("A", "B", "C", "A")
lista


a <- "Hola Mundo!"
nchar(a)
"Hola Mundo!"
12345678901
nchar(1)
nchar(234)

paste(a, ".. R Panama", "viva")
paste0(a, ".. R Panama")

substr(a, 1, 4)
substr(a, 6, 10)
# **********
# 4. Fechas

date1 <- as.Date(" 2012-06-28")
date1
class(date1)

date2 <- as.POSIXct(" 2012-06-28 17: 42")
date2
class(date2)

# **********
# 4. Logicos

2 == 3

2 != 3

2 > 3 
"data" == "state"
"data" == "data"
# **********
# 4. Operaciones con vector

x <- c(2, 50, 3, 9, 11, 20, 31, 45, -1, 0, 16)
x
x * 3

x + 2

x / 5

x <- 0:15
x
y <- -5:10
y

x + y
# sin son de diferentes longitudes genera un error.

# verifica la longitud
length(x)

# comparando vectores
y > x



nombres <- c("maria", "jose", "abril", "nickole")
nombres
length(nombres)
nchar(nombres)

nombres <- c("maria", "jose", "abril", 
             "nickole", "jose")
nombres
factor(nombres)

# presenta el segundo nombre
nombres[2]
# presenta los dos primeros nombres
nombres[1:3] # de la posición 1 a la 2
nombres[c(1, 4)] # la posición 1 y la 4 del vector




# ************************************************--
# Taller 1 ----
# ************************************************--

# 1. Cual es el resultado de la operación siguiente
7 + 2 * 2 - 2 
# a) 16
# b) 9
# c) 14

# 2. Cual es el resultado de la operación siguiente
12 / 2 + 2 - 1 
# a) 2
# b) 4
# c) 7

# 3. Cree un vector con 10 números enteros

# 4. Cree un vector con 10 números decimales

# 5. Multiplique ambos vectores

# 6. Cree un vector con 5 nombres

# 7. Imprima el tercer nombre del vector

# 8. Imprima los nombres del 2 al 4

