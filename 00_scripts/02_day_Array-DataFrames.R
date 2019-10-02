# **********************************************
# R course * By Erick Gordon
# **********************************************
# Date: 29May2017
# Tema 2: Arreglo, Matrices, Listas y DataFrames
# **********************************************

# *******************
# arreglos ----
# todos del mismo tipo de datos, con hasta dos dimensiones (vectores)

misDatos <- c(12, 10, 7, -3, 0, 11) # vector
x <- array(misDatos, dim = c(3,2))
x 
x[1, ] # selecciono la fila 1
x[, 2] # selecciono la columna 3
x[2, 2] # selecciono la celda ubicada en la fila 1 columna 3

x > 10 # cuales valores del arreglo que son mayores de 10
x[x>10] # filtra los valores de la lista mayores a 10
x[,2] > 10
x[, ]
# operadores lógicos 
# == igual
# != distinto
# < menor que 
# > mayor que
# <= menor o igual que
# >= mayor o igual que 

# *******************
# matriz ----
# todos del mismo tipo de datos, con una  MAS de dos dimensiones (vectores)
misDatos <- c(12, 10, 7, -3, 0, 11, -7, 7, 8)
x <- matrix(misDatos, nrow = 3, ncol = 3)
x

x[2:3, ] # selecciono las filas de la 2 a la 3 con todas las columnas
x[2:3, 1] # selecciono las filas de la 2 a la 3 pero solo la columna 1

# *******************
# listas ----
# pueden contener dos o más tipos de datos diferentes
persona <- list("Maria", 1995L)
persona[1]
persona[2]
# 
persona <- list(nombre="Maria", anio=1995L)
persona["nombre"]
persona$nombre
persona$anio
persona$edad <- 22
persona

# *******************
# dataFrames ----
# es como tener listas ordenadas en filas y columnas
nombres <- c("Maria", "Juan", "Diana")
edades <- c(22, 19, 27)
anios <- c(1995L, 1198L, 1990L)

personas <- data.frame(nombre=nombres, 
                       edad=edades, 
                       anio=anios)



personas
str(personas)
View(personas)

personas[1, ] # la primera persona del df
personas[1, 2] # la primera persona del df

personas$nombre

personas2 <-  personas[personas$anio >= 1990L, ]

personas$salario <- c(1500, 2000, 800)
personas$deducciones <- c(500, 500, 500)
personas$comprometido <- personas$deducciones/personas$salario *100
personas



# *******************
# Sentencias 
# if, else, ifelse
# condiciones combinadas * and | or 
# for
# while

# if
x <- 20
if(x > 10) print("Si")

x <- 5
if(x > 10) print("Si") # la condicion no se cumple y no hay salida

if(x > 10) print("imprime Si") else print("imprime no") # agrega condicion sino

salida <- ifelse(x > 10, "imprime Si", 
                 "imprime No") # forma abreviada (usar cuando la condición es corta)
print(salida)

# condicion larga utiizar if .. else
if(x > 20) {
  # ... 
  x <- x + 100
  print(x)
} else {
  x <- x * 1
  print(x)
}

z <- 44
if(x < 20 & z < 5) { # y 
  print("condicion completada")
} else { 
  print("no se cumple la condición")
}

x <- 10
z <- 1
if(x > 20 | z < 5) { # o  
  print("condicion completada")
} else { 
  print("no se cumple la condición")
}

# ********************
# ciclos 

# for
# realiza tarea repetitivamente cierta cantidad definida de veces
numeros <- c(1, 7, 9, 0, 11, 41, 12)
numeros
length(numeros)
for(i in 1:5) {
  print(paste(i, numeros[i], sep=": "))
}

# while 
# realiza tarea repetitivamente hasta que se cumpla la condición
i <- 1
while(i < 120) 
{
  i <- i + 20
  print(i)
}

# se pueden agregar condiciones lógicas 
i <- 1
while(i < 100) 
{
  i <- i + 20
  print(i)
  if(i >= 60) 
    i <- 101
}


# ********************
# funciones
x <- c(1, 7, 9, 0, 11, 41, 12)
mean(x) # media
var(x) # varianza 
sd(x) # desviacion estándar

summary(x)

my_function <- function (cadena) {
  return (paste0("Hola ", toupper(cadena)))
}

my_function("Erick")
my_function("Again!")

# cadena de caracteres
cadena <- "R es sin duda es uno de los más útiles a la hora de estructurar y manipular datos"
nchar(cadena) # cantida de caracteres, cada letra, espacio o símbolo es considerado un caracter

chartr(" ", "_", cadena) # reemplaza un caracter por otro

strsplit(cadena, " ") # separa los caracteres basado en un valor

substr(cadena, 1, 10) # selecciona las cadenas de 1 a 10 

toupper(cadena) # convierte mayuscula 

tolower(cadena) # a minisucula 



# ***********************************************
# Taller 2
# ***********************************************

# Dado 
misDatos <- c(12, 10, 7, -3, 0, 11)
x <- array(misDatos, dim = c(3,2))

# 1. Genere la condición que haga que se impriman solo los numeros menores o iguales a cero.

# 2. presente la fila 2 y 3

# 3. presente el valor de ubicado en la celda de la fila 3, columna 1

# 4, Cree una DataFrame de 10 personas con las siguientes caracteristicas: Nombre, cedula, salario, edad

# 5. Genere la condicion para establecer todos las las características de la segunda persona de la lista

# 6. Cual es la media de salario 

# 7. Cual es el salario más alto

# 8. Genera la condición para presenta la persona con el salario más alto

# 9. Genera la condición para presentar la persona con menor edad

# 10. Cree una funcion que le pase el nombre de la persona y le traiga el salario

# 11. Cree una funcion que le pase el nombre de la persona y devuelva el primer número de la cédula

# 12. Cree un ciclo que imprima los numeros pares de un vector de 10 números



