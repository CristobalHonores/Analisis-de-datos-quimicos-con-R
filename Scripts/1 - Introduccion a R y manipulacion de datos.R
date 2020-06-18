####################
## Primeros pasos ##
####################
2+2
5^5

# Objetos ####
a <- 5
a
b <- 12
a+b
b^a
B <- 6
b <- 10
rm(a)
rm(b)
rm(B)

# Importar bases de datos con rio ####
install.packages("rio") #Solo instalar una vez
library(rio)
Litiasis <- import(file.choose()) # El comando file.chooce() me permite seleccionar el archivo de la base de datos mas facilmente

# Primera vista nuestra base de datos ####
head(Litiasis) # Muestra las primeras filas de mi base
tail(Litiasis) # Muetra las ultimas filas dee mi base
names(Litiasis) # Muestra los nombres de las columnas de mi base
summary(Litiasis) # Realiza un resumen de mi base por columna

# Seleccionar columnas y filas ####
Litiasis$EDAD # se utiliza el simbolo de $ para seleccionar una columna
Litiasis[,3] # Selecciona la tercera columna en este caso EDAD
Litiasis[1,] # Selecciona la primera fila
Litiasis[1,3] # Selecciona la primera fila y tercera columna

# Filtros ####
Litiasis[Litiasis$FUMA==1,] # Selecciona las personas que fuman
Litiasis[Litiasis$EDAD <= 35,] # Selecciona a los menores o iguales de 35
Litiasis$FUMA[Litiasis$EDAD <= 35] # Muestra los fumadores menores o iguales a 35
Litiasis[Litiasis$EDAD <= 35,]$FUMA # Muestra los fumadores menores o iguales a 

# Nuevas variables (Columnas) ####
Litiasis$Peso_libras = Litiasis$PESO * 2.2 # Genera una columna del peso en libras
head(Litiasis$Peso_libras)
Litiasis$Peso_libras <- NULL # Elimina la variable Peso_libras

# Recodificar bases de datos ####
Litiasis$FUMA_RECOD <- ifelse(Litiasis$FUMA==1,"Si","No") 
Litiasis$FUMA_RECO <- NULL # De esta forma se puede elminar una columna no deseada (no recomendable).

###########
## dplyr ##
###########

# Instalar y cargar paquetes ####
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)

# Cargar bases de datos ####
Cantidades1 <- read_excel("Bases de datos/BSA-TOC (1 de 2).xlsx", 
                          sheet = "Cantidades")
Cantidades2 <- read_excel("Bases de datos/BSA-TOC (2 de 2).xlsx", 
                          sheet = "Cantidades")
HPLC1 <- read_excel("Bases de datos/BSA-TOC (1 de 2).xlsx", 
                    sheet = "HPLC")
HPLC2 <- read_excel("Bases de datos/BSA-TOC (2 de 2).xlsx", 
                    sheet = "HPLC")
Zeta1 <- read_excel("Bases de datos/BSA-TOC (1 de 2).xlsx", 
                    sheet = "Zetasizer")
Zeta2 <- read_excel("Bases de datos/BSA-TOC (2 de 2).xlsx", 
                    sheet = "Zetasizer")

# Unir bases de datos ####
BSA1 <- full_join(Cantidades1,HPLC1,by="Codificacion") 
BSA1 <- full_join(BSA1,Zeta1,by="Codificacion")
View(BSA1)
BSA2 <- full_join(Cantidades2,HPLC2,by="Codificacion") 
BSA2 <- full_join(BSA2,Zeta2,by="Codificacion")
View(BSA2)
BSA <- bind_rows(BSA1,BSA2)
View(BSA)

# Seleccionar ####
Peso_Y_Talla <- select(Litiasis,PESO,TALLA) # Primero nombramos la base de datos y luego las columnas que deseamos seleccionar
Todas_menos_IDENT <- select(Litiasis,-IDENT) # Selecciona todas las columnas menos IDENT
Jovenes <- filter(Litiasis, EDAD > 35) # Crea un objeto con todos los datos de los menores de 35
Muestra <- sample_frac(Litiasis, 0.5) # Crea una muestra al azar de mi base de datos de un 50% de mis datos

# Resumenes ####
summarise(Litiasis,avg=mean(EDAD)) # Entrega el promedio de edad
