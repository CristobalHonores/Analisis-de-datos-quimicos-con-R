#############
## Resumen ##
#############

# Importar bases de datos ####
library(readxl)
Litiasis <- read_excel("Bases de datos/Base Litiasis.xls")

# Primera vista nuestra base de datos ####
head(Litiasis) # Muestra las primeras filas de mi base
tail(Litiasis) # Muetra las ultimas filas dee mi base
names(Litiasis) # Muestra los nombres de las columnas de mi base
summary(Litiasis) # Realiza un resumen de mi base por columna

# Seleccionar columnas y filtros ####
Litiasis$EDAD # se utiliza el simbolo de $ para seleccionar una columna
Litiasis[Litiasis$FUMA==1,] # Selecciona las personas que fuman

# Recodificar bases de datos ####
Litiasis$FUMA_RECOD <- ifelse(Litiasis$FUMA==1,"Si","No") 
Litiasis$FUMA_RECO <- NULL # De esta forma se puede elminar una columna no deseada (no recomendable).


###############################
## Estadisticos Descriptivos ##
###############################

# De centro ####
mean(Litiasis$PESO) # Entrega el promedio del peso
mean(Litiasis$PESO[Litiasis$GENERO=="Hombre"]) # Entrega el promedio del peso de los hombres
median(Litiasis$TALLA) # Entrega la mediana de la talla
median(Litiasis[Litiasis$GENERO=="Mujer",]$TALLA) # Entrega la mediana de la talla de las mujeres
install.packages("modeest")
library(modeest)
mfv(Litiasis$EDAD) # Entrega la moda de la edad

# De posicion ####
quantile(Litiasis$PAS) # Cuartiles 
quantile(Litiasis$PAS, prob=seq(0,1, length=101)) # Percentiles

# De Variabilidad ####
range(Litiasis$TRIGLIC) # Rango del valor de Trigliceridos
IQR(Litiasis$TRIGLIC) # Rango intercuartil del valor de Trigliceridos
sd(Litiasis$TRIGLIC) # Desviacion estandar del valor de Trigliceridos
var(Litiasis$TRIGLIC) # Varianza del valor de Trigliceridos
sd(Litiasis$TRIGLIC)/length(Litiasis$TRIGLIC) # Error estandar del valor de Trigliceridos
sd(Litiasis$TRIGLIC)/mean(Litiasis$TRIGLIC) # Coeficiente de variacion del valor de Trigliceridos

##########################
## Tablas de frecuencia ##
##########################

# Tabla simple y de doble entrada ####
tabla1 <- table(Litiasis$GENERO) # Entrega cantidad de hombres y mujeres y la guarda como objeto
tabla1 # Muestra la tabla1 
tabla2 <- table(Litiasis$GENERO,Litiasis$LITIASIS) # Entrega una tabla por generno y litiasis
tabla2 # Muestra la tabla2 Mujeres y Hombres con y sin Litiasis

# add margins ####
addmargins(tabla1) # Agrega la suma total de la tabla 1
addmargins(tabla2) # Agrega la suma total de la tabla 2
addmargins(tabla2, margin = 1) # Agrega la suma por columna de la tabla 2 
addmargins(tabla2, margin = 2) # Agrega la suma por fila de la tabla 2 

# Tabla de proporciones ####
prop.table(tabla1) # Entrega la proporcion de hombres y mujeres
prop.table(tabla2) # Entrega la proporcion total de la tabla 2
prop.table(tabla2, margin = 1) # Entrega la porporcion de personas con Litiasis por genero (filas)
prop.table(tabla2, margin = 2) # Entrega la proporcion del genero por personas con y sin Litiasis (columnas)

# Paquete: fdth ####
install.packages("fdth")
library(fdth)
tabla3 <- fdt(Litiasis$EDAD,k=6)
print(tabla3)

########################
## Contorl de calidad ##
########################

# Diagrama de Pareto ####
install.packages("qcc")
library(qcc)
defect <- c(80, 27, 66, 94, 33)
names(defect) <- c("price code", "schedule date", "supplier code", "contact num.", "part num.")
pareto.chart(defect, ylab = "Error frequency", xlab = "Error causes",
             cumperc = seq(0, 100, by = 5))

# Diagrama de Causa-Efecto ####
cause.and.effect(cause=list(Measurements=c("Micrometers", "Microscopes", "Inspectors"),
                            Materials=c("Alloys", "Lubricants", "Suppliers"),
                            Personnel=c("Shofts", "Supervisors", "Training", "Operators"),
                            Environment=c("Condensation", "Moisture"),
                            Methods=c("Brake", "Engager", "Angle"),
                            Machines=c("Speed", "Lathes", "Bits", "Sockets")),
                 effect="Surface Flaws")

# Grafico de Dispersion ####
plot(Litiasis$PAS,Litiasis$PAD)
install.packages("ggplot2")
library(ggplot2)
ggplot(Litiasis, aes(x = PAS, y = PAD)) + geom_point()
install.packages("plotly")
library(plotly)
plot_ly(data = Litiasis, x = ~PAS, y = ~ PAD, type = "scatter")

# Matriz de dispersion ####
data(flea)
pairs(flea[2:4])
install.packages("GGally")
library(GGally)
p <- ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
ggplotly(p)

# Ejemplo extra
p <- ggpairs(iris, aes(colour = Species, alpha = 0.4))
ggplotly(p)

# Grafico de Caja ####
boxplot(Litiasis$COLHDL,horizontal = TRUE)
boxplot(Litiasis$COLHDL ~Litiasis$GENERO)
ggplot(Litiasis, aes(x = COLHDL, y = GENERO)) + geom_boxplot()
plot_ly(Litiasis, x = ~GENERO, y = ~COLHDL, type = "box")

# Grafico de Violin ####
install.packages("vioplot")
library(vioplot)
vioplot(Litiasis$COLHDL ~Litiasis$GENERO)
ggplot(Litiasis, aes(x = GENERO, y = COLHDL)) + geom_violin() + geom_boxplot()
plot_ly(Litiasis, x = ~GENERO, y = ~COLHDL, type = "violin",
        box = list(visible = T))
# Cartas de control ####
data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)

qcc(diameter[1:25,], type="xbar")
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], nsigmas=2)
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], confidence.level=0.99)

qcc(diameter[1:25,], type="R")
qcc(diameter[1:25,], type="R", newdata=diameter[26:40,])

qcc(diameter[1:25,], type="S")
qcc(diameter[1:25,], type="S", newdata=diameter[26:40,])

# agregar límites de advertencia en 2 desviaciones std. 
q <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)
(warn.limits <- limits.xbar(q$center, q$std.dev, q$sizes, 2))
plot(q, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")

detach(pistonrings)
##################
## Test Rosner  ##
##################
install.packages("EnvStats")
library(EnvStats)
# Ejemplo
set.seed(250) # Planta una semilla
dat <- c(rnorm(30, mean = 3, sd = 2), rnorm(3, mean = 10, sd = 1)) # Genera datos al azar con distibucion normal
rosnerTest(dat, k = 4) # Realiza el test para 4 posibles outliers
set.seed(NULL) # Desplanta la semilla
# Ejemplo aplicado
rosnerTest(Litiasis$COLHDL,k = 4)
