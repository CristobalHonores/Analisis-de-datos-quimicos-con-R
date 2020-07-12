#############
## Resumen ##
#############

# Estaditicos De centro ####
library(dplyr)
mean(Litiasis$PESO) # Entrega el promedio del peso
hombres <- filter(Litiasis, GENERO == "Hombre") # Filtra solo por hombres
mean(hombres$PESO) # Entrega el promedio del peso de los hombres
median(Litiasis$TALLA) # Entrega la mediana de la talla
mujeres <- filter(Litiasis, GENERO=="Mujer") # Filtra solo por mujeres
median(mujeres$TALLA) # Entrega la mediana de la talla de las mujeres
install.packages("modeest")
library(modeest)
mfv(Litiasis$EDAD) # Entrega la moda de la edad

# Estaditicos De posicion ####
quantile(Litiasis$PAS) # Cuartiles 
quantile(Litiasis$PAS, prob=seq(0,1, length=101)) # Percentiles

# Estaditicos De Variabilidad ####
range(Litiasis$TRIGLIC) # Rango del valor de Trigliceridos
IQR(Litiasis$TRIGLIC) # Rango intercuartil del valor de Trigliceridos
sd(Litiasis$TRIGLIC) # Desviacion estandar del valor de Trigliceridos
var(Litiasis$TRIGLIC) # Varianza del valor de Trigliceridos
sd(Litiasis$TRIGLIC)/length(Litiasis$TRIGLIC) # Error estandar del valor de Trigliceridos
sd(Litiasis$TRIGLIC)/mean(Litiasis$TRIGLIC) # Coeficiente de variacion del valor de Trigliceridos

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

# Test Rosner  ####
install.packages("EnvStats")
library(EnvStats)
# Ejemplo
set.seed(250) # Planta una semilla
dat <- c(rnorm(30, mean = 3, sd = 2), rnorm(3, mean = 10, sd = 1)) # Genera datos al azar con distibucion normal
rosnerTest(dat, k = 4) # Realiza el test para 4 posibles outliers
set.seed(NULL) # Desplanta la semilla

#############################
## Comparacion de muestras ##
#############################

# Base de datos ####
install.packages("carData") # Paquete de bases de datos
library(carData)
data("Pottery") # Composicion de ceramicas de diferentes lugares
# Base para test de medias y varianzas
Llan <- filter(Pottery , Site == "Llanedyrn")
Ashley <- filter(Pottery, Site == "AshleyRails")

# Bases para test de proporciones
Y1 <- ifelse(Pottery$Site == "Llanedyrn",1,0)
exitos1 <- sum(Y1)
n1 <- length(Y1)

Y2 <- ifelse(Pottery$Site == "AshleyRails",1,0)
exitos2 <- sum(Y2)
n2 <- length(Y2)

# Test hipotesis media ####
mean(Ashley$Al)
# Varianza poblacional desconocida:
t.test(Ashley$Al,alternative = "two.sided",conf.level = 0.95, mu=17) # Realiza el test para mu = 17
# Si es igual a 17
t.test(Ashley$Al,alternative = "less",mu = 18, conf.level = 0.95) # Realiza el test para mu < 18
# No es menor a 18
t.test(Ashley$Al,alternative = "greater", mu = 16, conf.level = 0.95) # Realiza el test para mu > 16
# No es mayor a 16
as.numeric(t.test(Ashley$Al,alternative = "two.sided",
                  conf.level = 0.95)$conf.int) # Entrega el intervalo deconfianza

# Varianza poblacional conocida:
install.packages("TeachingDemos")
library(TeachingDemos)
sigma <- sd(Llan$Al) # Varianza "poblacional"
z.test(Llan$Al,sd=sigma, alternative="two.sided",conf.level = 0.95, mu=17) # test con varianza POBLACIONAL conocida
# No es igual a 17
as.numeric(z.test(Llan$Al,sd=sigma, alternative="two.sided",
                  conf.level = 0.95)$conf.int) # Entrega el intervalo deconfianza
# Test de proporciones ####
prop.test(exitos1,n1, alternative = "two.sided",conf.level = 0.95) # Si no se coloca valor de p
# Se asume criterio de varianza maxima (p=0.5)
# La proporcion de ceramicas de Llanedyrn es igual a 0.5
as.numeric(prop.test(exitos1,n1, alternative = "two.sided",conf.level = 0.95)$conf.int)

# Test de varianza ####
sigma.test(Ashley$Al,alternative = "two.sided", conf.level = 0.95) # Test de una varianza
# La varianza no es igual a 1
as.numeric(sigma.test(Ashley$Al,alternative = "two.sided",
                      conf.level = 0.95)$conf.int) # Entrega el intervalo deconfianza
# Comparacion de 2 muestras ####
# Varianza:
var.test(Ashley$Al,Llan$Al,alternative = "two.sided",conf.level = 0.95) # test de 2 varianzas
# No tienen varainzas iguales

# Medias con varainzas iguales:
t.test(Ashley$Al,Llan$Al, alternative = "two.sided",conf.level = 0.95, var.equal = TRUE)
# OJO! Este resultado no es correcto porque no tienen varainzas iguales (SOLO EJEMPLO)

# Medias con varianzas distintas:
t.test(Ashley$Al,Llan$Al, alternative = "two.sided",conf.level = 0.95, var.equal = FALSE)
# Las medias no son iguales

# Proporciones:
prop.test(x=c(exitos1,exitos2), n=c(n1,n2), alternative = "two.sided", conf.level = 0.95)
# Las proporciones nos son iguales
prop.test(x=c(exitos1,exitos2), n=c(n1,n2), alternative = "greater", conf.level = 0.95)
# La proporcion de ceramicas de Llanedyrn es mayor que las de AshleyRails
# Anova ####
anova <- aov(Pottery$Al ~ Pottery$Site) # Realiza el test de ANOVA
summary(anova) # Entrega la tabal ANOVA apretando la tecla Enter EN LA CONSOLA
plot(anova) # Entrega multiples graficos
# Primer grafico: Residuos vs el ajuste
# Segundo grafico: Muestra normalidad de los residuos
# Tercer grafico: Entrega la escala
# Cuarto grafico: Ayuda a visualizar anomalos
TukeyHSD(anova) # Muestra la diferencia de medias (Entrega el orden de las medias)
plot(TukeyHSD(anova)) # Enseña las diferencias de manera grafica


# Kurskal ####
kruskal.test(Pottery$Al ~ Pottery$Site) # Realiza test de kruskal
pairwise.wilcox.test(Pottery$Al ,Pottery$Site, 
                     p.adjust.method = "holm") # Entrega diferencia de medias
