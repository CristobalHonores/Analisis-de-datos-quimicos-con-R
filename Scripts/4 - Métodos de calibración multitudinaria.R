#############
## Resumen ##
#############

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

#################
## Calibracion ##
#################

# Paquete y data ####
install.packages("pls")
library(pls)
data("gasoline")
gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]

# lm ####
# Base de datos para ejemplo
install.packages("mlbench")
library(mlbench)
data(Glass)
# Regresion linal simple
reg1 <- lm(Glass$RI ~ Glass$Ca) # Realiza una regresion enre RI y Ca
summary(reg1) # Entrega el resumen de la regresion
pairs(Glass)  # Permite visualizar si existe mas relaciones
# Regresion linial multiple
reg2 <- lm(RI ~ ., data=Glass) # Hace regresion con TODAS las variables
summary(reg2) # No muy preciso
# Step ####
step1 <- step(reg2) # Realiza el Step de la regresion 2
summary(step1) 

step2 <- step(reg2,direction = "both") # Realiza el Step por ambos metodos
summary(step2)

step3 <- step(reg2,direction = "backward") # Realiza la regresion por backward
summary(step3)

step4 <- step(reg2,direction = "forward") # Realiza la regresion por forward
summary(step4)

# pcr ####
gas1 <-  pcr(octane ~ NIR, 10, data = gasTrain, validation = "LOO") # Realiza la regresion por PCR
summary(gas1) # Resumen de la regresion
plot(RMSEP(gas1), legendpos = "topright") # Permite encontrar la cantidad de componentes mas optimos
plot(gas1, ncomp = 3, asp = 1, line = TRUE) # Muestra el error entre los predichos y medidos
plot(gas1, plottype = "scores", comps = 1:4)
explvar(gas1) # Muestra el porcentaje de variabilidad explicada por cada componte
plot(gas1, "loadings", comps = 1:3, legendpos = "topleft",
     labels = "numbers", xlab = "nm")
abline(h = 0)

# pls ####
gas2 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO") # Realiza la regresion por PLSR
summary(gas2) # Resumen de la regresion
plot(RMSEP(gas2), legendpos = "topright") # Permite encontrar la cantidad de componentes mas optimos
plot(gas2, ncomp = 2, asp = 1, line = TRUE) # Muestra el error entre los predichos y medidos
plot(gas2, plottype = "scores", comps = 1:3)
explvar(gas2) # Muestra el porcentaje de variabilidad explicada por cada componte
plot(gas2, "loadings", comps = 1:2, legendpos = "topleft",
     labels = "numbers", xlab = "nm")
abline(h = 0)

# Predecir ####
# PCR
pred1 <- predict(gas1, ncomp = 3, newdata = gasTest) ; pred1 # Predice los valores de mis nuevos datos
RMSEP(gas1, newdata = gasTest)
mean((gasTest$octane - pred1)^2) # Entrega el MSE

# PLS
pred2 <- predict(gas2, ncomp = 2, newdata = gasTest) ; pred2 # Predice los valores de mis nuevos datos
RMSEP(gas2, newdata = gasTest)
mean((gasTest$octane - pred2)^2) # Entrega el MSE
