#############
## Resumen ##
#############
# Regresion lineal
# Metodo Stepwise
# PCR
# PLSR
# Paquetes ####
library(ggplot2)
library(plotly)
library(plyr)
# Histogramas ####
# Datos de ejemplo
set.seed(1234)
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5), rnorm(200, mean=65, sd=5)))
)
# Histograma basico
hist(df$weight)
ggplot(df, aes(x=weight)) + geom_histogram(color="black", fill="white",binwidth=1)
plot_ly(data = df,x = ~ weight, type = "histogram")

hist(df$weight)
ggplot(df, aes(x=weight, color=sex, fill=sex)) +  geom_histogram(binwidth=2)
f <- df[df$sex == "F",] ; m <- df[df$sex == "M",]
plot_ly(data = f,x = ~ weight, type = "histogram") %>% add_histogram(data= m, x= ~weight)

# Barras ####
# datos de prueba
df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
barplot(df$len ~ df$dose)
ggplot(data=df, aes(x=dose, y=len, fill = dose)) +  geom_bar(stat="identity")
plot_ly(data= df, x = ~dose, y = ~ len, type = "bar")
# Horizontal
barplot(df$len ~ df$dose, horiz = TRUE)
ggplot(data=df, aes(x=dose, y=len, fill = dose)) +  geom_bar(stat="identity") + coord_flip()
plot_ly(data= df, x = ~len, y = ~ dose, type = "bar",orientation = 'h')
# Barras apliladas
# Torta ####
# datos de prueba
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)
pie(table(data$value), labels = data$group)
ggplot(data, aes(x="", y=value, fill=group)) +  geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
attach(data)
plot_ly(data = data, labels = group, values = value, type = "pie")

# Grafico de lineas ####
# datos de prueba 
df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
plot(df$len);lines(df$len)
ggplot(data=df, aes(x=dose, y=len, group=1)) +  geom_line()+  geom_point()
plot_ly(data = df, x = ~dose, y = ~len, type = 'scatter', mode = 'lines')

# Grafico de burbujas ####
# datos de prueba 
data("mtcars");df <- mtcars
ggplot(df, aes(x = wt, y = mpg)) +  
  geom_point(aes(color = cyl, size = qsec), alpha = 0.5) +
  scale_size(range = c(0.5, 12))
plot_ly(data = df, x = ~wt, y = ~mpg, type = 'scatter',mode = 'markers',
        color = ~cyl, marker = list(size = ~qsec, opacity = 0.5))

# Graficos de Error ####
# datos de prueba
data_mean <- ddply(ToothGrowth, c("supp", "dose"), summarise, length = mean(len))
data_sd <- ddply(ToothGrowth, c("supp", "dose"), summarise, length = sd(len))
data <- data.frame(data_mean, data_sd$length)
data <- rename(data, c("data_sd.length" = "sd"))
data$dose <- as.factor(data$dose)

ggplot(data, aes(x=dose, y=length, fill = supp)) + 
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=length-sd, ymax=length+sd), width=.2,
                position=position_dodge(.9))

plot_ly(data = data[which(data$supp == 'OJ'),], x = ~dose, y = ~length, type = 'bar', name = 'OJ',
        error_y = ~list(array = sd,color = '#000000')) %>% 
  add_trace(data = data[which(data$supp == 'VC'),], name = 'VC')
# Treemap ####
install.packages("treemapify")
library(treemapify)

labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura")
parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
values= c(10, 14, 12, 10, 2, 6, 6, 1, 4)
df <- data.frame(labels, parents, values)
ggplot(df, aes(area = values, fill = parents, label = labels)) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "italic",
    colour = "white",
    place = "centre",
    grow = TRUE
  )
plot_ly(
  type='treemap',
  labels=labels,
  parents=parents,
  values= c(10, 14, 12, 10, 2, 6, 6, 1, 4),
  textinfo="label+value+percent parent+percent entry+percent root",
  domain=list(column=0))

# Sunburst ####
d <- data.frame(
  ids = c(
    "North America", "Europe", "Australia", "North America - Football", "Soccer",
    "North America - Rugby", "Europe - Football", "Rugby",
    "Europe - American Football","Australia - Football", "Association",
    "Australian Rules", "Autstralia - American Football", "Australia - Rugby",
    "Rugby League", "Rugby Union"
  ),
  labels = c(
    "North<br>America", "Europe", "Australia", "Football", "Soccer", "Rugby",
    "Football", "Rugby", "American<br>Football", "Football", "Association",
    "Australian<br>Rules", "American<br>Football", "Rugby", "Rugby<br>League",
    "Rugby<br>Union"
  ),
  parents = c(
    "", "", "", "North America", "North America", "North America", "Europe",
    "Europe", "Europe","Australia", "Australia - Football", "Australia - Football",
    "Australia - Football", "Australia - Football", "Australia - Rugby",
    "Australia - Rugby"
  ),
  stringsAsFactors = FALSE
)

plot_ly(d, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst')

# Tablas con plotly ####
plot_ly(
  type = 'table',
  header = list(
    values = c('<b>EXPENSES</b>', '<b>Q1</b>','<b>Q2</b>','<b>Q3</b>','<b>Q4</b>'),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12)
  ),
  cells = list(
    values = rbind(
      c('Salaries', 'Office', 'Merchandise', 'Legal', '<b>TOTAL</b>'),
      c(1200000, 20000, 80000, 2000, 1212000),
      c(1300000, 20000, 70000, 2000, 1392000),
      c(1300000, 20000, 120000, 2000, 1442000),
      c(1400000, 20000, 90000, 2000, 1412000)),
    line = list(color = '#506784'),
    fill = list(color = c('#25FEFD', 'white')),
    align = c('left', 'center'),
    font = list(color = c('#506784'), size = 12)
  ))

