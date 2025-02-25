#Codigo para problema 2
mis_dades <- iris
mis_dades
mean(mis_dades$Sepal.Length) #media
sd(mis_dades$Sepal.Length) #desviacion estandar
dim(mis_dades) #dimensiones
names(mis_dades) #variables
hist(mis_dades$Sepal.Length) #histograma

#regresión por minimos cuadrados
x <- mis_dades$Petal.Length #asignar petal lenght a la variable x
x
y <- mis_dades$Sepal.Length #asignar sepal lenght a la variable y
y
plot(x,y) #grafico x vs y

m<- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2) # calculo de la pendiente (m)
m

b<- mean(y)-m*mean(x) #calculo de b
b

#trobar valor de sepal length quan petal length=1.5
m*1.5+b

#para predecir la recta
mod <- lm(y~x) #altgr+4 para el ~
summary(mod)
xpredict <- data.frame(x=1.5)
predict(mod, xpredict) #predecir el mismo valor que en la linea 24
xpredict<- data.frame(x=1:7)
xpredict<- data.frame(x=x)
xpredict
ypredict <- predict(mod, xpredict) #predecir cada valor de y en x
ypredict
lines(x, ypredict) #linia en el grafico donde se ven los valores predecidos

rsq <- sum((ypredict-mean(y))^2)/sum((y-mean(y))^2) #calculo de R^2
rsq

summary(mod) #R^2 redondeado
