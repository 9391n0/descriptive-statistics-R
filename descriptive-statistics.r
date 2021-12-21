#### Variables Cualitativas - Categorías ####

class(Sex)  # Es importante que esté pre-casteada como factor
as.factor(Sex) # Casteo
table(Sex)  # Tabla de frecuencia

# Para hacer un gráfico de barras:
counts.table <- table(Sex)
barplot(counts.table, col = "blue", density = 8 )

# Para gráfico de Tortas:
pie(counts.clase, col = c("blue","green","red"), main = "Título")

# Más de una variable categórica:
table <- counts(Sex,Pclass) # Tabla de contingencia

barplot(counts,col = c("blue","red"),main = "Sexo vs Clase", legend = rowname(counts)) # Si lo quiero por clase

counts <- table(Pclass,Sex) # Lo traspongo básicamente
barplot(counts,col = c("blue","red,green"),main = "Sexo vs Clase", legend = rownames(counts)) # Si lo quiero por clase

#### Variables Cuantitativas - Numéricas ####

data(iris)
attach(iris) # Fijo base así no tengo que andar especificando el dataframe todo el tiempo
head(iris)

# Si hay missing values hay que omitirlos
range(Sepal.Length) # Vector con mínimo en el el lugar [1], y el máximo en el lugar [2]
mean(Sepal.Length)
min()
max()
median()
length()
sd() # Desviación Standard
var()
cov()
cor() # Correlación
sort()
table() # Tabla de frecuencias absolutas
summary(Sepal.Length) # Medidas de resumen globales

#### Gráficos ####

# Histogramas

hist(Sepal.Length)

par(mfrow=c(1,2)) # Filas columnas de un grupo de 2 gráficos en conjunto
hist(Sepal.Length, main = "Histograma de frecuencia")
hist(Sepal.Length, freq = F, main = "Histograma de frecuencia")
par(mfrow = c(1,1)) # Es necesario cerrar con esto para que después los gráficos se sigan haciendo como antes, individualmente

# Otras opciones

hist(Sepal.Length, freq = F, col = "2", breaks = c(3,4,5,6), # Le especifico los cortes de los intervalos
	 xlab = "Longitud del sepalo",
	 main = "")
	 
hist(Sepal.Length, freq = F, col = "2", nclass = 15, # Les especifico la cantidad de cortes, haciéndomelos todos del mismo ancho
	 xlab = "Longitud del sepalo",
	 main = "")

# Para guardar líneas de comandos

pdf("histograma.pdf") # Formato y nombre del archivo que voy a guardar
par(mfrow = c(1,2))
hist(Sepal.Length_1)
hist(Sepal.Length_2)
par(mfrow = c(1,1))
graphics.off()# Cerramos la generación del pdf que comenzamos inmediatamente arriba

# Histogramas con densidad superpuesta

media.es <- mean(Sepal.Length)
desvio.es <- sd(Sepal.Length)
grilla <- seq(range(Sepal.Length)[1], range(Sepal.Length)[2], length = 100)

# Calculamos la densidad de la normal sobre la grilla

funn<- dnorm(grilla,media.es,desvio.es)

hist(Sepal.Length, nclass=15, freq = F, main = "Histograma")
lines(grilla,funn,col="blue",lwd = 2 ) # El lwd es el grosor

# Boxplot

quantile(data,0.25) # Cuartil Q1
quantile(data,0.75) # Cuartil Q3
IQR(data) # Rango intercuartil. (1,5 * IQR) es el largo de los bigotes del boxplot

boxplot(Sepal.Length)
boxplot(Sepal.Length ~ Species, xlab = "especies", main = "longitud del sepalo") # Los separamos en función de una categoría. Lo que está después de "~" clasifica lo que está a la izquierda
boxplot(Sepal.Length, Sepal.Width, names = c("longitud","ancho")) # Hacemos dos boxplots de dos variables distintas

# Gráfico de dispersión

plot(Sepal.Length,Sepal.Width) 
plot(Sepal.Length,Sepal.Width, xlab = "longitud", ylab = "ancho", main = "Plot de longitud vs. ancho", pch = 16) 
# Recordar que los "pch" son los tipos de puntos

# Más opciones para pares de variables:

plot(Sepal.Length,Sepal.Width, xlab = "longitud", 
     ylab = "ancho", 
     main = "Plot de longitud vs. ancho", pch = 16, type ="n") # Con la opción de "n" solo se grafica la caja
points(Sepal.Length[Sepal.Length<=6],Sepal.Width[Sepal.Length<=6],pch=20,col="magenta")
points(Sepal.Length[Sepal.Length>6],Sepal.Width[Sepal.Length>6],pch=20,col="green")

# Otras opciones:
#Cuadramos
par(bg = "lightgray",mar=c(4,2,3.5, 4))
#Hacemos la caja
plot(Sepal.Length,Sepal.Width,type ="n", xlim=c(4,8),
     ylim=c(1,6),xlab = "", ylab = "", xaxt="n", yaxt="n") 
points(Sepal.Length,Sepal.Width,pch=20,col="magenta")

# Ahora nos encargamos de los ejes
axis(1,c(4,6,8), cex=2) # "cex" indica el tamaño de la letra
mtext("Sepal.Length",side=1,cex=0.8,line=3) # El  "side" te dice de que lado del cuadrado
axis(4,cex=0.8,col="blue",labels=FALSE)
mtext(c(1,3,5),side=4,at=c(1,3,5),col="blue",line=0.3)
mtext("Sepal.Width",side=4,cex=0.8,line=2.5,col = "blue")
# Título
title("Diagrama Personalizado de Sepal.Length vs Sepal.Width",cex.main = 0.8)

# Gráficos entre todas las variables

pairs(SUB, col = "magenta") # Sirve para saber apriorísticamente si hay alguna relación clara entre las variables

#### GGPLOT ####

library("ggplot2")

ggplot(data = iris, aes(x=Sepal.Length, y = Sepal.Width))+
  geom_point()+ # Esta componente es la que determina el tipo de gráfico. Podría ser "geom_line()","geom_boxplot()"
  xlab("Longitud")+
  ylab("Ancho")+
  ggtitle("Título")

# Para hacer gráficos por distintas categorías

ggplot(data = iris, aes(x = Species, y = Sepal.Width))+
geom_boxplot(col = "red" )+ # Ese el color de las líneas de la geometría
xlab("Especie")+
ylab("Longitud del Sépalo")

# O bien el siguiente, que hace 3 distintos muy pegados, en vez de los 3 en 1

ggplot(data = iris, aes(x = "", y = Sepal.Width))+
geom_boxplot(col = "red" ) +
facet_grid(. ~ Species)# Vamos a hacer una grilla por separado para cada una de las condiciones, que en este caso son las especies
# El "~" te indica del lado derecho la variable condicionante

# Histogramas

ggplot(data = iris, aes(x = Sepal.Length))+
geom_histogram(bins = 15,col = "red") # El bins te especifica en cuantas categorías queremos dividir el rango de las variables

# Si queremos hacer el histograma de densidad

ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram(aes(y=..density..),breaks = c(4.5,5,6.3,6.8,8),col = "red") +
  xlab("Rango de Valores") +
  ggtitle("Histograma")

# Histogramas diferenciados por especies (binwidth indica el ancho del bin)
ggplot(data = iris, aes(x = Sepal.Length))+
geom_histogram(binwidth = 1, color = "black", aes (y = ..density.., fill = ..count..))+ # Especificamos el ancho del bin
facet_grid(. ~ Species)

# Histograma con densidad:

media.es <- mean(Sepal.Length)
desvio.es <- sd(Sepal.Length)

gg <- ggplot(data = iris, aes(x = Sepal.Length))
gg <- gg + geom_histogram(binwidth = 1, color = "black", aes(y = ..density.., fill = ..count..))
gg <- gg + scale_fill_gradient("Count", low = "#DCDCDC", high = "#7C7C7C")

gg <- gg + stat_function(fun = dnorm, color = "red", args = list(mean = media.es, sd =desvio.es))
gg + xlab("Rango de Valores") + ggtitle("Histograma")

# Gráficos para Variables Categóricas
ggplot(data = iris, aes(x = Species))+geom_bar(width = 0.25) 

# Le ponemos colores por categoría
ggplot(data = iris, aes(x = Species)) +
geom_bar(aes(fill = Species),width = 0.5) # Colores en función de la categoría

# Gráfico de tortas

ggplot(data = iris, aes(x = Species, fill = Species))+
  geom_bar(width = 1)+
  coord_polar()

# Cruzamos 2 categorías
fuma <- c(rbinom(50,1,0.3),rbinom(50,1,0.5),rbinom(50,1,0.1))
class(fuma)
fuma <- as.factor(fuma)

counts <- table(fuma,Species)
iris2 <- data.frame(cbind(iris,fuma))

ggplot(data = iris2, aes(x=Species))+ 
geom_bar(aes(fill=Species),width = 0.5) +
facet_grid(. ~ fuma)

# Guardado automático de gráficos:

ggsave("mi_grafico.pdf") # Puede ser otro formato, como .png

