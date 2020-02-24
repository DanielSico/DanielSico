#Ejer: Si la columna iris$Petal.Length tuviera valores 'NA', ¿Cómo podría eliminarlos antes de hacer la estimación de la media?
#Estime la media aritmética de la altura, la longitud y el ancho del caparazón de las tortugas pintadas [data(tortues), paquete: ade4], use la función apply para obtener los estadísticos de cada medida en un mismo vector.
pl.long <- na.omit(iris$Petal.Length)  
pl.long  
library(ade4)
data(tortues)
mi.promedio <- function(x) {
  mi.suma<- sum(x)
  mi.n<-length(x)
  mi.prom <- mi.suma/mi.n
  return(mi.prom)
}
long <- mi.promedio(x= tortues$long)
larg <- mi.promedio(x= tortues$larg)
haut <- mi.promedio(x= tortues$haut)

#Ejer: Genere una función propia que le permita encontrar la mediana, tal y como lo hizo con la media aritmética.
mi.mediana <- function(x) {
  return(median(x))
}
mi.mediana(x)

#Ejer: Genere una función propia que le permita calcular la moda
mi.moda <- function(x) {
  moda <- table(x)
  moda[which.max(moda)]
  as.numeric(names(moda[which.max(moda)]))
}
mi.moda(x)

#Ejer: Después de generar la función del cálculo de moda, grafíquela sobre el histograma.
abline(v = mi.moda(x),
       col = "black",
       lwd = 2)

#Ejer: Genere los cuantiles en intervalos de 0.5 Existen otras funciones que nos permiten generar cuartiles, incluyendo información de tendencias centrales. Es decir, density() pero más resumido.
quantile(x,probs = c(seq(0, 1, 0.5)))


#Ejer: ¿Qué sucede si el número de columnas deseado no se especifica? Obtenga la mediana, la moda y los cuantiles para cada especie de iris y para cada variable morfométrica.
aggregate(iris[,1:4], list(Especies = iris$Species), mean)
aggregate(iris[,1:4], list(Especies = iris$Species), median)
aggregate(iris[,1:4], list(Especies = iris$Species), mi.moda)
aggregate(iris[,1:4], list(Especies = iris$Species), quantile)

#Ejer: grafique la distribución de frecuencias de cada población usando histograma y densidad en el mismo gráfico. Adiciones la media, la mediana y la moda.
hist(pop1,
     col = "peachpuff",
     border = "black", 
     prob = TRUE,
     xlab = "Tamaño",
     main = "pop1")
lines(density(pop1),
      lwd = 2,
      col = "chocolate3")
abline(v = mean(pop1),
       col = "royalblue",
       lwd = 2)
abline(v = median(pop1),
       col = "red",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median"),
       col = c("chocolate3", "royalblue", "red"),
       lwd = c(2, 2, 2))
hist(pop2,
     col = "peachpuff",
     border = "black", 
     prob = TRUE,
     xlab = "Tamaño",
     main = "pop2")
lines(density(pop2),
      lwd = 2,
      col = "chocolate3")
abline(v = mean(pop2),
       col = "royalblue",
       lwd = 2)
abline(v = median(pop2),
       col = "red",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median"),
       col = c("chocolate3", "royalblue", "red"),
       lwd = c(2, 2, 2))