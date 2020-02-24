#Ejer:  Lea el artículo: http://www.limnetica.com/documentos/limnetica/limnetica-9-1-p-67.pdf Simule las variables que evalúan en el estudio. Aplique los descriptivos estadísticos que hemos visto hasta el momento. Grafique. ¿Qué otras preguntas de investigación podría formular usando esos datos? Tome los diagramas de flujo de test estadísticos y de graficado. Tome una de las preguntas que formuló y diseñe el análisis.

tomadata <- function(m, d, min, max) {
  pob <- rnorm(100, m, d)
  muestra <- pob[min <= pob & pob <= max]
  return(sample(muestra, 30))
}
tomadata2 <- function(m, d, min, max) {
  pob <- rnorm(100, m, d)
  muestra <- pob[min <= pob & pob <= max]
  return(sample(muestra, 23))
}
estandar <- function(list, datafram){
  dataframe <- list[[datafram]]
  desv <- cbind(sd(dataframe[,1]), sd(dataframe[,2]), sd(dataframe[,3]), sd(dataframe[,4]))
  meadi <- cbind(mean(dataframe[,1]), mean(dataframe[,2]), mean(dataframe[,3]), mean(dataframe[,4]))
  error <- desv [,1:4]/length(dataframe[,1])
  egress <- rbind(desv, meadi, error)
  rownames(egress) <- c("desviacion", "media", "error")
  colnames(egress) <- c("Eje Mayor(mm)", "Eje menor(mm)", "Volumen(ml)", "Peso(mg)")
  return(egress)
}

set.seed(999)

Ayamonte <- list(
  EstadioI = data.frame(cbind("Eje mayor(mm)" = (tomadata(0.91, 0.049, 0.8, 1)),
                              "Eje menor(mm)" = tomadata(0.683, 0.047, 0.6, 0.8),
                              "Volumen(ml)" = tomadata(0.223, 0.034, 0.15, 0.33),
                              "Peso(mg)" = tomadata(0.492, 0.142, 0.36, 0.64))),
  EstadioII = data.frame(cbind("Eje mayor(mm)" = (tomadata(0.946, 0.046, 0.8, 1)),
                                 "Eje menor(mm)" = tomadata(0.716, 0.059, 0.6, 0.9),
                                 "Volumen(ml)" = tomadata(0.247, 0.055, 0.16, 0.42),
                                 "Peso(mg)" = tomadata(0.430, 0.154, 0.25, 0.53))),
  EstadioIII = data.frame(cbind("Eje mayor(mm)" = (tomadata(1.03, 0.055, 0.9, 1.1)),
                                  "Eje menor(mm)" = tomadata(0.793, 0.021, 0.7, 0.8),
                                  "Volumen(ml)" = tomadata(0.339, 0.025, 0.27, 0.36),
                                  "Peso(mg)" = tomadata(0.428, 0.106, 0.33, 0.54))),
  EstadioIV = data.frame(cbind("Eje mayor(mm)" = (tomadata(1.098, 0.038, 0.9, 1.2)),
                                 "Eje menor(mm)" = tomadata(0.81, 0.038, 0.75, 0.9),
                                 "Volumen(ml)" = tomadata(0.386, 0.041, 0.32, 0.5),
                                 "Peso(mg)" = tomadata(0.576, 0.12, 0.43, 0.66))),
  EstadioVab = data.frame(cbind("Eje mayor(mm)" = (tomadata(1.196, 0.029, 1.1, 1.3)),
                                  "Eje menor(mm)" = tomadata(0.816, 0.035, 0.8, 0.9),
                                  "Volumen(ml)" = tomadata(0.418, 0.039, 0.36, 0.5),
                                  "Peso(mg)" = tomadata(0.434, 0.163, 0.27, 0.6))),
  EstadioVc = data.frame(cbind("Eje mayor(mm)" = (tomadata(1.555, 0.081, 1.4, 1.7)),
                                 "Eje menor(mm)" = tomadata(0.851, 0.049, 0.8, 0.9),
                                 "Volumen(ml)" = tomadata(0.593, 0.081, 0.46, 0.72),
                                 "Peso(mg)" = tomadata(0.785, 0.221, 0.63, 1.04)))
)

Villafranco <- list(
  EstadioI = data.frame(cbind("Eje mayor(mm)" = (tomadata(0.866, 0.046, 0.8, 0.95)),
                              "Eje menor(mm)" = tomadata(0.731, 0.02, 0.65, 0.8),
                              "Volumen(ml)" = tomadata(0.243, 0.03, 0.19, 0.3),
                              "Peso(mg)" = tomadata(0.393, 0.099, 0.28, 0.48))),
  EstadioII = data.frame(cbind("Eje mayor(mm)" = (tomadata(0.901, 0.044, 0.8, 1)),
                               "Eje menor(mm)" = tomadata(0.756, 0.043, 0.65, 0.8),
                               "Volumen(ml)" = tomadata(0.271, 0.032, 0.19, 0.31),
                               "Peso(mg)" = tomadata(0.347, 0.008, 0.33, 0.35))),
  EstadioIII = data.frame(cbind("Eje mayor(mm)" = (tomadata(0.963, 0.066, 0.8, 1.1)),
                                "Eje menor(mm)" = tomadata(0.783, 0.04, 0.7, 0.85),
                                "Volumen(ml)" = tomadata(0.310, 0.041, 0.23, 0.37),
                                "Peso(mg)" = tomadata(0.439, 0.078, 0.36, 0.64))),
  EstadioIV = data.frame(cbind("Eje mayor(mm)" = (tomadata(1.05, 0.055, 0.95, 1.2)),
                               "Eje menor(mm)" = tomadata(0.781, 0.035, 0.7, 0.85),
                               "Volumen(ml)" = tomadata(0.336, 0.035, 0.28, 0.41),
                               "Peso(mg)" = tomadata(0.43, 0.01, 0.41, 0.43))),
  EstadioVab = data.frame(cbind("Eje mayor(mm)" = (tomadata(1.09, 0.08, 0.95, 1.2)),
                                "Eje menor(mm)" = tomadata(0.843, 0.052, 0.75, 0.9),
                                "Volumen(ml)" = tomadata(0.41, 0.074, 0.29, 0.5),
                                "Peso(mg)" = tomadata(0.462, 0.089, 0.36, 0.54))),
  EstadioVc = data.frame(cbind("Eje mayor(mm)" = (tomadata2(1.41, 0.146, 1.3, 1.7)),
                               "Eje menor(mm)" = tomadata2(0.843, 0.058, 0.8, 1),
                               "Volumen(ml)" = tomadata2(0.527, 0.092, 0.43, 0.72),
                               "Peso(mg)" = tomadata2(0.384, 0.087, 0.28, 0.45)))
)

plotea <- function(list, co, nombre){
  par(mfrow = c(2, 3))
  hist(list[[1]][,co], probability = T, main = "", xlab = "", ylab = "", xlim = range(c(list[[1]][,co], list[[2]][,co], list[[3]][,co], list[[4]][,co], list[[5]][,co], list[[6]][,co])))
  lines(density(list[[1]][,co]))
  hist(list[[2]][,co], probability = T, main = c(nombre,colnames(list[[1]])[co]), xlab = "", xlim = range(c(list[[1]][,co], list[[2]][,co], list[[3]][,co], list[[4]][,co], list[[5]][,co], list[[6]][,co])), ylab = "")
  lines(density(list[[2]][,co]))
  hist(list[[3]][,co], probability = T, main = "", xlab = "", ylab = "", xlim = range(c(list[[1]][,co], list[[2]][,co], list[[3]][,co], list[[4]][,co], list[[5]][,co], list[[6]][,co])))
  lines(density(list[[3]][,co]))
  hist(list[[4]][,co], probability = T, main = "", xlab = "", ylab = "Densidad", xlim = range(c(list[[1]][,co], list[[2]][,co], list[[3]][,co], list[[4]][,co], list[[5]][,co], list[[6]][,co])))
  lines(density(list[[4]][,co]))
  hist(list[[5]][,co], probability = T, main = "", xlab = "Rango", ylab = "", xlim = range(c(list[[1]][,co], list[[2]][,co], list[[3]][,co], list[[4]][,co], list[[5]][,co], list[[6]][,co])))
  lines(density(list[[5]][,co]))
  hist(list[[6]][,co], probability = T, main = "", xlab = "", ylab = "", xlim = range(c(list[[1]][,co], list[[2]][,co], list[[3]][,co], list[[4]][,co], list[[5]][,co], list[[6]][,co])))
  lines(density(list[[6]][,co]))
}

for (i in 1:4) {
  plotea(Ayamonte, i, "Ayamonte")
  
}

for (i in 1:4) {
  plotea(Villafranco, i, "Villafranco")
  
}

for (i in 1:6) {
  View(estandar(Ayamonte, i))
}
for (i in 1:6) {
  View(estandar(Villafranco, i))
}

#Existe una evolucion en el tamaño de los huevos a medida que avanza el tiempo de gestacion?
#
#Es posible observar una clara tendencia en el volumen de los huevos a expandirse, esta tiene un ritmo lento al principio, sin embargo cuando ya se encuentran en la parte final del ciclo (Estadio Vc), se observa un salto debido a la expansion abdominal que ocurre justo antes de eclosionar. La manera en la que aumenta el volumen en los estadios anteriores se debe probablemente al la continua desaparicion del vitelio


