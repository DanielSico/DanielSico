Datos <- data.frame(X2007_EAA_Magud_et_al_data$Traits,X2007_EAA_Magud_et_al_data$A,X2007_EAA_Magud_et_al_data$P,X2007_EAA_Magud_et_al_data$W)
names(Datos)[1:4] <- c("Traits","body length","tubercles 1b apart","tarsus I length")
View(Datos)

tempdir()

Variancia <- aggregate(Datos[,2:4], list(Traits = Datos$Traits), var)
Desviacion <- aggregate(Datos[,2:4], list(Traits = Datos$Traits), sd)
Asimetria <- aggregate(Datos[,2:4], list(Traits = Datos$Traits), skewness)
Curtosis <- aggregate(Datos[,2:4], list(Traits = Datos$Traits), kurtosis)

par(mfrow = c(2, 2))
plot(density(Datos$`body length`[which(Datos$Traits == "I BK")]), col= "black", lwd= 3, main = "Longitud I BK", xlab = "longitud")
abline(v= mean(Datos$`body length`[which(Datos$Traits == "I BK")]), col= "Red", lwd= 3)
abline(v= median(Datos$`body length`[which(Datos$Traits == "I BK")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`body length`[which(Datos$Traits == "I BK")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
legend(x = "topright",
       c("Q1 y Q3", "Media", "Mediana"),
       col = c("purple", "red", "blue"),
       lwd = c(2, 2, 2))
plot(density(Datos$`body length`[which(Datos$Traits == "II BK")]), col= "black", lwd= 3, main = "Longitud II BK", xlab = "longitud")
abline(v= mean(Datos$`body length`[which(Datos$Traits == "II BK")]), col= "Red", lwd= 3)
abline(v= median(Datos$`body length`[which(Datos$Traits == "II BK")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`body length`[which(Datos$Traits == "II BK")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
plot(density(Datos$`body length`[which(Datos$Traits == "I IV")]), col= "black", lwd= 3, main = "Longitud I IV", xlab = "longitud")
abline(v= mean(Datos$`body length`[which(Datos$Traits == "I IV")]), col= "Red", lwd= 3)
abline(v= median(Datos$`body length`[which(Datos$Traits == "I IV")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`body length`[which(Datos$Traits == "I IV")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
plot(density(Datos$`body length`[which(Datos$Traits == "II IV")]), col= "black", lwd= 3, main = "Longitud II IV", xlab = "longitud")
abline(v= mean(Datos$`body length`[which(Datos$Traits == "II IV")]), col= "Red", lwd= 3)
abline(v= median(Datos$`body length`[which(Datos$Traits == "II IV")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`body length`[which(Datos$Traits == "II IV")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)

plot(density(Datos$`tubercles 1b apart`[which(Datos$Traits == "I BK")]), col= "black", lwd= 3, main = "Tuberculos I BK", xlab = "distancia")
abline(v= mean(Datos$`tubercles 1b apart`[which(Datos$Traits == "I BK")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tubercles 1b apart`[which(Datos$Traits == "I BK")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tubercles 1b apart`[which(Datos$Traits == "I BK")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
legend(x = "topleft",
       c("Q1 y Q3", "Media", "Mediana"),
       col = c("purple", "red", "blue"),
       lwd = c(2, 2, 2))
plot(density(Datos$`tubercles 1b apart`[which(Datos$Traits == "II BK")]), col= "black", lwd= 3, main = "Tuberculos II BK", xlab = "distancia")
abline(v= mean(Datos$`tubercles 1b apart`[which(Datos$Traits == "II BK")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tubercles 1b apart`[which(Datos$Traits == "II BK")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tubercles 1b apart`[which(Datos$Traits == "II BK")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
plot(density(Datos$`tubercles 1b apart`[which(Datos$Traits == "I IV")]), col= "black", lwd= 3, main = "Tuberculos I IV", xlab = "distancia")
abline(v= mean(Datos$`tubercles 1b apart`[which(Datos$Traits == "I IV")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tubercles 1b apart`[which(Datos$Traits == "I IV")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tubercles 1b apart`[which(Datos$Traits == "I IV")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
plot(density(Datos$`tubercles 1b apart`[which(Datos$Traits == "II IV")]), col= "black", lwd= 3, main = "Tuberculos II IV", xlab = "distancia")
abline(v= mean(Datos$`tubercles 1b apart`[which(Datos$Traits == "II IV")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tubercles 1b apart`[which(Datos$Traits == "II IV")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tubercles 1b apart`[which(Datos$Traits == "II IV")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)

plot(density(Datos$`tarsus I length`[which(Datos$Traits == "I BK")]), col= "black", lwd= 3, main = "Tarsus I BK", xlab = "longitud")
abline(v= mean(Datos$`tarsus I length`[which(Datos$Traits == "I BK")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tarsus I length`[which(Datos$Traits == "I BK")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tarsus I length`[which(Datos$Traits == "I BK")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
legend(x = "topleft",
       c("Q1 y Q3", "Media", "Mediana"),
       col = c("purple", "red", "blue"),
       lwd = c(2, 2, 2))
plot(density(Datos$`tarsus I length`[which(Datos$Traits == "II BK")]), col= "black", lwd= 3, main = "Tarsus II BK", xlab = "longitud")
abline(v= mean(Datos$`tarsus I length`[which(Datos$Traits == "II BK")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tarsus I length`[which(Datos$Traits == "II BK")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tarsus I length`[which(Datos$Traits == "II BK")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
plot(density(Datos$`tarsus I length`[which(Datos$Traits == "I IV")]), col= "black", lwd= 3, main = "Tarsus I IV", xlab = "longitud")
abline(v= mean(Datos$`tarsus I length`[which(Datos$Traits == "I IV")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tarsus I length`[which(Datos$Traits == "I IV")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tarsus I length`[which(Datos$Traits == "I IV")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
plot(density(Datos$`tarsus I length`[which(Datos$Traits == "II IV")]), col= "black", lwd= 3, main = "Tarsus II IV", xlab = "longitud")
abline(v= mean(Datos$`tarsus I length`[which(Datos$Traits == "II IV")]), col= "Red", lwd= 3)
abline(v= median(Datos$`tarsus I length`[which(Datos$Traits == "II IV")]), col= "Blue", lwd= 3)
abline(v= quantile(Datos$`tarsus I length`[which(Datos$Traits == "II IV")], seq(0.25, 0.75, 0.5)), col= "purple", lwd= 3)
