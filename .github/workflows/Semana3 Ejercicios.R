#Ejer: ¿Que clase de objeto es BIC? ¿Qué clase de objeto es geospiza? use la función *str* para obtener información sobre la estructura.
str(BCI)
str(geospiza)

#Ejer: ¿Qué tipo de variable es tarsusL y qué clase?
#Abra el objeto *tortues* y mire ¿Qué tipo de variables tiene y qué clase son?, haga lo mismo con el objeto *mite.env* y revise ¿Qué tipo de variable es *shurb* y qué clase es? Recuerde siempre usar help para saber de donde vienen los datos.
class(geospiza$geospiza.data[,2])
str(geospiza$geospiza.data[,2])
str(tortues)
str(mite.env)
str(mite.env$Shrub)

#Ejer: usando el data.frame *tortues* calcule la proporción de hembras y machos de la muestra
length(which(tortues$sexe == "M"))/length(which(tortues$sexe == "F"))

#Ejer: usando el data.frame *tortues* calcule la proporción entre el ancho y el largo del caparazón, genere una tabla con la información de *tortues* y la nueva variable derivada.
tortugas <- data.frame(cbind(tortues, tortues$larg/tortues$long))
colnames(tortugas) <- c(colnames(tortues), "Tamaño")

#Ejer: ¿Cuántas especies de *Corvus* fueron muestreadas en la región Paleártica-Africana? Use el data.frame del género *Corvus*. ¿Qué porcentaje de *Corvus* muestreados habitan zonas abiertas
table(corvus$phylog == "pale")
prop.table(table(corvus$habitat))*100

#Ejer: ¿Cómo luce el gráfico de frecuencias para los tipos de hábitat del género *Corvus*?
plot(corvus$habitat)

#Ejer: encuentre el máximo, así como lo hizo para el mínimo
which.max(apply(atlas$birds, 2, sum))
apply(atlas$birds, 2, sum)[which.max(apply(atlas$birds, 2, sum))]

#Ejer: Repita el ejercicio para las variables continuas de tamaño de pico y tamaño del ala en el género *Corvus*. Busque cómo cambiar colores, ejes y algunas propiedades que le permitan mejorar el diseño del histograma.
#Invente unos datos de medidas, como se hizo en el caso de los cráneos, y genere todo el ejercicio hasta el histograma. Busque otro tipo de distribuciones para generar las variables [ejemplo][3]. Busque en qué casos sirve cada distribución o para qué se usan.
transform(table(cut(corvus$bill, seq(40, 90, 5))), Relativ= prop.table(Freq), Acum= cumsum(Freq))
hist(corvus$bill, seq(45, 85, 5), main = 'Picos', xlab = "longitud")
transform(table(cut(corvus$wing, seq(225, 475, 25))), Relativ= prop.table(Freq), Acum= cumsum(Freq))
hist(corvus$wing, seq(225, 475, 25), main = 'Alas', xlab = "longitud")

