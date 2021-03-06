#Ejer: �Que clase de objeto es BIC? �Qu� clase de objeto es geospiza? use la funci�n *str* para obtener informaci�n sobre la estructura.
str(BCI)
str(geospiza)

#Ejer: �Qu� tipo de variable es tarsusL y qu� clase?
#Abra el objeto *tortues* y mire �Qu� tipo de variables tiene y qu� clase son?, haga lo mismo con el objeto *mite.env* y revise �Qu� tipo de variable es *shurb* y qu� clase es? Recuerde siempre usar help para saber de donde vienen los datos.
class(geospiza$geospiza.data[,2])
str(geospiza$geospiza.data[,2])
str(tortues)
str(mite.env)
str(mite.env$Shrub)

#Ejer: usando el data.frame *tortues* calcule la proporci�n de hembras y machos de la muestra
length(which(tortues$sexe == "M"))/length(which(tortues$sexe == "F"))

#Ejer: usando el data.frame *tortues* calcule la proporci�n entre el ancho y el largo del caparaz�n, genere una tabla con la informaci�n de *tortues* y la nueva variable derivada.
tortugas <- data.frame(cbind(tortues, tortues$larg/tortues$long))
colnames(tortugas) <- c(colnames(tortues), "Tama�o")

#Ejer: �Cu�ntas especies de *Corvus* fueron muestreadas en la regi�n Pale�rtica-Africana? Use el data.frame del g�nero *Corvus*. �Qu� porcentaje de *Corvus* muestreados habitan zonas abiertas
table(corvus$phylog == "pale")
prop.table(table(corvus$habitat))*100

#Ejer: �C�mo luce el gr�fico de frecuencias para los tipos de h�bitat del g�nero *Corvus*?
plot(corvus$habitat)

#Ejer: encuentre el m�ximo, as� como lo hizo para el m�nimo
which.max(apply(atlas$birds, 2, sum))
apply(atlas$birds, 2, sum)[which.max(apply(atlas$birds, 2, sum))]

#Ejer: Repita el ejercicio para las variables continuas de tama�o de pico y tama�o del ala en el g�nero *Corvus*. Busque c�mo cambiar colores, ejes y algunas propiedades que le permitan mejorar el dise�o del histograma.
#Invente unos datos de medidas, como se hizo en el caso de los cr�neos, y genere todo el ejercicio hasta el histograma. Busque otro tipo de distribuciones para generar las variables [ejemplo][3]. Busque en qu� casos sirve cada distribuci�n o para qu� se usan.
transform(table(cut(corvus$bill, seq(40, 90, 5))), Relativ= prop.table(Freq), Acum= cumsum(Freq))
hist(corvus$bill, seq(45, 85, 5), main = 'Picos', xlab = "longitud")
transform(table(cut(corvus$wing, seq(225, 475, 25))), Relativ= prop.table(Freq), Acum= cumsum(Freq))
hist(corvus$wing, seq(225, 475, 25), main = 'Alas', xlab = "longitud")

