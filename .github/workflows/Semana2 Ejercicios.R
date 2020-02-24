#Ejer: Reasigne 5 a la letra *z*, usando el operador *<-*, ahora, pregunte ¿qué es *z*?
z <- 5
z

#Ejer: Revisemos qué es rnorm(1). Empecemos buscando la función en help o ?rnorm
?rnorm

#Ejer: multiplique *x* por 2 y sumele 1 y dividalo por 1.5, ese resultado elevelo al cuadrado. Esto se escribe igual que en una calculadora manual.
(((x*2)+1)/1.5)^2

#Ejer: tome cualquier objeto anterior y preguntele la clase
class(enteros)

#Ejer: ¿Qué clase tiene el vector?
class(prueba)
  
#Ejer: Cree un una secuencia que repita los números de 1 a 4, 3 veces y asignela a un objeto llamado vector1. Ahora, cree una secuencia de números pares del 2 al 24 y asignela a un objeto llamado vector2. Gnere un arreglo con ambos vectores y busque la columna 2, fila 1 de la segunda matriz y para la primera.
vector1 <- rep(1:4, 3)
vector2 <- seq(2,24,2)
arreglo <- array(c(vector1,vector2), dim = c(3,4,3))
arreglo[1,2,2]
arreglo[1,2,1]

#**Ejer:**  tome la primera columna  y la segunda, concatenelas con c() ¿Qué encuentra?
#Ahora, tome amabas columnas y use la funcion cbind(), asignelo a un objeto llamado minuevatabla.
#Haga lo mismo, con las filas del 1:5 y del 10:30, use primero c() y luego rbind() ¿Qué encuentra? 
c(iris$Sepal.Length, iris$Sepal.Width)
minuevatabla <- cbind(iris$Sepal.Length, iris$Sepal.Width)
c(iris[1:5,1:5],iris[10:30,1:5])
rbind(iris[1:5,1:5],iris[10:30,1:5])
