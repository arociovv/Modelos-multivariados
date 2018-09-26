###Instalación de paquetes:
install.packages("vcd")
install.packages("effects")
install.packages("visreg")
install.packages("arules")

###Carga de las librerías:

library(vcd)
library(effects)
library(visreg)
library(arules)

###Cargar base de datos Artritis:

data("Arthritis")

###Adjuntar base de datos

attach(Arthritis)

###Ver la base de datos Artritis (print) y verificar número de casos (dim):

print(Arthritis)
dim(Arthritis)

###EJERCICIO 2. Con la misma base de datos anterior realice lo siguiente:

###2a.Discretice la edad de los individuos en intervalos de longitud 17

discretize(Age, method = "interval", length = 17)
Age_disc <- discretize(Age, method = "interval", length = 17)

###Confirmamos tabla con grupos de edad en intervalos de 17 años

table(Age_disc)

###Generamos tabla cruzada entre mejoras de pacientes y edades discretizadas.

table(Improved, Age_disc)

###2b.¿Existe asociación entre esta edad discretizada y los niveles de mejora de estos pacientes con artritis? Justifique su respuesta.
###La prueba de hipótesis para estas dos variables sería la siguiente:
###H0: Las variables condición de mejora de los pacientes con artritis y edad discretizada no están asociadas o su grado de asociación es nulo (son independientes)
###H1: Las variables condición de mejora de los pacientes con artritis y edad discretizada están asociadas (no son independientes)
###Primero realizamos la prueba de ji-cuadrada.
 
chisq.test(Age_disc)$expected

###El progrma R no nos permite hacer la prueba de ji-cuadrada de independencia de variables porque entre nuestros valores esperados hay una celda con un cero.
###Al hacer los cálculos en Excel observamos que los valores esperados y los observados son muy distintos, salvo en los casos en que la mejoría es nula.
###Aplicamos la prueba Ji cuadrada

chisq.test(table(Age_disc,Improved))

###Después de realizar lo anterior, el programa R nos muestra un mensaje de advertencia, ya que más del 20% de nuestros valores esperados son inferiores a 5, por ello, hacemos la prueba de Fisher

fisher.test(table(Age_disc,Improved)) 

###Esta prueba nos arroja un p-value de 0.006812 que es menor al valor del nivel de significancia con el que estamos trabajando (0.05). Esto nos indica que el p-valor es signficativo y por lo tanto, concluímos que las variables condición de mejora de los pacientes
###con artritis y edad discrtizada SÍ ESTÁN ASOCIADAS; es decir, no son independientes..

###2c.¿Existe una asociación creciente entre esta edad discretizada y los niveles de mejora de estos pacientes con artritis? Justifique su respuesta. 
###Aunque una de las maneras de obtener en el programa R la fuerza de asociación entre variables categóricas es mediante el comando: assocstats()
###en este caso no lo podemos hacer con lo siguiente, ya que nuestras variables no son nominales y nuestra tabla no es del tamaño 2x2 para aplicar los coeficientes phi o V de Cramer.

x<-table(Improved, Age_disc)
x
assocstats(x)

### Debido a que estamos trabajando con variables ordinales (condición de mejora de los pacientes con artritis -ninguna, alguna y marcada- e intervalos de la variable edad, que van desde los más jóvenes hasta los mayores,
### aplicaremos la Prueba Gamma, la cual que nos ayuda a evidenciar la fuerza y la dirección de la asociación entre nuestras variables.

GKgamma(table(Age_disc,Improved))

###El valor de Gamma estimada=0.392, lo que, considerando las "reglas de dedo" que conocemos: 
###?[0.0-0.30],fuerza de asociación débil;
###?(0.30-0.60],fuerza de asociación moderada;
###?(0.60,1], fuerza de asociación fuerte;
###nos indica que la fuerza de asociación entre las variables estudiadas es MODERADA.
###Cabe señalar que la dirección de la asociación entre las variables estudiadas es positiva.
###Es decir, a mayor edad mayor nivel de mejora. 
###Finalmente, hay que mencionar que cuando queremos evaluar la relación entre dos variables ordinales o una variable ordinal y una de intervalo,
###el coeficiente Tau de Kendall es el más apropiado (Moral,2006:190)

###EJERCICIO 3: La librería vcdExtra contiene la base Mental, una tabla de contingencia de 6x4 que representa la clasificación cruzada del estado de salud mental (mental) de 1,660 jóvenes residentes de Nueva York y el estado socioeconómico de sus padres (ses). Con esta base realice lo siguiente:
###Cargando la librería:

library(vcdExtra)

###Visualizamos los DF o BD del paquete "vcdExtra" que descargamos.

data(package="vcdExtra")

####Abrimos la BD "Mental". Solicitamos al programa ver los datos con el siguiente comando:

data(Mental)

###Revisamos la estructura de los datos del data frame "Mental" mediante el siguiente comando:

str(Mental) #Observamos 24 obserservaciones para 3 variables.

###3a.¿Existe una asociación entre el estado de salud mental de estos jóvenes y el estado socioeconómico de sus padres? Justifique su respuesta.
###3b. ¿Existe una asociación creciente entre este estado de salud mental y el estado socioeconómico de los padres? Justifique su respuesta.
###3c. Para la tabla anterior, construya las tablas de probabilidades empíricas, marginales y condicionales, e interprete estas probabilidades.


