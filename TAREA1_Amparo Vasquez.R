#IMPORTANTE: Para hacer los gráficos de asociación necesitamos tener las siguientes librerías en R:

library(tm)
library(gglot2)
library(gnm)
library(grid)
  



#TAREA 1. ASOCIACIONES.

# EJERCICIO 1: Trabajar con la BD Artritis (Arthritis) que está disponible en el paquete vcd de R. Para esto es necesario descargar dicho paquete haciendo lo siguiente:
#1.En la ventana principal de R ir a la pestaña Paquetes/ Seleccionar espejo CRAN/ Dar click en un país, con excepción de Argentina
#2.Seleccionar nuevamente Paquetes/Instalar paquetes/Escoger de la lista el paquete "vcd"
#3. Esperar a que R descargue el paquete y escribir lo siguiente: library(vcd)
# NOTA IMPORTANTE: OTRA MANERA DE LLAMAR A LA LIBRERÍA CON LA QUE NOS INTERESA TRABAJAR ES ESCRIBIENDO EL SIGUIENTE COMANDO: install.packages(c("nombre del paquete que nos interesa"))  o install. packages("nombre del paquete que nos interesa") OJO: Si quiero quitar un paquete lo hago mediante el siguiente comando: remove.packages("nombre del paquete")

library(vcd)

# Con el siguiente comando veremos las BD o data frames que están incluidos en el paquete "vcd" que descargamos.

data(package="vcd")

#como nos interesa trabajar con la BD "Arthritis", pedimos ver sus datos con el siguiente comando:

data(Arthritis) #OJO: DICE QUE NO ENCUENTRA LOS DATOS!!

#Vemos la estructura de los datos del data frame "Arthritis" mediante el siguiente comando:

str(Arthritis)

#Veremos los primeros datos del data frame para identificar las variables con las que trabajaremos

head(Arthritis)


#APUNTE DE LA CLASE:
#Definimos el objeto al que le vamos a aplicar una prueba de Chi cuadrada
bb<-chisq.test(Bebedor)

bb

bb$expected

bb$residuals

bb$stdres


### Gráficamente

mosaic(Bebedor,color=TRUE,shade=T,main="mosaico")

mosaicplot(SS,color=TRUE,shade=T,main="mosaico")

assoc(Bebedor,color=TRUE,shade=T,main="mosaico")

### Medidas de asociación

RR<-function(X,alpha){
  
  RR<-(X[1]/(X[1]+X[3]))/(X[2]/(X[2]+X[4]))
  
  sd<-sqrt(1/X[1]-1/(X[1]+X[3])+1/X[2]-1/(X[2]+X[4]))
  
  intervalo<-exp(log(RR)+c(-1,1)*qnorm(1-alpha/2)*sd)
  
  return(list(RiesgoRelativo=RR,Confianza=(1-alpha)*100,IC=intervalo))
  
}

CocienteMomios<-function(X,alpha){
  
  O<-X[1]*X[4]/(X[2]*X[3])
  sd<-sqrt(1/X[1]+1/X[2]+1/X[3]+1/X[4])
  
  intervalo<-exp(log(O)+c(-1,1)*qnorm(1-alpha/2)*sd)
  
  return(list(CocienteMomios=O,confianza=(1-alpha)*100,IntervaloConfianza=intervalo))
  
}

RR(Bebedor,0.05)

CocienteMomios(Bebedor,0.05)


### Es estadísticamente significativo?. Es decir, es estadísticamente distinto de UNO o su logaritmo es estadísticamente distinto de cero???

X<-Bebedor

Z<-(log(1.336773))/sqrt(1/X[1]-1/(X[1]+X[3])+1/X[2]-1/(X[2]+X[4]))

2*(1-pnorm(Z))

1-pchisq(Z^2,1)



Z<-(log(1.435681))/sqrt(1/X[1]+1/X[2]+1/X[3]+1/X[4])

2*(1-pnorm(Z))

1-pchisq(Z^2,1)

fisher.test(Bebedor, or = 1, alt = "two.sided", conf.int = F)

### Medidas de fuerza de asociación


assocstats(Bebedor)

#EJERCICIO 2: Con la BD de Arthritis realice lo siguiente:
#1. Discretice la edad de los individuos en intervalos de longitud 17

#2.¿Existe una asociación entre esta edad discretizada y los niveles de mejora de estos pacientes con artritis?  Justifique su respuesta.

#3.¿Existe una asociación creciente entre esta edad discretizada y los niveles de mejora de estos pacientes con artritis? Justifique su respuesta.

#4. Para la tabla anterior, construya las tablas de probabilidades empíricas, marginales y condicionales e interprete estas probabilidades.


#EJERCICIO 3: La librería vcdExtra contiene la base Mental, una tabla de contingencia de 6x4 que representa la clasificación cruzada del estado de salud mental (mental) de 1,660 jóvenes residentes de Nueva York y el estado socioeconómico de sus padres (ses). Con esta base realice lo siguiente:
#1. ¿Existe una asociación entre el estado de salud mental de estos jóvenes y el estado socioeconómico de sus padres? Justifique su respuesta.
#2. ¿Existe una asociación creciente entre este estado de salud mental y el estado socioeconómico de los padres? Justifique su respuesta.
#3. Para la tabla anterior, construya las tablas de probabilidades empíricas, marginales y condicionales, e interprete estas probabilidades.

#PASO 1:

library(vcdExtra)

#Para ver DF o BD incluidas en el paquete "vcdExtra" que descargamos.

data(package="vcdExtra")

#Nos interesa trabajar con la BD "Mental". Solicitamos al programa ver los datos con el siguiente comando:

data(Mental)

#Vemos la estructura de los datos del data frame "Mental" mediante el siguiente comando:

str(Mental) #Observamos 24 obserservaciones para 3 variables.





#EJERCICIO 4.La base de datos Arrests.csv contiene la información sobre datos de individuos arrestados en Toronto por posesión de pequeñas cantidades de marihuana. Contiene 5,226 observaciones con las siguientes variables:
#released (liberado): Si el individuo arrestado fue liberado o no con un citatorio. Factor con niveles: No, Yes.
#colour (color): La raza del arrestado. Factor con niveles: Black, White.
#year (año): año de arresto, con valores desde 1997 hasta 2002. Vector numérico.
#age (edad): Edad del arrestado en años. Vector numérico.
#sex (sexo): Sexo del arrestado. Factor con niveles: Female, Male.
#employed (empleado): Factor con niveles: No, Yes.
#citizen (ciudadano): Si el arrestado era o no ciudadano canadiense. Factor con niveles: No, Yes.
#checks (fichado): Número de bases de datos policiales (de arresos previos, condenas anteriores, estado de libertad condicional, etc. 6 en total) en las que apareció el nombre del arrestado. Vector numérico.
#Con esta base de datos realice lo siguiente:

#1. Realizando una pequeña investigación sobre el tipo de fenómeno subyacente a estos datos, proponga el mejor modelo de regresión logística, con variable dependiente: released que considere que refleja de mejor manera la relación entre la respuesta y las variables predictoras.
#Su modelo debe contener, además de todos los términos que usted considere apropiados, las interacciones entre años y color, además de la de edad y color.
#2. Interprete todos los parámetros estimados del modelo. Especialmente las interacciones mencionadas arriba.
#3. Juzgue la bondad de ajuste de su modelo propuesto. Si algún (os) individuo(s) no está(n) bien ajustado(s), intente explicar a qué se debe esta falta de ajsute.

#PASO 1. IMPORTAR DATOS DESDE EXCEL.
#a) Seguir la siguiente sintaxis para importar los datos a R: 
#nombre del objeto que vamos a crear<-read.table("Copiar la ubicación de nuestro archivo a importar desde las "Propiedades" del mismo, colocar todas las diagonales hacia la derecha (/) y escribir después de la última diagonal el nombre del archivo y la extensión que le corresponde: ejemplo: 070918_Arrests.csv, terminar con las ",header=T)
#b) Correr el comando para poder empezar a trabajar con la BD.

arrestados<-read.table("C:/Users/Rocio/Documents/00_MDS_2018_2020/01 MDS 2019_1/02_MODELOS MULTIVARIADOS/03_TAREAS/070918_Arrests.csv",header=T)

####APUNTE DE LA CLASE###
#VAMOS A HACER UN MODELO PARTIENDO DE UNA TABLA, NO DE LA BD
#TENEMOS TABLAS YU PODEMOS CORRER MODELOS CON ELLAS, NO TENEMOS SUJETOS PORQUE ESTÁN AGRUPADOS PERO NOS SIRVE
#DECLARAR LA TABLA COMO SI FUERA UNA BD
SS1<-as.data.frame(SS)
SS1
#CORREREMOS UN MODELO DE REGRESION LOGISTICA PARTIENDO DE LA TABLA

fit1<-glm(survived~sex,weights=Freq,data=SS1,family=binomial)

summary(fit1)

#LECTURA DE RESULTADOS:

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)   1.1124     0.1176   9.455   <2e-16 ***
  sexmale      -2.4667     0.1522 -16.208   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1414.6  on 3  degrees of freedom
Residual deviance: 1102.0  on 2  degrees of freedom
AIC: 1106

> exp(-2.4667)
[1] 0.08486445
> 1/exo(-2.4997)
Error in exo(-2.4997) : no se pudo encontrar la función "exo"
> 1/exp(-2.4667)
[1] 11.7835
> 
  
  #COMPARAMOS EL MOMIO DE SUPERVIVENCIA DE HOMBRES Y MUJERES
  #el momio de supervivencia de las mujeres es casi 12 veces mas de que el de un hombre muera
  #VEREMOS PROBABILIDADES, LAS RECTAS SON ESO
  
  plot(allEffects(fit1))
allEffects(fit1)

#r no nos da probabilidades que suman uno o suman mas, si quiero hacerlas relativas las tengo exhaustivas
#mostramos riesgo relativo 
#Estas son probabildiades de supervivencia
0.7525773/0.2057672
[1] 3.657421
#La probabilidad de que un hombre muriera era 3.668117 por cada mujer
#forma de hacerlo a mano
##Probabilidades. Pr(sobrevivir|mujer)=exp(1.1124)/(1+exp(1.1124))=0.7525763; 
## Pr(sobrevivir|hombre)=exp(1.1124-2.4667)/(1+exp(1.1124-2.4667))=0.2051683. Riesgo relativo: 0.7525763/0.2051683=3.668092.

#supervivencia por clase
SC<-table(survived,pclass)

#que nos muestre la clase
sc

#grafica
barplot(SC, main="Supervivencia por clase",col=c("darkblue","darkred"),legend=rownames(SC),beside=TRUE)

#hacemos el de correlaciones

mosaicplot(SC,color=TRUE,shade=T,main="mosaico")

#hacemos asociacioens
assoc(SC,color=TRUE,shade=T,main="mosaico")

#vemos de qué tamano son las diferencias a través de un modelo
SC1<-as.data.frame(SC)

fit2<-glm(survived~pclass,weights=Freq,data=SC1,family=binomial)

#resultados
Call:  glm(formula = survived ~ pclass, family = binomial, data = SC1, 
           weights = Freq)

Coefficients:
  (Intercept)    pclass2nd    pclass3rd  
0.5638      -0.8024      -1.6021  

Degrees of Freedom: 5 Total (i.e. Null);  3 Residual
Null Deviance:      1415 
Residual Deviance: 1306         AIC: 1312
> 
  
  #la primera clase es la comparacion para todas
  #todos los estimadores se comparan en relación a la clase 1
  #encontramos cocientes de momios
  #tomamos el coeficiente de clase 2, 1/exp(-0.8024) y luego hacemos 1/exp(-1.6021)
  #el momio de supervivencia de la clase 1 es 2.23 veces el momio de la clase 2
  #el momio de supervivencia de la clase 1 es 4.96 veces más el momio de la clase 3
  #OJO: HAY QUE SACAR LAS PROBABILIDADES
  
  plot(allEffects(fit2))

#para ver los valores que está graficando effec hay que tomar la misma instruccion y quitar plot

(allEffects(fit2)
  
  #RIESGOS RELATIVOS TOMANDO COMO REFERENCIA LA CLASE 1
  
  0.6373239/0.4406130
  [1] 1.446448
  > 0.6373239/0.2614771
  [1] 2.437399
  
  #PODEMOS CALCULAR MOMIOS DE COMPARACION CLASE 2 ENTRE CLASE 3 CUANDO TENEMOS COMO CATEGORIA BASAL 1
  #SÍ PODEMOS HACERLO PORQUE SE ELIMINA
  #LA CLASE 1 TUVO MAYOR SUPERVIVENCIA
  
  visreg(fit1,"sex",xlab="sexo",ylab="Log momio de supervivencia")
  
  visreg(fit1,"sex",xlab="sexo",scale="response",ylab="Probabilidad de supervivencia")
  