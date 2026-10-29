#=================================================================
# INFERENCIA ESTADÍSTICA  - GRADO DE MATEMÁTICAS - ULL
# PRÁCTICA DE LABORATORIO 02 - Intervalos de confianza
# 
#=================================================================



#=================================================================
# ESTIMACIÓN POR INTERVALOS: INTERVALOS DE CONFIANZA
#=================================================================
# Cargar el dataset HIPER200.RData
#load("HIPER200.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/HIPER200.RData?raw=true"))

# si queremos evitar tener que escribir el nombre del data.frame, podemos
# hacer attach(HIPER200) y a partir de ese momento ya podemos hacer mención directa
# a las variables
attach(HIPER200)

x<-cbind(peso,talla)
apply(x,2,mean)
apply(x,2,sd)

library(DescTools)

#intervalo de confianza al 95% para la media del PESO 
#con desviación típica (sigma) CONOCIDA
MeanCI(peso,sd=10.0,conf.level=0.95)  

#calculo directo del intervalo
#l1<-mean(peso)-qnorm(0.975)*10/sqrt(length(peso))
#l2<-mean(peso)+qnorm(0.975)*10/sqrt(length(peso))

# cotas de confianza al 95% de la media del PESO (sigma conocida)
MeanCI(peso,sd=10.0,conf.level=0.95, sides="left")
MeanCI(peso,sd=10.0,conf.level=0.95, sides="right")


#intervalo de confianza al 95% para la media del PESO 
#con desviación típica (sigma) DESCONOCIDA
MeanCI(peso,conf.level=0.95)   

#intervalo de confianza al 95% para la varianza del PESO
#con media (mu) DESCONOCIDA
VarCI(peso, conf.level=0.95)  

#################################
## EJERCICIO PARA PRACTICAR
#Intervalos de confianza al 99% de la media de la variable TALLA suponiendo: 
# (a) que sigma es 10 y (b) que sigma desconocida
MeanCI(..., ..., conf.level=...)
MeanCI(...,conf.level=...)
#Intervalos de confianza al 99% de la varianza de la variable TALLA mu desconocida
VarCI(..., conf.level=...)


#Intervalo de confianza al 97% de la media de la variable tensión arterial sistólica (TAsist1) 
#de los individuos hipertensos suponiendo que sigma es 20
tasist_hip <-subset(... ,... )
MeanCI(... ,sd=... , conf.level=... )
# Cota superior de confianza de dicha media al 97% 
MeanCI(... ,sd=... , conf.level=... ,side="...")

#Similar para los individuos NO hipertensos suponiendo que sigma es desconocida
tasist_nohip <-subset(... ,... )
MeanCI(... ,conf.level=...)

#Intervalo de confianza al 97% de la DESVIACIÓN TÍPICA de la variable tensión arterial sistólica (TAsist1) 
#de los individuos hipertensos y el IC similar para los individuos NO hipertensos 
VarCI(..., conf.level=... ) 
VarCI(..., conf.level=... ) 
#################################






#IC para la diferencia de medias del peso de hombres y mujeres con varianzas desconocidas y distintas
MeanDiffCI(peso~genero, conf.level=0.95)

#IC para la diferencia de medias con varianzas desconocidas y distintas (otra manera)
peso1<-subset(peso, genero=="masculino")
peso2<-subset(peso, genero=="femenino")
MeanDiffCI(peso1,peso2, conf.level=0.95) 





#################################
## EJERCICIO PARA PRACTICAR
#Intervalo de confianza al 99% de la diferencia de medias en la talla de los individuos NO hipertensos y 
#la de los individuos hipertensos (varianzas desconocidas y distintas)
talla_no_hip <-subset(..., ... )
talla_si_hip <-subset(... , ... )
MeanDiffCI(... ,... , conf.level=0.99) 
#################################


# IC para una proporción según Wilson
table(sal,genero)
margin.table(table(sal,genero),2)
BinomCI(x=18, n=106, conf.level=0.95, method="wilson") # el IC usual para una proporción
BinomCI(x=18, n=106, conf.level=0.95, method="wald")   # IC para una proporción según Wald


#################################
## EJERCICIO PARa PRACTICAR
#Intervalo de confianza al 99% de la proporción de los individuos bebedores (por Wilson y Wald )
table(...)
BinomCI(x=..., n=..., conf.level=0.99, method="...")  
BinomCI(x=..., n=..., conf.level=0.99, method="...") 
#################################



# IC para la diferencia de proporciones según Wald
table(sal,genero)
margin.table(table(sal,genero),2)
BinomDiffCI(x1=24, n1=106, x2=41, n2=94, method="wald")


#################################
## EJERCICIO PARA PRACTICAR
#Intervalo de confianza al 99% de la diferencia de proporciones 
#de los individuos bebedores hipertensos y 
#la de los individuos bebedores NO hipertensos 
table(..., ... )
margin.table(table(... ,... ),2)
BinomDiffCI(x1=..., n1=...  , x2=... , n2=... , method="wald")
#################################


# IC para una distribución de Poisson
PoissonCI(x=15, n=38,conf.level=0.98,method=c( "score", "wald"))




#============================================================================================
# INTERPRETACIÓN FRECUENTISTA DE UN INTERVALO DE CONFIANZA
# Simulación de 100 muestras de tamaño 25 de una distribución normal de media (mu) y
# desviación típica (sd) conocidas, y evaluación del cubrimiento del intervalo de confianza 
# para la media. (IC DE LA MEDIA DE UNA NORMAL CON VARIANZA CONOCIDA)
#
# a(N,n,mu,sd, conf) vector de parametros a pasar
#============================================================================================


a<-c(100,25,10,1.4,0.95)
set.seed(12345)
v <- matrix(0,ncol=a[1],nrow=2)
for (i in 1:a[1]) {
  x <- rnorm(a[2], a[3], a[4])
  l1<-mean(x)+qnorm((1-a[5])/2)*a[4]/sqrt(a[2])
  l2<-mean(x)-qnorm((1-a[5])/2)*a[4]/sqrt(a[2])
  v[1,i] <- l1
  v[2,i] <- l2
}
plot(apply(v,2,mean), ylim=c(min(v),max(v)),
     ylab='Intervalo de confianza', xlab='número de muestras')
abline(a[3],0)
c <- apply(v,2,min)>a[3] | apply(v,2,max)<a[3]
segments(1:a[1],v[1,],1:a[1],v[2,], col=c(par('fg'),'red')[c+1], lwd=2)
title(main="Simulación del cubrimiento de un intervalo de confianza")
print(100*sum(c)/a[1])  #porcentaje


# otra forma de simular, más fácil que antes
library(TeachingDemos)
set.seed(19485)
ci.examp(mean.sim = 10, sd = 1.4, n = 25, reps = 100, conf.level = 0.95)




### Ejercicio 05 (Intervalo de Wald)
library(DescTools)
waldInterval <- function(x, n, conf.level = 0.95){
  ci<-BinomCI (x=15, n=20, conf.level=0.95,method='wald') 
  return(ci[2:3])
}
#example
waldInterval(x = 20, n =40) #this will return 0.345 and 0.655



