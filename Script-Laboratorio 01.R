#=================================================================
# INFERENCIA ESTADÍSTICA  - GRADO DE MATEMÁTICAS - ULL
# PRÁCTICA DE LABORATORIO 01 - Variables aleatorias y estimación puntual
# 
#=================================================================


getwd()                      # muestra el directorio de trabajo


#================================
# (A) DISTRIBUCIONES DE PROBABILIDAD
#================================

# Hallar la probabilidad de que una variable X de Poisson con lambda=3.5 tome el valor 0
dpois(x=0,lambda=0.5)
dpois(0,0.5)

#################################
## EJERCICIO PARA PRACTICAR
#¿Cuál es la probabilidad que X=5? 
dpois(..., ...)

#¿Y de que X sea par y menor o igual a 20?
x<-1:20
sum(dpois(x[....],...))
#################################


# Para hallar la probabilidad acumulada de que una variable X de Poisson con lambda=3.5 tome valor como máximo 6
ppois (6, 3.5)
# Para hallar una probabilidad acumulada inversa, es decir, dada una probabilidad de 0.2 en qué valor
# de la variable X de Poisson con lambda=3.5 se alcanza dicha probabilidad acumulada
qpois (0.2, 3.5)

#################################
## EJERCICIO PARA PRACTICAR
#¿Cuál es la probabilidad de que X valga más de 2? 
1-ppois (..., 0.35)
#¿Y de que X sea distinto de 3?

#¿Y que X valga entre 3 y 7, ambos incluidos?

#¿En qué valor de X se tiene una probabilidad acumulada de 0.999?

#################################


# Hallar la probabilidad acumulada de que una variable X binomial(n=10,p=0.65) 
# tome valor como máximo 6
pbinom (8, 10, 0.65)

#################################
## EJERCICIO PARA PRACTICAR
#¿De qué otra forma se puede hallar esta probabilidad utilizando 
# la función de densidad de probabilidad, es decir, con dbinom?
1-( dbinom ( )+ dbinom ( )) 
#################################

#Supongamos que queremos hallar la probabilidad de que una variable 
#X normal con mu=15 y sigma=5 valga menos de 13 
pnorm(13,mean=15,sd=5)
pnorm (13, 15,5)

#################################
## EJERCICIO PARA PRACTICAR
#¿Cuál es la probabilidad de que X valga más de 16?
1-pnorm(...,15,5)
#¿Y de que X esté entre 10 y 20?
pnorm(...,...,...)-pnorm(...,...,...)
# ¿Para qué valor de X se tiene una probabilidad acumulada de 0.2, , es decir, Pr(X<x)=0.2? 
qnorm(...,15,5)
# ¿Y para qué valor se tiene que la probabilidad acumulada por encima del 
# mismo sea 0.2, es decir, Pr(X>x)=0.2?
qnorm(...,...,...,lower.tail = FALSE)
#¿Cómo se hallaría el valor anterior considerando la probabilidad que hay por debajo
# del mismo, es decir, Pr(X<x)=0.8?

#Hallar Pr(|X-mu|<sigma), Pr(|X-mu|<2*sigma) y Pr(|X-mu|<3*sigma)


#################################





#================================
# (B) GENERAR NÚMEROS ALEATORIOS
#================================

n <- 1000
x <- runif(n)
hist(x, freq=FALSE, col='light blue', main='Distribución Uniforme', ylab="Densidad")
curve(dunif(x,min=0,max=1.0), add=T, col='red', lty=1, lwd=3)

set.seed(149653)
n <- 750
x <- runif(n)
x1<-round(26712*x, digits=0)
x2<-sort(x1)
z<-cbind(x,x1,x2)
print(z)
#save(z, file="L:/RTRABAJO/IE2017/uniforme.Rdata")

# Para realizar un muestreo aleatorio sobre una lista de items con o sin reemplazamiento

y<-sample(x1, 50, replace = FALSE, prob = NULL)
print(sort(y))

#===============================================
# (C) GENERAR VALORES DE UNA DISTRIBUCIÓN NORMAL
#===============================================

# Generar 1000 números aleatorios de una normal N(0,1)
set.seed(149653)
n <- 1000
x <- rnorm(n)
hist(x, freq=FALSE,  ylim = c(0, 0.4), col='light blue', ylab='f. de densidad - f(x)',main='Distribución Normal',cex.main=1.0)
curve(dnorm(x), add=T, col='red', lty=1, lwd=3)
box()

# Comprobación del ajuste de los datos generados mediante un gráfico Q-Q plot
qqplot(qnorm(ppoints(n)),x, pch=20, main = "Q-Q plot", ylab="datos observados", xlab="datos teóricos")
qqline(x, distribution = qnorm,   prob = c(0.25, 0.75), col = 2,lwd=3, qtype=9)  #col= color de la línea, 0.25 y 0.75 son por defecto

# En el caso de querer comprobar si los datos se ajustan a la distribución normal entonces
# qqnorm(x) 
# qqline(x)

# Comprobación del ajuste de los datos generados mediante un gráfico P-P plot
z3<-pnorm(sort(x))
plot(z3,(1:n)/n ,type = 'p', ylim = c(0, 1), pch=20, xlab = 'probabilidad teórica', 
     ylab = 'probabilidad empírica', main = 'P-P plot')
abline(0,1, col = 2, lwd=3)

# otra forma del gráfico P-P
plot(z3,ppoints(n), type = 'p', ylim = c(0, 1), pch=20, xlab = 'probabilidad teórica', 
     ylab = 'probabilidad empírica', main = 'P-P plot')
abline(0,1, col = 2, lwd=3)

# Comprobación mediante las funciones de distribución teórica y empírica
set.seed(149653)
n <- 100
x <- rnorm(n)
plot.ecdf(x, verticals=FALSE , do.p=FALSE ,
          main="Función de distribución empírica", lwd=2, ylab="valores de F",sub="N(0,1)", 
          panel.first=grid(col="gray",lty="dotted"))
curve(pnorm(x), add=T, col='red', lty=1, lwd=3)


# Comprobar propiedades mediante simulación
#Si Xi ~ Normal(mu,sigma^2) entonces X1+ ...+ Xn ~  Normal(n*mu,n*sigma^2) 
#Comprobar para mu=10, sigma^2=100, con n=50
set.seed(149653)
simul<-replicate(10000, sum(rnorm(50,10,2))  )
mean(simul)    # debería ser aprox. igual a 50*10
var(simul)     # debería ser aprox. igual a 50*2^2




# Generar datos de otra distribución y su posterior comprobación requiere los mismos 
# procedimientos que hemos seguido para la distribucion normal

#################################
## EJERCICIO PARA PRACTICAR
#Si Xi ~ Exp(λ) entonces T=X1+ ...+ Xn ~  Gamma(alfa=n, beta=λ), con E[T] =n/λ y Var[T] =n/λ^2
#Comprobar para λ=0.3 con n=50
simul<-replicate(..., ..... )
mean(simul)  
var(simul)   
#################################


#=============================================================
# (D) ESTIMACIÓN PUNTUAL: ESTIMADORES DE MÁXIMA VEROSIMILITUD
#=============================================================

# Caso de una distribución Normal
set.seed(1496)
x<-rnorm(100,5,2)
print(x)
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad: f(x)',main='Distribución Normal',cex.main=1.0)
curve(dnorm(x,5,2), add=T, col='red', lty=1, lwd=3)

# MLE de mu y sigma
library(fitdistrplus)
fitdist(x, distr = "norm")
mean(x)
sd(x)


# Caso de una distribución Exponencial
set.seed(1496)
x<-rexp(100,0.5)
print(x)
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad: f(x)',main='Distribución Exponencial',cex.main=1.0)
curve(dexp(x,0.5), add=T, col='red', lty=1, lwd=3)

# MLE de lambda
fitdist(x, distr = "exp")
1/mean(x)


# Caso de una distribución Gamma
set.seed(1496)
x<-rgamma(100,4,0.5)
print(x)
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad: f(x)',main='Distribución Gamma',cex.main=1.0)
curve(dgamma(x,4,0.5), add=T, col='red', lty=1, lwd=3)

# MLE de alfa y beta
fitdist(x, distr = "gamma")


# Caso de una distribución Geométrica
set.seed(1496)
x<-rgeom(100,0.3)
print(x)
fitdist(x,distr="geom")


# Caso de una distribución Binomial
set.seed(1496)
x<-rbinom(100,12,0.3)
print(x)
fitdist(x, discrete = T, distr = "binom", fix.arg=list(size=12), start=list(prob=0.2))


#################################
## EJERCICIO PARA PRACTICAR
# Cargar el dataset HIPER200.RData
load("HIPER200.RData")
head(...)

# Hallar MLE de la variable peso (asumiendo normalidad) y comparar con media y desv. típica
HIPER200$...
fitdist(..., distr = "...")  
mean(...)
sd(...)

# ¿Se puede considerar que la variable peso sigue una distribución normal?
# Representar en un histograma la variable peso
# y representar una curva de densidad normal especificando como media y desv. típica 
# los valores de mean() y sd()
hist(...., freq=FALSE,  col='light blue', 
     ylab='f. de densidad:  f(x)',main='Distribución Normal',cex.main=1.0)
curve(dnorm(x,...,....), add=T, col='red', lty=1, lwd=3)

# Realizar un qqplot y qqline para la variable peso
qqnorm(...) 
qqline(...)


# Si filtramos sólo las personas hipertensas (es_hip="si), 
# ¿qué distribución tiene la variable peso?
# NOTA: el comando subset(vble, filter) sirve para filtrar la variable "vble" de acuerdo
# al filtro "filter"
pesoHiper<-subset(...,....=="si")
# si queremos evitar tener que escribir el nombre del data.frame, podemos
# hacer attach(HIPER200) y a partir de ese momento ya podemos hacer mención directa
# a las variables
attach(HIPER200)
pesoHiper<-subset(...,es_hip=="si")

# Representar en un histograma y qqplot la variable pesoHiper
hist(..., freq=FALSE,  col='light blue', 
     ylab='f. de densidad:  f(x)',main='Distribución Normal',cex.main=1.0)
curve(dnorm(x,70.36,11.17), add=T, col='red', lty=1, lwd=3)

qqnorm(...) 
qqline(...)
#################################




#=======================
# FINAL DE LA PRÁCTICA
#=======================


