#=================================================================
# INFERENCIA ESTADÍSTICA  - GRADO DE MATEMÁTICAS - ULL
# PRÁCTICA DE LABORATORIO 01 - Números aleatorios, simulación de 
#                         variables aleatorias y estimación puntual
# Prof.:Miguel A. Glez. Sierra      Curso:2018/19
#=================================================================


getwd()                      # muestra el directorio de trabajo
setwd("D:/RTRABAJO/IE")     # establece el directorio de trabajo
#================================
# (A) GENERAR NÚMEROS ALEATORIOS
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
# (B) GENERAR VALORES DE UNA DISTRIBUCIÓN NORMAL
#===============================================

set.seed(149653)
n <- 1000
x <- rnorm(n)
hist(x, freq=FALSE,  ylim = c(0, 0.4), col='light blue', ylab='f. de densidad - f(x)',main='Distribución Normal',cex.main=1.0)
curve(dnorm(x), add=T, col='red', lty=1, lwd=3)
box()
# Comprobación de los datos generados mediante los gráficos P-P plot y Q-Q plot

qqplot(qnorm(ppoints(n)),x, pch=20, main = "Q-Q plot", ylab="datos observados", xlab="datos teóricos")
qqline(x, distribution = qnorm,   prob = c(0.25, 0.75), col = 2,lwd=3, qtype=9)  #col= color de la línea, 0.25 y 0.75 son por defecto

# qqnorm(x) solo para la distribución normal
# qqline(x)

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


# Generar datos de otra distribución y su posterior comprobación requiere los mismos 
# procedimientos que hemos seguido para la distribucion normal

#=============================================================
# (C) ESTIMACIÓN PUNTUAL: ESTIMADORES DE MÁXIMA VEROSIMILITUD
#=============================================================

# Caso de una distribución Normal
set.seed(1496)
x<-rnorm(100,5,2)
print(x)
#no va a funcionar el gráfico, indicar el motivo
hist(x)
curve(dnorm(x,5,2), add=T, col='red', lty=1, lwd=3)
#si va a funcionar
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad - f(x)',main='Distribución Normal',cex.main=1.0)
curve(dnorm(x,5,2), add=T, col='red', lty=1, lwd=3)
library(MASS)
fitdistr(x,"normal")

mean(x)
sd(x)
mle(minuslogl, start = formals(minuslogl), method = "BFGS",
    fixed = list(), nobs, ...)
# Caso de una distribución Exponencial
set.seed(149653)
x<-rexp(100,0.5)
print(x)
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad - f(x)',main='Distribución Exponencial',cex.main=1.0)
curve(dexp(x,0.5), add=T, col='red', lty=1, lwd=3)
fitdistr(x,"exponential")

mean(x)
sd(x)

# Caso de una distribución Gamma
set.seed(1496)
x<-rgamma(100,4,0.5)
print(x)
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad - f(x)',main='Distribución Gamma',cex.main=1.0)
curve(dgamma(x,4,0.5), add=T, col='red', lty=1, lwd=3)
fitdistr(x,"gamma")

mean(x)
sd(x)

# Caso de una distribución Geométrica
set.seed(149653)
x<-rgeom(100,0.3)
print(x)
fitdistr(x,"geometric")

mean(x)
sd(x)

# Caso de una distribución Binomial
set.seed(1496)
x<-rbinom(100,12,0.3)
print(x)
library(bbmle)
mtmp <- function(size,prob) 
{-sum(dbinom(x,size,prob,log=TRUE))}
(m0 <- mle2(mtmp,start=list(prob=0.8),data=list(size=12)))
summary(m0)
sum(x)/(100*12)

mean(x)
sd(x)


#=======================
# FINAL DE LA PRÁCTICA
#=======================


