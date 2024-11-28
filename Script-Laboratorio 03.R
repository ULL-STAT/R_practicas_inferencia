#=================================================================
# INFERENCIA ESTADÍSTICA  - GRADO DE MATEMÁTICAS - ULL
# PRÁCTICA DE LABORATORIO 03 - Test de hipótesis y tamaño muestral

#=================================================================



#==========================================================================
# (A) test de hipotesis sobre una media para una m.a.s. de una dist. normal
#==========================================================================
#load("HIPER200.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/HIPER200.RData?raw=true"))

# si queremos evitar tener que escribir el nombre del data.frame, podemos
# hacer attach(HIPER200) y a partir de ese momento ya podemos hacer mención directa
# a las variables
attach(HIPER200)

# cargamos la siguiente librería
library(HH)


# test bilateral de la media al 5% significación de media de Tasist0 (H0: mu=148.7)
t1<-t.test(TAsist0,alternative="two.sided", mu=148.7, conf.level = 0.95)
t1

print(NTplot(t1), tablesOnPlot=FALSE)
#(1-pt(abs(mean(TAsist0)-148.7)/(sd(TAsist0)/sqrt(200)),199))

# test unilateral izqda. de la media al 5% significación de media de Tasist0 (H0: mu>=148.7)
t2<-t.test(TAsist0,alternative="less", mu=148.7, conf.level = 0.95)
t2

#################################
## EJERCICIO PARA PRACTICAR
# Test al 1% de significación si la media de tensión siastólica final (TAsist1) es mayor de 131: 
t3<-t.test(... ,alternative="...", mu=..., conf.level = ...)
t3

# Test al 1% de significación si la media de tensión siastólica final (TAsist1) 
# de los hipertensos (es_hip=="si) es mayor de 131: 
tasist1_hiper<-subset(..., es_hip=="...")
t4<-t.test(...,alternative="greater", mu=..., conf.level = ...)
t4

#################################



#===========================================================================================
# (B) test de hipotesis sobre la igualdad de dos medias para dos m.a.s.independientes de sendas dist. normales
#===========================================================================================

table(act_fisi)                #contar frecuencias de cada categoría

# queremos comparar al 5% de significación si el peso medio de los individuos que 
# hacen actividad física escasa
# es diferente del de los individuos que hacen actividad física moderada

HIP_em <- subset(HIPER200, subset=(act_fisi=="escasa" | act_fisi=="moderada"), select=c(peso,act_fisi) )
table(HIP_em$act_fisi)
HIP_em$act_fisi<-factor(HIP_em$act_fisi)
table(HIP_em$act_fisi)

t5<-t.test(HIP_em$peso~HIP_em$act_fisi,alternative="two.sided",var.equal=TRUE)
t5

t6<-t.test(HIP_em$peso~HIP_em$act_fisi,alternative="two.sided",var.equal=FALSE)
t6


var.test(HIP_em$peso~HIP_em$act_fisi,alternative="two.sided")



#################################
## EJERCICIO PARA PRACTICAR
# Test al 5% de significación para comparar si el peso medio
# de los indviduos que toman poca sal (sal=="normal") es menor que el peso medio
# de los indviduos que toman una cantidad normal de sal (sal=="mucha")

HIP_sal <- subset(HIPER200, subset=(...), select=c(peso,sal) )
HIP_sal$sal<-factor(...)

var.test(HIP_sal$...~HIP_sal$...,alternative="two.sided")
t7<-t.test(HIP_sal$...~HIP_sal$...,alternative="less", var.equal=TRUE)
t7
#################################



#=============================================================================================
# (C) test de hipotesis sobre la igualdad de dos medias para dos m.a.s.dependientes de sendas dist. normales
#=============================================================================================

# queremos comparar estudiar los individuos de >25 años y comparar si la tensión diastólica inicial media
# es diferente de la tensión diastólica final media
HIPER200_25 <- subset(HIPER200, subset=edad > 25, select=c(TAdias1,TAdias0))
t6<-t.test(HIPER200_25$TAdias0,HIPER200_25$TAdias1,alternative="two.sided",paired=TRUE)




#==========================================================================
# (D) tes de hipótesis de medias en el supuesto de varianza(s) CONOCIDA(s)

# Usar la libreria BSDA
# este programa permite realizar los tests de medias de 1 y 2 muestras al caso de ser 
# conocidas las varianzas de las distribuciones de partida (z-test).
#==========================================================================

library(BSDA)
# comparar si la media poblacional de la variable TAsist0 es igual a 148.7 suponiendo que sigma=24
z1<-z.test(TAsist0,alternative="two.sided", mu=148.7, sigma.x=24)

# contrastar si la media de TAsist0 es menor a 148.7 suponiendo sigma=24. 
z.test(TAsist0,alternative="less",mu=148.7, sigma.x=24)
# contrastar si la media de TAsist0 es mayor a 148.7 suponiendo sigma=24. 
z.test(TAsist0,alternative="greater", mu=148.7, sigma.x=24)

# contrastar si la media de TAsist0 de los hombres >25 es distinta de la de las mujeres de >25 suponiendo
# que la sigma de cada grupo (tanto hombres como mujeres) es =13.
TAsist0_m <- subset(TAsist0, subset=edad > 25 & genero=="masculino")
TAsist0_f <- subset(TAsist0, subset=edad > 25 & genero=="femenino")
z.test(TAsist0_m,TAsist0_f,alternative="two.sided",mu=0, sigma.x=13, sigma.y=13)



#==================================================================================
# (E) Para el caso de 1 o 2 proporciones se proponen los contrastes
#==================================================================================
# prop.test(x, n, p = NULL,
#          alternative = c("two.sided", "less", "greater"),
#          conf.level = 0.95, correct = TRUE)
#==================================================================================

# queremos comparar si la proporción de hipertensos en la población es del 20%
table(es_hip)
prop.test(36, 200, p=0.20, alternative="two.sided",conf.level = 0.95, correct = FALSE)  # una proporción
binom.test(36, 200, p=0.20, alternative="two.sided",conf.level = 0.95) # test exacto

# queremos comparar si la proporción de hipertensos en los hombres es la misma que en mujeres
table(es_hip, genero)
prop.test(x = c(12, 24), n = c(106, 94), correct = FALSE)   # dos proporciones




#################################
## EJERCICIO PARA PRACTICAR
# Test al 5% de significación para comparar si, entre las personas que toman una 
# cantidad normal de sal (sal=="normal"), la proporción 
# de hombres es mayor que la de mujeres 

table(..., ...)
prop.test(x = c(..., ...), n = c(..., ...), alternative="greater", correct = FALSE)   # dos proporciones


#################################



#############################################
#  BLOQUE DE TRABAJO AUTÓNOMO PARA EL ALUMNO (OPCIONAL)
#############################################


#===========================================================================================
# (F) CÁLCULOS DE POTENCIA Y TAMAÑO MUESTRAL PARA EL TEST DE UNA MEDIA CON VARIANZA CONOCIDA
# DE UNA DISTRIBUCIÓN NORMAL
#===========================================================================================

library(pwr) 
# Considerar el contraste H0: mu=mu0=150 vs H1: mu≠mu0=150, cuando sigma=20 conocida.
# ¿Cuál ha de ser el tamaño muestral necesario para detectar 
# un punto mu1 de la alternativa a 5 unidades de la hipótesis nula, para 
# un nivel de significación alfa=0.05 y una potencia B(mu1)=0.90 (o beta=0.10)
sigma<-24.0
mu0<-150
mu1<-155
d1<-(mu1-mu0)/sigma

# Tamaño muestral del test para una potencia de 0.90  (OPCIÓn I)
pwr.norm.test(d=d1,n=NULL,sig.level=0.05,alternative="two.sided", power=0.9)  

# Tamaño muestral del test para una potencia de 0.90  (OPCIÓn II)
library(TrialSize)
OneSampleMean.Equality(alpha=0.05, beta=0.1, sigma=20, margin=5)


## Función de potencia del citado test
mu<-seq(130,170,l=100)
d<-(mu-mu0)/sigma
plot(d,pwr.norm.test(d=d,n=200,sig.level=0.05,alternative="two.sided")$power,
     type="l",ylab="función de potencia",ylim=c(0,1))
abline(h=0.05)
abline(h=0.90)

## Potencia del citado test frente al tamaño de la muestra
n1<-seq(10,500,l=100)
plot(n1,pwr.norm.test(d=0.2,n=n1,sig.level=0.05,alternative="two.sided")$power,
     type="l",ylab="función de potencia",xlab="tamaño muestral",ylim=c(0,1))
abline(h=0.90)



# Tamaño muestral para otros supuestos (para test de proporciones, 
# detectar una diferencia de medias, o detectar dif. de proporciones)
k1<-OneSampleProportion.Equality(alpha=0.05, beta=0.1,  p=0.35, differ=0.2)
k1
k2<-TwoSampleMean.Equality(alpha=0.05, beta=0.1, sigma=20, k=1, margin=5)
k2
k3<-TwoSampleProportion.Equality(alpha=0.05, beta=0.1,  p1=0.35, p2=0.45, k=1.2)
k3





#==========================================================================
# (G) RESOLUCIÓN DE CONTRASTES UTILIZANDO ESTIMADORES
# Si se dispone de los estimadores muestrales y no de los datos podemos utilizar
# zsum.test (ZTEST) y tsum.test (TTEST) como se explica a continuación 
# (por ejemplo, para resolver los contrastes de las hojas de problemas)
#==========================================================================


# (G1) FUNCIÓN ZSUM.TEST


#==================================================================================
# zsum.test(mean.x, sigma.x = NULL, n.x = NULL, mean.y = NULL,
#          sigma.y = NULL, n.y = NULL, alternative = "two.sided", mu = 0,
#          conf.level = 0.95)
#==================================================================================

library(BSDA)
library(DescTools) 
Desc(TAsist0)
# contrastar si la media de TAsist0 es igual a 148.7 suponiendo que sigma=24 (nivel de significación=0.05)
zsum.test(mean.x=144.805, sigma.x = 24, n.x = 200,
          alternative = "two.sided", mu = 148.7, conf.level = 0.95)


#################################
## EJERCICIO PARA PRACTICAR

#Desc(TAsist1)
# contrastar si la media de TAsist1 es igual a 130 suponiendo que sigma=20 (nivel de significación=0.05)
zsum.test(mean.x=..., sigma.x = ..., n.x = ..., 
          alternative = "two.sided", mu = 130, conf.level = 0.95)
#################################

# contrastar si la media de TAdias0 de hombres es distinta de la de las mujeres suponiendo que la sigma 
# de cada grupo (tanto hombres como mujeres) es =12 (nivel de significación=0.05).
#by( TAdias0 , genero,  length )
#by( TAdias0 , genero,  mean )
#by( TAdias0 , genero,  sd)
zsum.test(mean.x=79.82075, sigma.x = 12, n.x = 106,
          mean.y=82.1383,  sigma.y = 12, n.y = 94,
          alternative = "two.sided", mu = 0, conf.level = 0.95)



# (G2) FUNCIÓN TSUM.TEST

#==================================================================================
# tsum.test(mean.x, s.x = NULL, n.x = NULL, mean.y = NULL, s.y = NULL,
#          n.y = NULL, alternative = "two.sided", mu = 0, var.equal = FALSE,
#          conf.level = 0.95)
#==================================================================================

#Desc(TAsist1)
# contrastar si la media de TAsist1 es igual a 130 suponiendo que sigma desconocida (nivel de significación=0.05)
tsum.test(mean.x=132.64, s.x = 22.27, n.x = 200, 
          alternative = "two.sided", mu = 130, conf.level = 0.95)

#################################
## EJERCICIO PARA PRACTICAR

#Desc(TAsist1)
# contrastar si la media de TAsist1 es igual a 130 (nivel de significación=0.05)
tsum.test(mean.x=..., s.x = ..., n.x = ..., 
          alternative = "...", mu = ..., conf.level = 0.95)
#################################

# contrastar si la media de TAdias0 de hombres es distinta de la de las mujeres suponiendo la sigma 
# de cada grupo (tanto hombres como mujeres) son iguales y desconocidas (nivel de significación=0.05).
#by( TAdias0 , genero,  length )
#by( TAdias0 , genero,  mean )
#by( TAdias0 , genero,  sd)
tsum.test(mean.x=79.82075, s.x = 12.43941, n.x = 106,
          mean.y=82.1383,  s.y = 12.52748, n.y = 94,
          alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)





#=======================
# FINAL DE LA PRÁCTICA
#=======================

