#=================================================================
# INFERENCIA ESTADÍSTICA  - GRADO DE MATEMÁTICAS - ULL
# PRÁCTICA DE LABORATORIO 04 - ANOVA y estadística no paramétrica

#=================================================================

#========================================
#========================================
# ANOVA
#========================================
#========================================


#========================================
# Diseño de 1 factor
#========================================

#load("madera.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/madera.RData?raw=true"))

str(madera)
attach(madera)

mod1F<-lm(desgaste~marca)               #ajusta un modelo lineal

anova1F<-anova(object=mod1F)            # obtiene la tabla ANOVA del modelo ajustado
print(anova1F) 
med1F<-tapply(desgaste, marca, mean)    # medias de la var. respuesta para cada nivel del factor"
print(med1F)
sd1F<-tapply(desgaste, marca, sd)       # desviaciones tipicas de la var. respuesta para cada nivel del factor"
print(sd1F)
# gráfico de las medias de la var. respuesta en cada nivel del factor
plot(med1F,type='o',pch=19, 
     col="red",ylim=c(1.8,3.0),
     xlab="marca del producto",ylab="media del desgaste",
     main="MEDIAS",cex.main=1.0)

bartlett.test(desgaste~marca)          # contraste de homogeneidad de varianzas
boxplot(desgaste~marca)                # comparar la varianza de desgaste frente a cada marca

library(ggplot2)
madera %>% ggplot(aes(sample = desgaste)) +
  geom_qq() + geom_qq_line() +
  facet_wrap(~marca, scales = "free_y") # comparar la normalidad de la muestra para cada marca


aov1F<-aov(desgaste ~ marca,  data=madera)                    #modelo lineal para comparaciones múltiples 
library(stats)
medtu<-TukeyHSD(aov1F)                                        # test de comparaciones de Tukey HSD
medtu
plot(medtu)
pairwise.t.test(desgaste,marca,p.adjust.method="bonferroni")  # ajuste de Bonferroni


#################################
## EJERCICIO PARA PRACTICAR
# 
# Una empresa de materiales de construcción quiere estudiar la influencia 
# que tienen el grosor y el tipo de templado sobre la resistencia máxima 
# de unas láminas de acero. Para ello miden el estrés hasta la rotura (variable cuantitativa dependiente)
# para dos tipos de templado (lento y rápido) y tres grosores de lámina (8mm, 16mm y 24 mm).

#load("acero.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/acero.RData?raw=true"))


# Realizar el análisis de varianza de un factor para constrastar si hay diferencias 
# entre el nivel medio de rotura para los tres grosores de lámina (8mm, 16mm y 24 mm)
acero<-acero[order(acero$grosor),c("grosor","resistencia")]
.....

#################################



#========================================
#Diseño de 1 factor + 1 bloque
#========================================

#load("frutas.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/frutas.RData?raw=true"))


str(frutas)
attach(frutas)

mod1F1B<-lm(formula(pesofruta~riego+bloque),data=frutas)
anova1F1B<-anova(object=mod1F1B)
print(anova1F1B)  # Para obtener la tabla ANOVA completa

bartlett.test(pesofruta~riego)
boxplot(pesofruta~riego,data=frutas, main="Peso de la fruta por riego",
        xlab="riego", ylab="Peso de la fruta")

medtu<-TukeyHSD(aov(pesofruta~riego+bloque),conf.level=0.95)
print(medtu)
plot(medtu)


pairwise.t.test(pesofruta, riego,p.adjust.method="bonferroni")
pairwise.t.test(pesofruta, riego,p.adjust.method="holm")        #mejor que Bonferroni

med1f<-tapply(pesofruta, INDEX=riego, FUN=mean)
plot(med1f,pch=19,type='o',col="red",xlab="riego",ylim=c(200,350),ylab="media de pesofruta",main="MEDIAS",cex.main=1.0)

med1b<-tapply(pesofruta, INDEX=bloque, FUN=mean)
plot(med1b,pch=19,type='o',col="green",xlab="bloque",ylim=c(50,500),ylab="media de pesofruta",main="MEDIAS",cex.main=1.0)


#========================================
#Diseño de 2 factores con interacción
#========================================

#load("semillas.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/semillas.RData?raw=true"))

str(semillas)
attach(semillas)

mod2F<-lm(formula(rendimiento~metodo*variedad),data=semillas)
anova2F<-anova(object=mod2F)
print(anova2F)  # Para obtener la tabla ANOVA completa
tapply(rendimiento, INDEX=list(metodo,variedad), FUN=mean) # medias de la respuesta por cada tratamiento
interaction.plot(metodo, variedad, rendimiento, fun = mean,
                 type = c("l", "p", "b", "o","c"),
                 xlab = "metodo",  ylab = "rendimiento medio",
                 main="gráfico de  interacción",
                 col = c(1:5),leg.bty = "o")
interaction.plot( variedad,metodo, rendimiento, fun = mean,
                  type = c("l", "p", "b", "o", "c"),
                  xlab = "variedad",  ylab = "rendimiento medio",
                  main="gráfico de  interacción",
                  col = c(1:3),leg.bty = "o")

medtu<-TukeyHSD(aov(rendimiento~metodo+variedad+metodo:variedad))
print(medtu)
plot(medtu)

pairwise.t.test(rendimiento,metodo:variedad,p.adjust.method="bonferroni")




#========================================
#========================================
# Estadística no paramétrica
#========================================
#========================================

#load("HIPER200.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/HIPER200.RData?raw=true"))

attach(HIPER200)
library(DescTools)


#======================================
# (A) BONDAD DE AJUSTE: TEST Y GRÁFICOS
#======================================

library(MASS)
library(fitdistrplus)
library(nortest)

fitdist(TAsist0,distr="norm")  #estimadores de máxima verosimilitud

pearson.test(TAsist0, n.classes=8)  # test de pearson para la normalidad
ks.test(TAsist0,"pnorm", mean=145,sd=24)  #test de Kolmogorov-Smirnov de 1 o 2 muestras
ks.test(TAsist0,"pnorm")  # test de normalidad de K-S con parametros por defecto (0,1)
lillie.test(TAsist0)   # test de Lilliefors para la normalidad
shapiro.test(TAsist0)  # test de normalidad de Shapiro-Wilk

hist(TAsist0)

#Funcion de distribución empírica y la teórica
plot.ecdf(x=TAsist0, verticals=FALSE , do.points=FALSE ,
          main="Curva de distribución empírica", lwd=2, ylab="valores de F",xlab="TASist0", 
          panel.first=grid(col="gray",lty="dotted"))
curve(pnorm(x,145,24), add=T, col='red', lty=1, lwd=3)


#Grafico Q-Q
qqnorm(TAsist0,pch=20)
qqline(TAsist0,col="red")

#################################
## EJERCICIO PARA PRACTICAR
# 
#Varios ejemplos con diferentes distribuciones
x<-rchisq(100,5)
#Realizar un histograma y un QQ plot de x


x<-runif(100)
#Realizar un histograma y un QQ plot de x

#################################

# comparación de muestras
TAsist0_m<-subset(TAsist0, genero=="masculino")
TAsist0_f<-subset(TAsist0, genero=="femenino")
ks.test(TAsist0_m,TAsist0_f)  #test de Kolmogorov-Smirnov  2 muestras



#=========================================================
# (B) TABLAS DE CONTINGENCIA: INDEPENDENCIA Y HOMOGENEIDAD
#=========================================================

# test de independencia
tab1<-table(genero,sal)  #variables genero y Sal
tab1

library(vcd)
res1<-chisq.test(tab1,correct=FALSE)
res1
res1$observed
res1$expected
assocstats(tab1) #proporciona G2 y CHI2 con la libreria "vcd"


grupo_edad<-ifelse(edad<=40,1,2)  #recodificación de la edad en dos clases (grupo_edad)
ed<-factor(grupo_edad, labels=c("<=40",">40"))
tab1<-table(grupo_edad,conc_hta)   #variables grupo_edad y conc_hta
tab1
chisq.test(tab1, correct=FALSE)

#################################
## EJERCICIO PARA PRACTICAR
# 
tab1<-table(cafe,act_fisi)  #variables Cafe y Actividad Fisica
print(tab1)
chisq.test(...)


#reagrupando clases
grupo_cafe<-Recode(cafe, 
                   "no toma" = "no toma", 
                   "poco"= "poco", 
                   "mucho"=c("moderado","mucho"))
tab1<-table(...)  #variables Cafe y Actividad Fisica

#################################


#otro ejemplo: En un estudio a 97 individuos, se analiza la incidencia de cáncer de pulmón y
# nivel de stress. Se tienen los siguientes datos:
#
#                 cáncer de pulmón
#                    Sí      No
# ---------------|----------------
#  Nivel de |Alto|   38      12
#   stress  |Bajo|   10      37
# ---------------|----------------

tabla_datos <- matrix( c(38,12,10,37), nrow= 2, byrow = TRUE)
colnames(tabla_datos) <- factor(c("sí","no"))
rownames(tabla_datos) <- factor(c("alto","bajo"))
chisq.test(tabla_datos, correct=FALSE)





# test de homogeneidad
table(genero,sal)  #variables genero y Sal
x<-c(41,53,0)   # datos de genero="femenino" y Sal
n<-c(65,117,18)
prop.test(x,n, conf.level=0.95,correct=FALSE)


#=====================
# TEST NO PARAMÉTRICOS
#=====================

#Test de una muestra
library(BSDA)
median(TAsist0)

SIGN.test(TAsist0, md=150)
wilcox.test(TAsist0, mu=150, paired=FALSE, correct=FALSE, conf.int=TRUE)

#test de dos muestras dependientes
SIGN.test(TAsist1,TAsist0, md=0)
binom.test(x=11,n=177,p=0.5)
wilcox.test(TAsist1,TAsist0, paired=TRUE, correct=FALSE)

#test de dos muestras independientes
wilcox.test(TAsist0~genero, paired=FALSE, correct=FALSE, conf.int=TRUE)

#test de varias muestras independientes
kruskal.test(TAsist1~sal)