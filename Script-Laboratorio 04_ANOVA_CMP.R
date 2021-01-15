#=================================================================
# INFERENCIA ESTADÍSTICA  - GRADO DE MATEMÁTICAS - ULL
# PRÁCTICA DE LABORATORIO 04 - ANOVA y estadística no paramétrica

###
### ANOVA_CMP Comparaciones a posteriori en un diseño con un factor
###
#=================================================================

#=============================================================
# Programa para realizar los contrastes a posteriori más conocidos
# trás la realización de un Anova One-way significativo.
#=============================================================

# Analisis de un conjunto de datos mediante un anova de un factor
#load("madera.RData")
load(url("https://github.com/ULL-STAT/R_practicas_inferencia/blob/master/madera.RData?raw=true"))

str(madera)
attach(madera)
mod1F<-lm(formula(desgaste~marca))  #ajusta un modelo lineal
anova1F<-anova(object=mod1F)        # obtiene la tabla ANOVA del modelo ajustado
anova1F


####################################################################
# Punto de partida: una tabla anova de 1 factor 
#
print(anova1F)
mse<-as.numeric(anova1F[2,3])
k<-as.numeric(anova1F[1,1])+1
gl<-as.numeric(anova1F[2,1])


# asignacion de factores y variable respuesta (IMPORTANTE)
#
F1<-madera$marca
Y<-madera$desgaste
#

nn<-as.vector(table(F1))     # número de observaciones para cada nivel del factor
print(nn)

med<-tapply(Y, F1, mean)     # medias de la respuesta para cada nivel del factor
med<-matrix(med,nrow=k, ncol=1)


###################################################################
# Punto de partida: una tabla anova de 1 factor y 1  bloque 
# 
print(anova1F1B)
mse<-as.numeric(anova1F1B[3,3])
k<-as.numeric(anova1F1B[1,1])+1   #si es el FACTOR
b<-as.numeric(anova1F1B[2,1])+1   #si es el BLOQUE
gl<-as.numeric(anova1F1B[3,1])

# asignacion de factor (O DE BLOQUE) y variable respuesta (IMPORTANTE)
# 
F1<-...
Y<-...
# 

nn<-as.vector(table(F1))         # número de observaciones para cada nivel del factor/bloque
print(nn)

med<-tapply(Y, F1, mean)         # medias de la respuesta para cada nivel del factor/bloque
med<-matrix(med,nrow=k, ncol=1)  #si es el FACTOR
med<-matrix(med,nrow=b, ncol=1)  #si es el BLOQUE






#========================================================================
#Intervalos de confianza individuales post-hoc en ANOVA 1F balanceado
alfa<-0.05                             # elegir el valor de alfa

####################################################################
# Intervalos de confianza simultáneos pareados según SCHEFFE   


f<-qf(1-alfa,k-1,gl)    # punto crítico de F Snedecor
p<-k*(k-1)/2
L<-matrix(0,nrow=p,ncol=8)

s<-c("SIG")
s<-rep(s, each=p)

a0<-0
for (i in 1:(k-1))
{
  for (j in (i+1):k)
    {
    a0<-a0+1
    L[a0,1]<-i
    L[a0,2]<-j
    L[a0,3]<-med[i,1]
    L[a0,4]<-med[j,1]
    L[a0,5]<-med[i,1]-med[j,1]
    L[a0,6]<-sqrt(mse*(k-1)*f*(1/nn[i]+1/nn[j]))
    L[a0,7]<-L[a0,5]-L[a0,6]
    L[a0,8]<-L[a0,5]+L[a0,6]
    if(abs(L[a0,5])<=L[a0,6]) s[a0]="NO Sig" else s[a0]="SIG"
  }
}
c1<-data.frame(L,s)
names(c1)<-c("i","j","med(i)","med(j)","dif_med","Ampl","LI_Scheffe","LS_Scheffe","RES")
cat("\nINTERVALOS DE CONFIANZA PAREADOS SEGÜN SCHEFFE:\n\n")
print(c1,digits=5)


###################################################################
# Intervalos de confianza simultáneos pareados según TUKEY-KRAMER

q<-qtukey(1-alfa,k,gl)    # punto crítico de Q Range Studentized
p<-k*(k-1)/2
L<-matrix(0,nrow=p,ncol=8)

s<-c("SIG")
s<-rep(s, each=p)

a0<-0
for (i in 1:(k-1))
{
  for (j in (i+1):k)
  {
    a0<-a0+1
    L[a0,1]<-i
    L[a0,2]<-j
    L[a0,3]<-med[i,1]
    L[a0,4]<-med[j,1]
    L[a0,5]<-med[i,1]-med[j,1]
    L[a0,6]<-q*sqrt(mse*0.5*(1/nn[i]+1/nn[j]))
    L[a0,7]<-L[a0,5]-L[a0,6]
    L[a0,8]<-L[a0,5]+L[a0,6]
    if(abs(L[a0,5])<=L[a0,6]) s[a0]="NO Sig" else s[a0]="SIG"
  }
}
c1<-data.frame(L,s)
names(c1)<-c("i","j","med(i)","med(j)","dif_med","Ampl","LI_Tukey","LS_Tukey","RES")
cat("\nINTERVALOS DE CONFIANZA PAREADOS SEGÚN TUKEY:\n\n")
print(c1,digits=5)

###################################################################
# Intervalos de confianza simultáneos pareados según BONFERRONI


eps_b <- (2*alfa)/(k*(k-1))         #epsilon de Bonferroni
tb<-abs(qt(eps_b/2,gl))             # Punto crítico T Student con ajuste de Bonferroni
p<-k*(k-1)/2
L<-matrix(0,nrow=p,ncol=8)

s<-c("SIG")
s<-rep(s, each=p)

a0<-0
for (i in 1:(k-1))
{
  for (j in (i+1):k)
  {
    a0<-a0+1
    L[a0,1]<-i
    L[a0,2]<-j
    L[a0,3]<-med[i,1]
    L[a0,4]<-med[j,1]
    L[a0,5]<-med[i,1]-med[j,1]
    L[a0,6]<-tb*sqrt(mse*(1/nn[i]+1/nn[j]))
    L[a0,7]<-L[a0,5]-L[a0,6]
    L[a0,8]<-L[a0,5]+L[a0,6]
    if(abs(L[a0,5])<=L[a0,6]) s[a0]="NO Sig" else s[a0]="SIG"
  }
}
c1<-data.frame(L,s)
names(c1)<-c("i","j","med(i)","med(j)","dif_med","Ampl","LI_Bonf","LS_Bonf","RES")
cat("\nINTERVALOS DE CONFIANZA PAREADOS SEGÚN BONFERRONI:\n\n")
print(c1,digits=5)

###################################################################
# Intervalos de confianza simultáneos pareados según SIDACK


eps_s <- 1-(1-alfa)**(2/(k*(k-1)))            #epsilon de Sidak
ts<-abs(qt(eps_s/2,gl))     # Punto crítico T Student con ajuste de Sidak
p<-k*(k-1)/2
L<-matrix(0,nrow=p,ncol=8)

s<-c("SIG")
s<-rep(s, each=p)

a0<-0
for (i in 1:(k-1))
{
  for (j in (i+1):k)
  {
    a0<-a0+1
    L[a0,1]<-i
    L[a0,2]<-j
    L[a0,3]<-med[i,1]
    L[a0,4]<-med[j,1]
    L[a0,5]<-med[i,1]-med[j,1]
    L[a0,6]<-ts*sqrt(mse*(1/nn[i]+1/nn[j]))
    L[a0,7]<-L[a0,5]-L[a0,6]
    L[a0,8]<-L[a0,5]+L[a0,6]
    if(abs(L[a0,5])<=L[a0,6]) s[a0]="NO Sig" else s[a0]="SIG"
  }
}
c1<-data.frame(L,s)
names(c1)<-c("i","j","med(i)","med(j)","dif_med","Ampl","LI_Sidack","LS_Sidack","RES")
cat("\nINTERVALOS DE CONFIANZA PAREADOS SEGÚN SIDACK:\n\n")
print(c1,digits=5)

###################################################################
# Intervalos de confianza simultáneos pareados según FISHER (LDS)


tf<-abs(qt(alfa/2,gl))     # Punto crítico T Student 
p<-k*(k-1)/2
L<-matrix(0,nrow=p,ncol=8)

s<-c("SIG")
s<-rep(s, each=p)

a0<-0
for (i in 1:(k-1))
{
  for (j in (i+1):k)
  {
    a0<-a0+1
    L[a0,1]<-i
    L[a0,2]<-j
    L[a0,3]<-med[i,1]
    L[a0,4]<-med[j,1]
    L[a0,5]<-med[i,1]-med[j,1]
    L[a0,6]<-tf*sqrt(mse*(1/nn[i]+1/nn[j]))
    L[a0,7]<-L[a0,5]-L[a0,6]
    L[a0,8]<-L[a0,5]+L[a0,6]
    if(abs(L[a0,5])<=L[a0,6]) s[a0]="NO Sig" else s[a0]="SIG"
  }
}
c1<-data.frame(L,s)
names(c1)<-c("i","j","med(i)","med(j)","dif_med","Ampl","LI_Fisher","LS_Fisher","RES")
cat("\nINTERVALOS DE CONFIANZA PAREADOS SEGÚN FISHER:\n\n")
print(c1,digits=5)

###################################################################################
# Intervalos de confianza para comparaciones ordenadas según Ryan, Einot, Gabriel


medo<-matrix(0,nrow=k, ncol=1)
med1<-sort(med)
for (i in 1:k)
{
  medo[i,1]<-med1[i]
}
p<-k*(k-1)/2
L<-matrix(0,nrow=p,ncol=6)


s<-c("SIG")
s<-rep(s, each=p)

b2<-matrix(0,nrow=k, ncol=1)
c2<-matrix(0,nrow=k, ncol=1)
for (i in 2:k)
{
  b2[i]<-1-((1-alfa)**(i/k))
  c2[i]<-qtukey(1-b2[i],i,gl)
} 

a0<-0
for (i in 1:(k-1))
{
  for (j in 1:i)
  {
    a0<-a0+1  
    L[a0,1]<-j
    L[a0,2]<-k-i+j
    L[a0,3]<-medo[j,1]
    L[a0,4]<-medo[k-i+j,1]
    L[a0,5]<-L[a0,4]-L[a0,3]
    L[a0,6]<-c2[k-i+1]*sqrt(mse/nn[1]) 
    if(abs(L[a0,5])<=L[a0,6]) s[a0]="NO Sig" else s[a0]="SIG"
  }
}         
c1<-data.frame(L,s)
names(c1)<-c("i","j","med(i)","med(j)","dif_med","Ampl","RES")
cat("\nINTERVALOS DE CONFIANZA PAREADOS SEGÚN RYAN:\n\n")
print(c1,digits=5)

#################################################################################
# Intervalos de confianza para comparaciones con el mejor HSU (máximo o mínimo)


memax<-matrix(0,nrow=k, ncol=1)
memin<-matrix(0,nrow=k, ncol=1)
for (i in 1:k)
{
  memax[i]<-max(med[-i])
  memin[i]<-min(med[-i])
}

q<-qtukey(1-alfa,k,gl)    # punto crítico de Q Range Studentized 
p<-2*k
L<-matrix(0,nrow=p,ncol=8)

s<-c("SIG")
s<-rep(s, each=p)

for (i in 1:k)
{
    L[i,1]<-i
    L[i,2]<-j
    L[i,3]<-med[i,1]
    L[i,4]<-memax[i,1]
    L[i,5]<-med[i,1]-memax[i,1]
    L[i,6]<-q*sqrt(mse*0.5*(1/nn[i]+1/nn[j]))
    L[i,7]<-L[i,5]-L[i,6]
    L[i,8]<-L[i,5]+L[i,6]
    if(abs(L[i,5])<=L[i,6]) s[i]="NO Sig" else s[i]="SIG"
    L[k+i,1]<-i
    L[k+i,2]<-j
    L[k+i,3]<-med[i,1]
    L[k+i,4]<-memin[i,1]
    L[k+i,5]<-med[i,1]-memin[i,1]
    L[k+i,6]<-q*sqrt(mse*0.5*(1/nn[i]+1/nn[j]))
    L[k+i,7]<-L[k+i,5]-L[k+i,6]
    L[k+i,8]<-L[k+i,5]+L[k+i,6]
    if(abs(L[k+i,5])<=L[k+i,6]) s[k+i]="NO Sig" else s[k+i]="SIG"
}
c1<-data.frame(L,s)
names(c1)<-c("i","j","med(i)","med(j)","dif_med","Ampl","LI_Hsu","LS_Hsu","RES")
cat("\nINTERVALOS DE CONFIANZA PAREADOS SEGÚN HSU:\n\n")
print(c1,digits=5)




#=====================
# FINAL DE LA PRÁCTICA
#=====================


