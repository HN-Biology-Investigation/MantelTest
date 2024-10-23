## Como realizar la rarefaccion##
## seleccionar direccion de trabajo##

setwd("C:/Users/David Murillo/R/Investigacion_DAI")
library(vegan)

## Llamar tablas##

DAI<-read.csv("C:/Users/David Murillo/R/Investigacion_DAI/DAI.csv")
SOL<-read.csv("C:/Users/David Murillo/R/Investigacion_DAI/SOL.csv")
SOMBRA<-read.csv("C:/Users/David Murillo/R/Investigacion_DAI/SOMBRA.csv")
BOSQUE<-read.csv("C:/Users/David Murillo/R/Investigacion_DAI/BOSQUE.csv")

## Grafico##

dai<-specaccum(DAI,"rarefaction")
sol<-specaccum(SOL,"rarefaction")
bosque<-specaccum(BOSQUE,"rarefaction")
sombra<-specaccum(SOMBRA,"rarefaction")
plot(dai, xlab = "No de Muestreos", ylab = "No de Especies", main= "Curva de Acumulacion de Especies")
plot(sol, add=TRUE,col="red")
plot(bosque, add=TRUE,col="blue")
plot(sombra, add=TRUE,col="yellow")
library(lme4)
library(ggplot2)
library(Hmisc)
library(plotrix)
library(gmodels) #planned contrasts
library(car) #type III ANOVA
setwd("C:/Users/David Murillo/R/Tesis/Datos/")
getwd()

Abundance_sp<-read.csv("C:/Users/David Murillo/R/Tesis/Datos/Abundacia_System_sp.csv")
VERCHR<-(Abundance_sp[35,2:5])
VERCYA<-(Abundance_sp[15,2:5])
HYLMUS<-(Abundance_sp[78,2:5])
CARPUS<-(Abundance_sp[4,1:4])
boxplot(CARPUS, main="Abundancia Cardellina pusilla",xlab="Sistema",ylab="Abundacia")
boxplot(HYLMUS, main="Abundancia Hylocichla mustelina",xlab="Sistema",ylab="Abundacia")
boxplot(VERCHR, main="Abundancia Vermivora chrysoptera",xlab="Sistema",ylab="Abundacia")
boxplot(VERCYA, main="Abundancia Vermivora cyanoptera",xlab="Sistema",ylab="Abundacia")

##Riqueza
Riqueza<-read.csv("C:/Users/David Murillo/R/Tesis/Datos/Riqueza de especies.csv")
Todas<-(Riqueza[1,2:5])
Migratorias<-(Riqueza[2,2:5])
Residentes<-(Riqueza[3,2:5])
boxplot(Todas,main="Riqueza de aves",xlab="Sistema",ylab="Riqueza",cex.lab="0.8",cex.lab="0.8",)
