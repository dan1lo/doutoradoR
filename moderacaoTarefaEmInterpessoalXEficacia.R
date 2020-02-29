library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('I1','I2','I3','I4'), var2 = c('reque2','reque3','req5','req6','req7'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(dados.dmc)
modelo.v1 <- '

INTER =~ I1+I2+I3+I4

instabilidadeTarefas=~ reque2+reque3+req5+req6+req7
ab =~ I1.reque2 + I1.reque3 + I1.req5 + I1.req6 + I1.req7 + I2.reque2 + I2.reque3 + I2.req5 +I2.req6 + I2.req7+ I3.reque2 + I3.reque3 + I3.req5 +I3.req6 + I3.req7+ I4.reque2 + I4.reque3 + I4.req5+I4.req6 + I4.req7

burnoutEficacia =~ et1+et2+et3+et5+et6
burnoutEficacia ~ a * INTER
burnoutEficacia ~ b * instabilidadeTarefas
burnoutEficacia ~ ab

I1.reque2 ~~ I1.reque3
I2.reque2 ~~ I2.reque3 
I4.reque3 ~~   I4.req5
I4.reque3 ~~   I4.req6  
I4.reque2 ~~ I4.reque3  
I1.reque3 ~~   I1.req7 
I3.reque3 ~~   I3.req5 
I3.reque3 ~~   I3.req7  
I1.reque2 ~~   I1.req6 
I2.req6 ~~   I2.req7 
I1.reque2 ~~   I1.req7  
I3.reque2 ~~   I3.req6  
I2.reque3 ~~   I2.req7 
I4.reque2 ~~   I4.req5  
I1.reque3 ~~   I1.req6  
I4.reque2 ~~   I4.req6  
I3.reque3 ~~   I3.req6  
I3.reque2 ~~ I3.reque3 
I2.reque2 ~~   I2.req6  
I4.reque3 ~~   I4.req7 
I3.reque2 ~~   I3.req5 
I1.reque2 ~~ I3.reque3  
I1.reque2 ~~   I1.req5  
I1.reque3 ~~   I1.req5 
I1.reque3 ~~ I3.reque2  
I1.req6 ~~   I1.req7 
I1.req5 ~~   I1.req7  
I4.req5 ~~   I4.req6  
I4.reque2 ~~   I4.req7 
I4.req6 ~~   I4.req7 
I2.reque3 ~~   I2.req5 
I1.req6 ~~   I3.req5  
I3.req5 ~~   I3.req7 
I3.reque3 ~~ I4.reque2 
I1.req5 ~~   I4.req5 
I3.reque2 ~~ I4.reque3  
I2.reque2 ~~   I2.req7  
I2.reque2 ~~   I4.req5  
I2.reque2 ~~   I2.req5 
I1.reque3 ~~ I2.reque3
I2.req5 ~~   I3.req5
I1.req7 ~~   I2.req7
I1.req5 ~~   I1.req6
I4.req5 ~~   I4.req7
I1.req7 ~~   I3.req7
I1.req5 ~~   I2.req5
I3.reque3 ~~ I4.reque3 
I2.req7 ~~   I3.req7
I3.req5 ~~   I3.req6
I3.reque2 ~~   I3.req7
I3.req6 ~~   I3.req7
I3.req6 ~~ I4.reque2
I2.req5 ~~   I4.req5
I3.req6 ~~ I4.reque2
I1.req5 ~~   I3.req5
I1.req5 ~~   I3.req5
I1.reque2 ~~ I4.reque3
I2.reque2 ~~ I3.reque2 
I3.req5 ~~   I4.req5
ee3 ~~       ee5

 et1 ~~       et2
 et1 ~~       et6
 I1.req5 ~~ I4.reque2
' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




