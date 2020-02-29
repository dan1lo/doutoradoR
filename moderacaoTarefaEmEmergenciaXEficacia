library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('EM1','EM2','EM3','EM4'), var2 = c('reque2','reque3','req5','req6','req7'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(dados.dmc)
modelo.v1 <- '

EMERGENCIA =~ EM1+ EM2 +EM3+ EM4

instabilidadeTarefas=~ reque2+reque3+req5+req6+req7
xz =~   EM1.reque2 + EM1.reque3 + EM1.req5 + EM1.req6 + EM1.req7+ EM2.reque2 + EM2.reque3 + EM2.req5 + EM2.req6 + EM2.req7 + EM3.reque2 + EM3.reque3 + EM3.req5 + EM3.req6 + EM3.req7+ EM4.reque2 + EM4.reque3 + EM4.req5 + EM4.req6 + EM4.req7
burnoutEficacia =~ et1+et2+et3+et5+et6

burnoutEficacia ~ a * EMERGENCIA
burnoutEficacia~ b * instabilidadeTarefas
burnoutEficacia ~ xz

ee3 ~~    ee5
ee1 ~~    ee2

EM2.reque2 ~~ EM2.reque3
EM3.reque2 ~~   EM3.req6  
EM3.reque2 ~~ EM3.reque3  
EM2.reque3 ~~   EM2.req5  
EM3.reque3 ~~   EM3.req6  
EM1.reque2 ~~ EM1.reque3  
EM1.req7 ~~   EM4.req7  
EM1.req5 ~~   EM4.req5  
EM1.reque3 ~~ EM4.reque3  
EM2.reque2 ~~   EM2.req5  
EM4.reque2 ~~ EM4.reque3  
EM4.reque3 ~~   EM4.req5  
EM1.reque2 ~~ EM4.reque2  
EM3.reque3 ~~   EM3.req5  
EM1.req6 ~~   EM4.req6  
EM2.req6 ~~   EM2.req7  
EM3.req5 ~~   EM3.req7  
EM1.reque2 ~~ EM3.reque3  
EM1.reque2 ~~   EM1.req6  
EM4.reque2 ~~   EM4.req6  
EM4.req5 ~~   EM4.req6  
EM1.reque2 ~~ EM2.reque3  
EM1.reque3 ~~ EM3.reque2  
EM4.reque3 ~~   EM4.req6  
EM3.reque2 ~~   EM3.req5  
EM2.req7 ~~   EM3.req7  
EM1.reque3 ~~ EM2.reque2  
EM2.reque2 ~~ EM3.reque2  
EM1.reque2 ~~   EM1.req5  
EM4.reque2 ~~   EM4.req7  
EM4.reque2 ~~   EM4.req5  
EM3.req5 ~~   EM3.req6  
EM2.reque3 ~~   EM2.req7  
EM3.req6 ~~   EM3.req7  
EM3.reque3 ~~   EM3.req7  
EM2.req6 ~~   EM3.req6  
EM3.req7 ~~   EM4.req6  
EM3.reque2 ~~   EM4.req5  
EM2.reque2 ~~ EM4.reque3 
EM2.req5 ~~   EM3.req5
EM2.reque2 ~~ EM4.reque2
EM3.reque2 ~~   EM3.req7
EM2.reque3 ~~ EM3.reque3
EM2.req5 ~~   EM4.req5
EM2.reque3 ~~ EM4.reque2
EM1.reque3 ~~   EM4.req7
EM2.req5 ~~   EM2.req6
EM3.reque2 ~~ EM4.reque2
EM3.reque3 ~~ EM4.reque3
EM1.req5 ~~   EM4.req7
EM1.reque2 ~~ EM3.reque2 
EM4.reque3 ~~   EM4.req7 
EM1.req7 ~~   EM4.req5

ci3 ~~        ci4 
ci1 ~~        ci2 
ci2 ~~        ci4
ci1 ~~        ci3
' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




