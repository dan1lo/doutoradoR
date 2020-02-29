library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('S1','S2','S3'), var2 = c('reque2','reque3','req5','req6','req7'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(dados.dmc)
modelo.v1 <- '

STRESS =~ S1+S2+S3

T1~~T2

instabilidadeTarefas=~ reque2+reque3+req5+req6+req7
ab=~ S1.reque2 + S1.reque3 + S1.req5 + S1.req6+ S1.req7 + S2.reque2 + S2.reque3 + S2.req7+ S2.req5+ S2.req6 + S3.reque2 + S3.reque3 + S3.req5+ S3.req6+ S3.req7

burnoutEficacia =~ et1+et2+et3+et5+et6

burnoutEficacia ~ a * STRESS
burnoutEficacia ~ b * instabilidadeTarefas
burnoutEficacia ~ ab


 S3.reque2 ~~ S3.reque3 
S1.reque2 ~~ S1.reque3 
S3.reque3 ~~   S3.req6  
S2.reque2 ~~ S2.reque3  
S3.reque3 ~~   S3.req5  
S3.req6 ~~   S3.req7  
S3.reque2 ~~   S3.req6  
S3.req5 ~~   S3.req6  
S3.req5 ~~   S3.req7  
S3.reque3 ~~   S3.req7  
S3.reque2 ~~   S3.req7  
S3.reque2 ~~   S3.req5  
S2.reque2 ~~   S2.req6  
S1.reque3 ~~ S2.reque2  
S1.reque3 ~~   S1.req6
S2.reque3 ~~   S2.req6 
S1.reque2 ~~ S2.reque3 

S1.req5 ~~   S3.req5 
S1.req7 ~~   S3.req7 
S1.req6 ~~   S3.req6 
S1.reque2 ~~ S3.reque2 
S1.reque2 ~~   S1.req6 
S2.req6 ~~   S3.req6 
S2.reque2 ~~ S3.reque2
S1.req6 ~~   S2.req6
S1.reque2 ~~   S2.req6
S2.req5 ~~   S2.req6
S1.req6 ~~ S2.reque2

 et1 ~~       et2 
' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




