library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('C2','C3','C4'), var2 = c('reque2','reque3','req5','req6','req7'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
modelo.v1 <- '

CRIATIVIDADE =~ C2+C3+C4

instabilidadeTarefas=~ reque2+reque3+req5+req6+req7
ab =~ C2.reque2 + C2.reque3 + C2.req5 + C2.req6 + C2.req7+ C3.reque2 + C3.reque3 + C3.req5 + C3.req6 + C3.req7+ C4.reque2 + C4.reque3 + C4.req5 + C4.req6 + C4.req7

SATISFACAO=~ sat1+sat2+sat4

SATISFACAO ~ a * CRIATIVIDADE
SATISFACAO ~ b * instabilidadeTarefas
SATISFACAO ~ ab

C2.reque2 ~~ C2.reque3 
C2.reque3 ~~   C2.req5  
C2.reque3 ~~   C2.req7  
C2.reque2 ~~   C2.req5  
C3.reque3 ~~   C3.req5  
C3.reque2 ~~ C3.reque3  
C4.req5 ~~   C4.req6  
C2.req6 ~~   C2.req7  
C2.reque2 ~~   C2.req6  
C4.reque3 ~~   C4.req5  
C4.reque3 ~~   C4.req6  
C4.reque2 ~~ C4.reque3  
C2.reque3 ~~   C2.req6  
C2.req5 ~~   C2.req6  
C3.reque2 ~~   C3.req6  
C3.reque3 ~~   C3.req7  
C4.reque2 ~~   C4.req6  
C3.reque2 ~~   C3.req5  
C3.reque3 ~~   C4.req6  
C3.req5 ~~   C3.req6  
C3.req7 ~~   C4.req7  
C2.reque2 ~~   C2.req7  
C3.reque2 ~~ C4.reque3  
C3.reque3 ~~ C4.reque2  
C3.req5 ~~   C4.req5  
C2.req5 ~~   C2.req7  
C4.reque2 ~~   C4.req5  
C2.reque2 ~~   C3.req6  
C2.reque3 ~~   C3.req5 
C3.req6 ~~   C4.req6  
C2.req6 ~~ C3.reque2  
C3.req6 ~~ C4.reque3  
C2.req7 ~~   C3.req5  
C2.req7 ~~ C3.reque3 
C2.reque3 ~~   C4.req6 
C2.reque2 ~~   C3.req5 
C2.req5 ~~ C3.reque2

C2.req5 ~~   C3.req5
C3.reque2 ~~ C4.reque2

' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




