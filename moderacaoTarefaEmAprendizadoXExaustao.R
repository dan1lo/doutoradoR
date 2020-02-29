library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('T1','T2','T3','T4'), var2 = c('reque2','reque3','req5','req6','req7'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
modelo.v1 <- '

TREIN =~ T1+T2+T3+T4

instabilidadeTarefas=~ reque2+reque3+req5+req6+req7
ab =~   T1.reque2 + T1.reque3 + T1.req5 + T1.req6 + T1.req7+ T2.reque2 + T2.reque3 + T2.req5 + T2.req6 + T2.req7 + T3.reque2 + T3.reque3 + T3.req5 + T3.req6 + T3.req7+ T4.reque2 + T4.reque3 + T4.req5 + T4.req6 + T4.req7
burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5+ee4

burnoutExaustaoEmocional ~ a * TREIN
burnoutExaustaoEmocional ~ b * instabilidadeTarefas
burnoutExaustaoEmocional ~ ab

T4.reque2 ~~ T4.reque3 
T4.reque3 ~~   T4.req7  
T4.reque2 ~~   T4.req5  
T3.reque2 ~~ T3.reque3  
T4.reque3 ~~   T4.req6  
T4.reque3 ~~   T4.req5  
T4.reque2 ~~   T4.req6  
T4.reque2 ~~   T4.req7  
T1.req5 ~~   T2.req5  
T1.req7 ~~   T2.req7  
T4.req5 ~~   T4.req7  
T4.req5 ~~   T4.req6  
T2.reque2 ~~ T2.reque3  
T2.reque2 ~~   T2.req6  
T1.req6 ~~   T2.req6  
T1.reque3 ~~   T1.req5  
T3.req5 ~~   T3.req7  
T3.reque2 ~~   T3.req5  
T4.req6 ~~   T4.req7  
T3.reque3 ~~   T3.req5  
T1.req6 ~~ T3.reque3  
T1.req7 ~~   T3.req7  
T3.req5 ~~   T3.req6  
T2.req7 ~~   T3.req7  
T2.reque3 ~~   T2.req6  
T2.req6 ~~   T2.req7  
T3.reque2 ~~   T3.req7  
T1.reque3 ~~ T2.reque3  
T1.reque2 ~~ T1.reque3  
T1.reque3 ~~   T3.req6  
T1.req7 ~~   T4.req7  
T1.req6 ~~   T3.req6  
T1.req7 ~~ T2.reque2  
T3.reque2 ~~   T3.req6 
T1.reque2 ~~   T1.req6 
T2.reque3 ~~   T2.req7
T1.reque2 ~~ T3.reque2
T2.reque3 ~~   T2.req5
T2.reque2 ~~ T3.reque2
T2.reque2 ~~   T2.req7
T2.req5 ~~   T2.req7
T1.req5 ~~   T3.req5
T2.req5 ~~   T3.req5
T2.req5 ~~   T2.req6 
T1.reque2 ~~ T2.reque2
T3.reque3 ~~   T3.req7 
T1.req6 ~~   T2.req5
T3.reque3 ~~   T3.req6
T3.req6 ~~   T3.req7
T2.reque2 ~~   T2.req5
T2.req6 ~~   T3.req6
' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




