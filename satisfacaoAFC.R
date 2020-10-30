
library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoSatisfacao.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
modelo.v2 <- '



SATISFACAO=~sat1+ sat2+sat4

' #verificar modelo



modelo.v1.fit <- cfa(modelo.v2, data=dados, std.lv=TRUE) # fazer analise confimatória

summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

