

library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '


SATISFACAO=~ sat1+sat2+sat4

#instabilidadeTarefas =~ reque2+reque3+req5+req6+req7
instabilidadeEquipe=~ equipe1+equipe2+equipe3
SATISFACAO ~instabilidadeEquipe



' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit)

s <- standardizedSolution(modelo.v1.fit)
s[s$op == "~~",]
