library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('T1','T2','T3','T4'), var2 = c('equipe1','equipe2','equipe3'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
modelo.v1 <- '

TREIN =~ T1+T2+T3+T4

instabilidadeEquipe=~ equipe1+equipe2+equipe3
ab =~ T1.equipe1 + T1.equipe2 + T1.equipe3 + T2.equipe1 + T2.equipe2 + T2.equipe3 + T3.equipe1 + T3.equipe2 + T3.equipe3 + T4.equipe1 + T4.equipe2 + T4.equipe3
SATISFACAO=~ sat1+sat2+sat4

SATISFACAO ~ a * TREIN
SATISFACAO ~ b * instabilidadeEquipe
SATISFACAO ~ ab

T1.equipe2 ~~ T2.equipe2
T1.equipe1 ~~ T2.equipe1
T1.equipe3 ~~ T2.equipe3
T2.equipe2 ~~ T3.equipe2
T1.equipe2 ~~ T3.equipe2
T1.equipe1 ~~ T3.equipe1
T1.equipe3 ~~ T2.equipe1
T3.equipe2 ~~ T4.equipe2
T1.equipe2 ~~ T2.equipe3
T2.equipe2 ~~ T4.equipe2
T1.equipe3 ~~ T3.equipe1
T1.equipe3 ~~ T3.equipe3
T4.equipe1 ~~ T4.equipe3
T1.equipe3 ~~ T4.equipe1
T1.equipe1 ~~ T2.equipe3
T4.equipe1 ~~ T4.equipe2
T2.equipe3 ~~ T3.equipe2
T1.equipe1 ~~ T4.equipe3
T1.equipe2 ~~ T4.equipe2
T2.equipe1 ~~ T2.equipe2
T2.equipe1 ~~ T3.equipe1
T1.equipe2 ~~ T2.equipe1
T2.equipe3 ~~ T3.equipe1

' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




