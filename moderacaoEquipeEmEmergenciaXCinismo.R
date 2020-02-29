library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('EM1','EM2','EM3','EM4'), var2 = c('equipe1','equipe2','equipe3'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(dados.dmc)
modelo.v1 <- '

EMERGENCIA =~ EM1+ EM2 +EM3+ EM4

instabilidadeEquipe=~ equipe1+equipe2+equipe3
xz =~ EM1.equipe1 + EM1.equipe2 + EM1.equipe3 + EM2.equipe1 + EM2.equipe2 + EM2.equipe3 + EM3.equipe1 + EM3.equipe2 + EM3.equipe3 + EM4.equipe1 + EM4.equipe2 + EM4.equipe3


burnoutCinismo =~ ci1+ci2+ci3+ci4

burnoutCinismo ~ a * EMERGENCIA
burnoutCinismo ~ b * instabilidadeEquipe
burnoutCinismo ~ xz

EM1.equipe1 ~~ EM2.equipe1
EM1.equipe2 ~~ EM4.equipe2
EM1.equipe3 ~~ EM4.equipe3
EM1.equipe2 ~~ EM2.equipe3
EM2.equipe3 ~~ EM3.equipe3
EM2.equipe1 ~~ EM3.equipe1
EM2.equipe2 ~~ EM3.equipe2
EM3.equipe3 ~~ EM4.equipe1
EM1.equipe3 ~~ EM2.equipe2
EM3.equipe1 ~~ EM4.equipe1
EM2.equipe1 ~~ EM4.equipe2
EM1.equipe3 ~~ EM2.equipe3
EM1.equipe1 ~~ EM4.equipe1
EM1.equipe1 ~~ EM3.equipe2
EM2.equipe3 ~~ EM4.equipe3
EM2.equipe1 ~~ EM2.equipe3
EM2.equipe2 ~~ EM2.equipe3
EM4.equipe1 ~~ EM4.equipe3
EM3.equipe1 ~~ EM3.equipe3
EM1.equipe1 ~~ EM1.equipe2
EM4.equipe1 ~~ EM4.equipe2
EM1.equipe2 ~~ EM1.equipe3
EM1.equipe2 ~~ EM2.equipe2
EM1.equipe1 ~~ EM1.equipe3
EM2.equipe1 ~~ EM2.equipe2
EM1.equipe1 ~~ EM3.equipe1
EM2.equipe1 ~~ EM3.equipe2
EM1.equipe2 ~~ EM3.equipe2
EM1.equipe3 ~~ EM3.equipe3

' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




