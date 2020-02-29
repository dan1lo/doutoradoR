library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('S1','S2','S3'), var2 = c('equipe1','equipe2','equipe3'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(dados.dmc)
modelo.v1 <- '

STRESS =~ S1+S2+S3

T1~~T2

instabilidadeEquipe=~ equipe1+equipe2+equipe3
xz =~ S1.equipe1 + S1.equipe2 + S1.equipe3 + S2.equipe1 + S2.equipe2 + S2.equipe3 + S3.equipe1 + S3.equipe2 + S3.equipe3
burnoutCinismo =~ ci1+ci2+ci3+ci4

burnoutCinismo ~ a * STRESS
burnoutCinismo ~ b * instabilidadeEquipe
burnoutCinismo ~ xz

S1.equipe3 ~~ S2.equipe3
S3.equipe1 ~~ S3.equipe2
S1.equipe2 ~~ S2.equipe2
S1.equipe2 ~~ S1.equipe3
S2.equipe2 ~~ S2.equipe3
S1.equipe1 ~~ S2.equipe2
S1.equipe3 ~~ S2.equipe1
S1.equipe2 ~~ S3.equipe2
S2.equipe3 ~~ S3.equipe3
S1.equipe1 ~~ S2.equipe3
S1.equipe1 ~~ S1.equipe2
S3.equipe1 ~~ S3.equipe3
S1.equipe1 ~~ S3.equipe3
S1.equipe3 ~~ S3.equipe3
S2.equipe2 ~~ S3.equipe2

' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




