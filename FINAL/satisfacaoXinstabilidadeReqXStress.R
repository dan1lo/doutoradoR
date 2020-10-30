library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

#dados.dmc <- indProd(dados,var1 = c('T1','T2','T3','T4'), var2 = c('equipe1','equipe2','equipe3'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
dados.dmc <- indProd(dados,var1 = c('S1','S2','S3'), var2 = c('reque2','reque3','req5','req6','req7'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)

names(dados.dmc)

modelo.v1 <- '

STRESS =~ S1+S2+S3
SATISFACAO=~ sat1+sat2+sat4

#instabilidadeEquipe=~ equipe1+equipe2+equipe3
instabilidadeTarefas =~ reque2+reque3+req5+req6+req7
#TREINxINST =~ T1.equipe1 + T1.equipe2 + T1.equipe3 + T2.equipe1 + T2.equipe2 + T2.equipe3 + T3.equipe1 + T3.equipe2 + T3.equipe3 + T4.equipe1 + T4.equipe2 + T4.equipe3
STRESSxINST =~ S1.reque2 + S1.reque3 + S1.req5+ S1.req6+ S1.req7 + S2.reque2 + S2.reque3 + S2.req5 + S3.reque2 + S3.reque3 + S3.req5+ S3.req6+ S3.req7

SATISFACAO ~ STRESS
SATISFACAO ~ instabilidadeTarefas
SATISFACAO ~ STRESSxINST


S3.reque2 ~~ S3.reque3 
S2.reque2 ~~ S2.reque3 
S3.reque3 ~~   S3.req6 
S1.reque2 ~~ S1.reque3  
S3.reque3 ~~   S3.req5  
S3.req6 ~~   S3.req7  
S3.reque2 ~~   S3.req6  
S3.req5 ~~   S3.req6  
S3.req5 ~~   S3.req7  
S3.reque3 ~~   S3.req7  
S1.reque2 ~~   S3.req5 
S2.reque2 ~~   S2.req5
S1.reque2 ~~ S2.reque2
S1.reque2 ~~ S2.reque3 
S1.req7 ~~   S3.req7 
S1.req6 ~~   S3.req6
S1.req5 ~~   S3.req5
 S3.reque2 ~~   S3.req5
 S2.reque3 ~~   S2.req5
 S3.reque2 ~~   S3.req7

'

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)

modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)

s <- standardizedSolution(modelo.v1.fit)
s[s$op == "~~",]
