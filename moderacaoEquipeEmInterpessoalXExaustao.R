library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('I1','I2','I3','I4'), var2 = c('equipe1','equipe2','equipe3'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(dados.dmc)
modelo.v1 <- '

INTER =~ I1+I2+I3+I4

instabilidadeEquipe=~ equipe1+equipe2+equipe3
ab =~ I1.equipe1 + I1.equipe2 + I1.equipe3 + I2.equipe1 + I2.equipe2 + I2.equipe3 + I3.equipe1 + I3.equipe2 + I3.equipe3 + I4.equipe1 + I4.equipe2 + I4.equipe3

burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5+ee4


burnoutExaustaoEmocional ~ a * INTER
burnoutExaustaoEmocional ~ b * instabilidadeEquipe
burnoutExaustaoEmocional ~ ab


I2.equipe2 ~~ I2.equipe3
I4.equipe1 ~~ I4.equipe2
I3.equipe2 ~~ I4.equipe2
I1.equipe2 ~~ I4.equipe2
I1.equipe1 ~~ I2.equipe1
I1.equipe3 ~~ I3.equipe3
I2.equipe1 ~~ I3.equipe1
I2.equipe1 ~~ I4.equipe2
I2.equipe3 ~~ I4.equipe1 
I2.equipe2 ~~ I3.equipe2
I2.equipe1 ~~ I2.equipe2 
I3.equipe1 ~~ I3.equipe2
I1.equipe2 ~~ I2.equipe2
I1.equipe2 ~~ I3.equipe2
I1.equipe1 ~~ I1.equipe2
I2.equipe1 ~~ I2.equipe3 
I2.equipe2 ~~ I4.equipe2
I1.equipe1 ~~ I3.equipe1
' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




