library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)
dados.dmc <- indProd(dados,var1 = c('C2','C3','C4'), var2 = c('equipe1','equipe2','equipe3'), match=FALSE, meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
modelo.v1 <- '

CRIATIVIDADE =~ C2+C3+C4

instabilidadeEquipe=~ equipe1+equipe2+equipe3
ab =~ C2.equipe1 + C2.equipe2 + C2.equipe3 + C3.equipe1 + C3.equipe2 + C3.equipe3 + C4.equipe1 + C4.equipe2 + C4.equipe3

burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5+ee4

burnoutExaustaoEmocional ~ a * CRIATIVIDADE
burnoutExaustaoEmocional ~ b * instabilidadeEquipe
burnoutExaustaoEmocional ~ ab


C3.equipe1 ~~ C4.equipe1
C3.equipe3 ~~ C4.equipe3
C3.equipe3 ~~ C4.equipe1
C2.equipe1 ~~ C2.equipe3
C3.equipe2 ~~ C4.equipe1
C3.equipe1 ~~ C4.equipe3
C2.equipe2 ~~ C2.equipe3 
C2.equipe1 ~~ C2.equipe2
C3.equipe3 ~~ C4.equipe2
C2.equipe2 ~~ C3.equipe3
C3.equipe2 ~~ C4.equipe2
C3.equipe1 ~~ C4.equipe2
C3.equipe1 ~~ C4.equipe2
C2.equipe3 ~~ C3.equipe2
C3.equipe2 ~~ C4.equipe3
C2.equipe3 ~~ C4.equipe2
' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados.dmc, std.lv=TRUE) 


summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit) 




