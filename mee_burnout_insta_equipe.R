library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '

burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5+ee4
burnoutCinismo =~ ci1+ci2+ci3+ci4
burnoutEficacia =~ et1+et2+et3+et6+et4




instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
instabilidadeEquipe=~ equipe1+equipe2+equipe3

equipe1 ~~equipe2
equipe1 ~~ equipe3
equipe2 ~~equipe3
et3 ~~ et6
ci3 ~~ ci4
ee3 ~~ ee5
ci2 ~~ ci4  
et1 ~~ et2



#burnoutExaustaoEmocional ~ instabilidadeEquipe
#  0.839 p 0.000
#burnoutCinismo ~ instabilidadeEquipe
# 0.962 p 0.047 
burnoutEficacia ~ instabilidadeEquipe
# -0.744 p 0.000


' #verificar modelo adaptabilidade



modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

