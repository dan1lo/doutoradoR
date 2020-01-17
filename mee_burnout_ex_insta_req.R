library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '


TREIN =~ T1+T2+T3+T4


burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5+ee4

instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
#instabilidadeEquipe =~ equipe1+equipe2+equipe3

#burnoutExaustaoEmocional ~ a*STRESS + b*instabilidadeTarefas
#-0.102 p 0.000
# burnoutExaustaoEmocional ~ STRESS   -0.362 0.000
# instblddTr                 0.000        0.281

#burnoutExaustaoEmocional ~ a*EMOCAO + b*instabilidadeTarefas
#--0.096 p 0.000
#burnoutExaustaoEmocional ~                                                      
#    EMOCAO                     0.000      -0.323
#    instblddTr                0.000       0.297

#burnoutExaustaoEmocional ~ a*INTER + b*instabilidadeTarefas
#-0.055 p 0.009
#burnoutExaustaoEmocional ~                                                      
#    INTER      (a)              0.003      -0.193
#    instblddTr (b)               0.000     0.286


#burnoutExaustaoEmocional ~ a*CRIATIVIDADE + b*instabilidadeTarefas
#-0.077 0.003
#burnoutExaustaoEmocional ~                                                      
#    CRIATIVIDA (a)            0.000     -0.252
# instblddTr (b)               0.000     0.304

#burnoutExaustaoEmocional ~ a*TREIN + b*instabilidadeTarefas
#ei              0.001     -0.071
#burnoutExaustaoEmocional ~                                                      
#    TREIN (a)              0.000      -0.227
#    instabilidadeTarefas (b)              0.000       0.257



#burnoutExaustaoEmocional ~ a*CRIATIVIDADE + b*instabilidadeEquipe
#ei              0.003     -0.098
#burnoutExaustaoEmocional ~                                                      
#    CRIATIVIDA (a)              0.001      -0.220
#    instblddEq (b)              0.000       0.445


#burnoutExaustaoEmocional ~ a*TREIN + b*instabilidadeTarefas
#ei              0.001     -0.071
#burnoutExaustaoEmocional ~                                                      
#    TREIN (a)              0.000      -0.227
#    instabilidadeTarefas (b)              0.000       0.257



ei:= a*b


ee3 ~~    ee5
ee1 ~~    ee2
req4 ~~   req7 

' #verificar modelo adaptabilidade




modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

