library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '



STRESS =~ S1+S2+S3


burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5+ee4

#instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
instabilidadeEquipe =~ equipe1+equipe2+equipe3

burnoutExaustaoEmocional ~ a*STRESS + b*instabilidadeEquipe
# ei               -0.278    0.075   -3.702    0.000   -0.177   -0.177
# burnoutExaustaoEmocional ~                                                      
#    STRESS     (a)             -0.476    0.086   -5.549    0.000   -0.380   -0.380
#    instblddEq (b)              0.584    0.092    6.347    0.000    0.466    0.466

#burnoutExaustaoEmocional ~ a*EMOCAO + b*instabilidadeEquipe
#ei               -0.263    0.070   -3.757    0.000   -0.172   -0.172
#burnoutExaustaoEmocional ~                                                      
#    EMOCAO     (a)             -0.439    0.077   -5.674    0.000   -0.355   -0.355
#    instblddEq (b)              0.599    0.092    6.538    0.000    0.485    0.485

#burnoutExaustaoEmocional ~ a*INTER + b*instabilidadeEquipe
#ei               -0.115    0.044   -2.597    0.009   -0.086   -0.086
#burnoutExaustaoEmocional ~                                                      
#    INTER      (a)             -0.218    0.075   -2.927    0.003   -0.188   -0.188
#   instblddEq (b)              0.528    0.084    6.273    0.000    0.456    0.456




#burnoutExaustaoEmocional ~ a*CRIATIVIDADE + b*instabilidadeEquipe
#ei              0.003     -0.098
#burnoutExaustaoEmocional ~                                                      
#    CRIATIVIDA (a)              0.001      -0.220
#    instblddEq (b)              0.000       0.445


#burnoutExaustaoEmocional ~ a*TREIN + b*instabilidadeEquipe
#ei               -0.195    0.053   -3.653    0.000   -0.136   -0.136
#burnoutExaustaoEmocional ~                                                      
#    TREIN      (a)             -0.366    0.072   -5.062    0.000   -0.306   -0.306
#    instblddEq (b)              0.533    0.085    6.253    0.000    0.446    0.446



ei:= a*b


ee3 ~~    ee5
ee1 ~~    ee2
req4 ~~   req7 
ee2 ~~  ee5
' #verificar modelo adaptabilidade




modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

