library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '



STRESS =~ S1+S2+S3


burnoutCinismo =~ ci1+ci2+ci3+ci4

instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
#instabilidadeEquipe =~ equipe1+equipe2+equipe3

#burnoutCinismo ~ a*STRESS + b*instabilidadeTarefas
# i               -0.117    0.038   -3.064    0.002   -0.093   -0.093
# burnoutCinismo ~                                                      
#    STRESS     (a)   -0.296    0.078   -3.817    0.000   -0.264   -0.264
#    instblddTr (b)    0.394    0.070    5.642    0.000    0.351    0.351

#burnoutCinismo ~ a*EMOCAO + b*instabilidadeTarefas
#ei               -0.145    0.041   -3.521    0.000   -0.112   -0.112
#burnoutCinismo ~                                                      
#    EMOCAO     (a)   -0.357    0.074   -4.850    0.000   -0.313   -0.313
#    instblddTr (b)    0.408    0.071    5.765    0.000    0.357    0.357

#burnoutCinismo ~ a*INTER + b*instabilidadeTarefas
# ei              -0.089    0.033   -2.708    0.007   -0.073   -0.073
#burnoutCinismo ~                                                      
 #   INTER      (a)   -0.235    0.074   -3.161    0.002   -0.213   -0.213
 #   instblddTr (b)    0.380    0.069    5.497    0.000    0.344    0.344




#burnoutCinismo ~ a*CRIATIVIDADE + b*instabilidadeTarefas
#  ei               -0.081    0.034   -2.377    0.017   -0.067   -0.067
#burnoutCinismo ~                                                      
#    CRIATIVIDA (a)   -0.201    0.073   -2.741    0.006   -0.183   -0.183
#    instblddTr (b)    0.404    0.070    5.795    0.000    0.368    0.368

#burnoutCinismo ~ a*TREIN + b*instabilidadeTarefas
# ei               -0.081    0.028   -2.895    0.004   -0.068   -0.068
#                                                     
#    burnoutCinismo ~                                                      
#   TREIN      (a)   -0.239    0.066   -3.611    0.000   -0.218   -0.218
#    instblddTr (b)    0.341    0.067    5.086    0.000    0.311    0.311



ei:= a*b

ci3 ~~  ci4
ci1 ~~ci2
ci2 ~~ ci4

#ci1 ~~ ci3


' #verificar modelo adaptabilidade




modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

