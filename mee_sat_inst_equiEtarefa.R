library(sem)

library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '

#STRESS =~ S1+S2+S3
#EMOCAO =~ EM1+ EM2 +EM3+ EM4
#INTER =~ I1+I2+I3+I4
#CRIATIVIDADE =~ C2+C3+C4
TREIN =~ T1+T2+T3+T4

T1~~T2

#EM2 ~~ EM3


SATISFACAO=~ sat1+sat2+sat4


#SATISFACAO ~ STRESS
#0.365
#SATISFACAO ~ EMOCAO
#0.354
#SATISFACAO ~ INTER
# 0.354
#SATISFACAO ~ CRIATIVIDADE
# 0.286
#SATISFACAO ~TREIN
# 0.247


instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
instabilidadeEquipe=~ equipe1+equipe2+equipe3




#SATISFACAO ~ a*STRESS + b*instabilidadeTarefas
# ei               -0.078    0.027   -2.845    0.004   -0.065   -0.065
# SATISFACAO ~                                                          
#    STRESS     (a)    0.380    0.072    5.248    0.000    0.348    0.348
#    instblddTr (b)   -0.206    0.060   -3.403    0.001   -0.188   -0.188

#SATISFACAO ~ a*STRESS + b*instabilidadeEquipe
#  ei               -0.131    0.040   -3.297    0.001   -0.103   -0.103
#SATISFACAO ~                                                          
#    STRESS     (a)    0.397    0.075    5.308    0.000    0.353    0.353
#    instblddEq (b)   -0.329    0.071   -4.662    0.000   -0.292   -0.292

#SATISFACAO ~ a*EMOCAO + b*instabilidadeTarefas
# ei               -0.074    0.025   -2.968    0.003   -0.063   -0.063
# SATISFACAO ~                                                          
#    EMOCAO     (a)    0.344    0.065    5.311    0.000    0.318    0.318
#    instblddTr (b)   -0.216    0.059   -3.655    0.000   -0.200   -0.200

#SATISFACAO ~ a*EMOCAO + b*instabilidadeEquipe
# ei               -0.130    0.038   -3.425    0.001   -0.104   -0.104
#SATISFACAO ~                                                          
#    EMOCAO     (a)    0.378    0.068    5.580    0.000    0.338    0.338
#    instblddEq (b)   -0.345    0.070   -4.958    0.000   -0.308   -0.308

#SATISFACAO ~ a*INTER + b*instabilidadeTarefas
# ei               -0.063    0.023   -2.716    0.007   -0.054   -0.054
#SATISFACAO ~                                                          
#    INTER      (a)    0.333    0.069    4.817    0.000    0.309    0.309
#    instblddTr (b)   -0.188    0.060   -3.148    0.002   -0.174   -0.174


#SATISFACAO ~ a*INTER + b*instabilidadeEquipe
# ei               -0.099    0.031   -3.186    0.001   -0.081   -0.081
#SATISFACAO ~                                                          
#    INTER      (a)    0.331    0.070    4.706    0.000    0.300    0.300
#    instblddEq (b)   -0.299    0.069   -4.354    0.000   -0.271   -0.271


#SATISFACAO ~ a*CRIATIVIDADE + b*instabilidadeTarefas
#  ei               -0.034    0.017   -1.995    0.046   -0.032   -0.032
#SATISFACAO ~                                                          
#    CRIATIVIDA (a)    0.161    0.067    2.422    0.015    0.156    0.156
#    instblddTr (b)   -0.212    0.058   -3.661    0.000   -0.205   -0.205

#SATISFACAO ~ a*CRIATIVIDADE + b*instabilidadeEquipe
# ei               -0.043    0.023   -1.854    0.064   -0.038   -0.038
#SATISFACAO ~                                                          
#    CRIATIVIDA (a)    0.135    0.068    1.984    0.047    0.127    0.127
#    instblddEq (b)   -0.319    0.067   -4.773    0.000   -0.301   -0.301


#SATISFACAO ~ a*TREIN + b*instabilidadeTarefas
# ei               -0.039    0.016   -2.450    0.014   -0.036   -0.03
#SATISFACAO ~                                                          
#    TREIN      (a)    0.213    0.065    3.277    0.001    0.204    0.204
#    instblddTr (b)   -0.184    0.058   -3.151    0.002   -0.176   -0.176

SATISFACAO ~ a*TREIN + b*instabilidadeEquipe
# ei               -0.072    0.026   -2.800    0.005   -0.063   -0.063
#SATISFACAO ~                                                          
#    TREIN      (a)    0.238    0.066    3.620    0.000    0.222    0.222
#    instblddEq (b)   -0.304    0.067   -4.555    0.000   -0.283   -0.283


ei:= a*b

' #verificar modelo adaptabilidade



modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
