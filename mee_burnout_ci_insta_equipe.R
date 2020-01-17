library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '



TREIN =~ T1+T2+T3+T4


burnoutCinismo =~ ci1+ci2+ci3+ci4

#instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
instabilidadeEquipe =~ equipe1+equipe2+equipe3

#burnoutCinismo ~ a*STRESS + b*instabilidadeEquipe
# ei               -0.243    0.078   -3.124    0.002   -0.154   -0.154
# burnoutCinismo ~                                                      
#    STRESS     (a)   -0.370    0.089   -4.168    0.000   -0.294   -0.294
#    instblddEq (b)    0.658    0.104    6.311    0.000    0.524    0.524

#burnoutCinismo ~ a*EMOCAO + b*instabilidadeEquipe
#ei               -0.315    0.091   -3.452    0.001   -0.189   -0.189
#burnoutCinismo ~                                                      
#    EMOCAO     (a)   -0.450    0.087   -5.153    0.000   -0.349   -0.349
#    instblddEq (b)    0.699    0.110    6.383    0.000    0.542    0.542

#burnoutCinismo ~ a*INTER + b*instabilidadeEquipe
#ei               -0.165    0.061   -2.712    0.007   -0.112   -0.112
#burnoutCinismo ~                                                      
#    INTER      (a)   -0.265    0.083   -3.201    0.001   -0.218   -0.218
#    instblddEq (b)    0.624    0.100    6.237    0.000    0.512    0.512




#burnoutCinismo ~ a*CRIATIVIDADE + b*instabilidadeEquipe
# ei               -0.121    0.055   -2.201    0.028   -0.084   -0.084
#burnoutCinismo ~                                                      
#    CRIATIVIDA (a)   -0.195    0.081   -2.407    0.016   -0.162   -0.162
#    instblddEq (b)    0.623    0.099    6.281    0.000    0.519    0.519

#burnoutCinismo ~ a*TREIN + b*instabilidadeEquipe
#ei               -0.194    0.062   -3.155    0.002   -0.132   -0.132
#                                                     
#   burnoutCinismo ~                                                      
#    TREIN      (a)   -0.323    0.076   -4.245    0.000   -0.266   -0.266
#    instblddEq (b)    0.602    0.098    6.141    0.000    0.495    0.495



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

