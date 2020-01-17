library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '



STRESS =~ S1+S2+S3


burnoutEficacia =~ et1+et2+et3+et5+et6

#instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
instabilidadeEquipe =~ equipe1+equipe2+equipe3

burnoutEficacia ~ a*STRESS + b*instabilidadeEquipe
#  ei               -0.128    0.087   -1.462    0.144   -0.067   -0.067
# burnoutEficacia ~                                                      
#    STRESS     (a)     0.936    0.130    7.180    0.000    0.680    0.680
#    instblddEq (b)    -0.137    0.089   -1.536    0.125   -0.099   -0.099

#burnoutEficacia ~ a*EMOCAO + b*instabilidadeEquipe
#ei               -0.164    0.086   -1.907    0.05   -0.085   -0.085
#burnoutEficacia ~                                                      
#    EMOCAO     (a)     0.963    0.109    8.845    0.000    0.691    0.691
#    instblddEq (b)    -0.171    0.084   -2.037    0.042   -0.122   -0.122


#burnoutEficacia ~ a*INTER + b*instabilidadeEquipe
# ei               -0.041    0.034   -1.222    0.222   -0.034   -0.034
#b burnoutEficacia ~                                                      
#    INTER      (a)     0.448    0.081    5.566    0.000    0.407    0.407
#    instblddEq (b)    -0.092    0.074   -1.244    0.214   -0.084   -0.084




#burnoutEficacia ~ a*CRIATIVIDADE + b*instabilidadeEquipe
# ei               -0.045    0.042   -1.072    0.284   -0.034   -0.034
# burnoutEficacia ~                                                      
#    CRIATIVIDA (a)     0.543    0.087    6.233    0.000    0.475    0.475
#    instblddEq (b)    -0.082    0.076   -1.081    0.280   -0.072   -0.072

#burnoutEficacia ~ a*TREIN + b*instabilidadeEquipe
# ei               -0.043    0.036   -1.184    0.236   -0.034   -0.034
#     burnoutEficacia ~                                                      
#    TREIN      (a)     0.480    0.075    6.417    0.000    0.431    0.431
#    instblddEq (b)    -0.089    0.074   -1.208    0.227   -0.080   -0.080



ei:= a*b
et1 ~~ et2
et1 ~~ et6
et5 ~~ et6

' #verificar modelo adaptabilidade




modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

