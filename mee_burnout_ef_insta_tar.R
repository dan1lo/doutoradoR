library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '



TREIN =~ T1+T2+T3+T4


burnoutEficacia =~ et1+et2+et3+et5+et6

instabilidadeTarefas =~ reque2+reque3+req4+req5+req6+req7
#instabilidadeEquipe =~ equipe1+equipe2+equipe3

#burnoutEficacia ~ a*STRESS + b*instabilidadeTarefas
# ei                0.007    0.068    0.106    0.916    0.004    0.004
# burnoutEficacia ~                                                      
#    STRESS     (a)     0.920    0.126    7.294    0.000    0.677    0.677
#    instblddTr (b)     0.008    0.074    0.106    0.916    0.006    0.006

#burnoutEficacia ~ a*EMOCAO + b*instabilidadeTarefas
#ei               -0.006    0.066   -0.087    0.931   -0.003   -0.003
#burnoutEficacia ~                                                      
#    EMOCAO     (a)     0.950    0.105    9.009    0.000    0.689    0.689
#    instblddTr (b)    -0.006    0.069   -0.087    0.931   -0.004   -0.004


#burnoutEficacia ~ a*INTER + b*instabilidadeTarefas
# ei                0.008    0.029    0.264    0.792    0.006    0.006
#burnoutEficacia ~                                                      
#    INTER      (a)     0.456    0.080    5.677    0.000    0.415    0.415
#    instblddTr (b)     0.017    0.063    0.266    0.790    0.015    0.015




#burnoutEficacia ~ a*CRIATIVIDADE + b*instabilidadeTarefas
# ei               -0.013    0.035   -0.361    0.718   -0.010   -0.010
# burnoutEficacia ~                                                      
#    CRIATIVIDA (a)     0.547    0.086    6.344    0.000    0.480    0.480
#    instblddTr (b)    -0.023    0.064   -0.362    0.717   -0.020   -0.020

#burnoutEficacia ~ a*TREIN + b*instabilidadeTarefas
# ei                0.028    0.032    0.879    0.379    0.023    0.023
#     burnoutEficacia ~                                                      
#    TREIN      (a)     0.482    0.075    6.409    0.000    0.435    0.435
#    instblddTr (b)     0.058    0.063    0.913    0.361    0.052    0.052



ei:= a*b

et1 ~~  et2
req4 ~~ req7
et1 ~~  et6

' #verificar modelo adaptabilidade




modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)

