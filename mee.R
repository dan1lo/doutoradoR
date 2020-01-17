library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

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


#SATISFACAO ~instabilidadeTarefas
# - 0.201
#SATISFACAO ~instabilidadeEquipe
# - 0.287


#SATISFACAO ~ a*STRESS + b*instabilidadeTarefas
# -0.064 p 0.005
#SATISFACAO ~ a*STRESS + b*instabilidadeEquipe
# -0.101 p 0.001

#SATISFACAO ~ a*EMOCAO + b*instabilidadeTarefas
# -0.065 p 0.003
#SATISFACAO ~ a*EMOCAO + b*instabilidadeEquipe
# -0.110

#SATISFACAO ~ a*INTER + b*instabilidadeTarefas
# -0.052 p 0.008
#SATISFACAO ~ a*INTER + b*instabilidadeEquipe
# -0.080 0.002

#SATISFACAO ~ a*CRIATIVIDADE + b*instabilidadeTarefas
# -0.032 p 0.046
#SATISFACAO ~ a*CRIATIVIDADE + b*instabilidadeEquipe
# -0.038 0.064


#SATISFACAO ~ a*TREIN + b*instabilidadeTarefas
# -0.036 p 0.014
#SATISFACAO ~ a*TREIN + b*instabilidadeEquipe
# -0.063 0.005



ei:= a*b

' #verificar modelo adaptabilidade



modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
