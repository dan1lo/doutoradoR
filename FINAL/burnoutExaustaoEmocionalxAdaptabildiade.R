

library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "teste/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados)

modelo.v1 <- '


STRESS =~ S1+S2+S3
#EMOCAO =~ EM1+ EM2 +EM3+ EM4
#INTER =~ I1+I2+I3+I4
#CRIATIVIDADE =~ C2+C3+C4
#TREIN =~ T1+T2+T3+T4

#T1~~T2



burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5
#burnoutCinismo =~ ci1+ci2+ci3+ci4
#burnoutEficacia =~ et1+et2+et3+et6+et4

burnoutExaustaoEmocional ~STRESS
#burnoutExaustaoEmocional ~EMOCAO
#burnoutExaustaoEmocional ~INTER
#burnoutExaustaoEmocional ~CRIATIVIDADE
#burnoutExaustaoEmocional ~TREIN



' #verificar modelo adaptabilidade

modelo.v1.fit <- sem(modelo.v1, data=dados, std.lv=TRUE) 
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit)


