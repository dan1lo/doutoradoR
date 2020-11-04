library(sem)
library(lavaan)
library(semTools)
library(sqldf)
library(semMediation)
library(processR) #install.packages("processR")
#dados<-read.csv2(file = "GitHub/doutoradoR/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo
dados<-read.csv2(file = "doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo


names(dados)

modelo.v1 <- '

STRESS =~ S1+S2+S3
EMOCAO =~ EM1+ EM2 +EM3+ EM4
INTER =~ I1+I2+I3+I4
CRIATIVIDADE =~ C1+C2+C3+C4
TREIN =~ T1+T2+T3+

' #verificar modelo adaptabilidade

modelo.v2 <- '

STRESS =~ S1+S2+S3
EMOCAO =~ EM1+ EM2 +EM3+ EM4
INTER =~ I1+I2+I3+I4
CRIATIVIDADE =~ C1+C2+C3+C4
TREIN =~ T1+T2+T3+T4


STRESS=~1*EMOCAO


' #verificar modelo adaptabilidade

modelo.v1.fit <- cfa(modelo.v1, data=dados, std.lv=TRUE,ordered = TRUE  ) 
modelo.v2.fit <- cfa(modelo.v2, data=dados, std.lv=TRUE, ordered = TRUE ) 


anova(modelo.v1.fit, modelo.v2.fit)

reliability(modelo.v1.fit)
summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
summary(modelo.v2.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE)
lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
fitMeasures(modelo.v2.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit)
discriminantValidityTable(modelo.v1.fit)
semPlot::semPaths(modelo.v1.fit, "std")
sessionInfo()
