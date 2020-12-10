library(sem)
library(lavaan)
library(semTools)
library(sqldf)
library(semMediation)
library(processR) #install.packages("processR")
dados<-read.csv2(file = "GitHub/doutoradoR/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo
dados<-read.csv2(file = "doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo


names(dados)

modelo.v1 <- '

burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5
burnoutCinismo =~ ci1+ci2+ci3+ci4
burnoutEficacia =~ et1+et2+et3+et6+et4
' #verificar modelo adaptabilidade

modelo.v2 <- '

burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5
burnoutCinismo =~ ci1+ci2+ci3+ci4
burnoutEficacia =~ et1+et2+et3+et6+et4


burnoutExaustaoEmocional=~1*burnoutCinismo


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
