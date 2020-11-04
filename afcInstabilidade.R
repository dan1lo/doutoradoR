library(sem)
library(lavaan)
library(semTools)
library(sqldf)
#dados<-read.csv2(file = "GitHub/doutoradoR/doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo
dados<-read.csv2(file = "doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo
names(dados)

modelo.v1 <- '

instabilidadeTarefas =~ reque2+reque3+req5+req6+req7
instabilidadeEquipe=~ equipe1+equipe2+equipe3
instabildiadeTecnica=~ tec1+tec2


' #verificar modelo adaptabilidade

modelo.v2 <- '

instabilidadeTarefas =~ reque2+reque3+req5+req6+req7
instabilidadeEquipe=~ equipe1+equipe2+equipe3
instabildiadeTecnica=~ tec1+tec2

instabilidadeTarefas=~ 1*instabilidadeEquipe




' #verificar modelo adaptabilidade

modelo.v1.fit <- cfa(modelo.v1, data=dados, std.lv=TRUE,ordered = TRUE  ) 
modelo.v2.fit <- cfa(modelo.v2, data=dados, std.lv=TRUE, ordered = TRUE ) 
anova(modelo.v1.fit, modelo.v2.fit)

summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
summary(modelo.v2.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # fitMeasures(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
fitMeasures(modelo.v1.fit)
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
reliability(modelo.v1.fit)
semPlot::semPaths(modelo.v1.fit, "std")
sessionInfo()
