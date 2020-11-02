library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "doutoradoModelo20.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados) # Verificar nomes das colunas
nrow(dados)#Verificar quantas linhas tem
dados<-sqldf("SELECT * FROM dados WHERE satisfacao is not null and instabilidade is not null and burnout_cinismo is not null and burnout_exaustao is not null and burnout_et is not null")
summary(dados$instabilidade) # verificar dados descritivos da coluna instabilidade dentro de dados
write.csv(dados, "dadosTratadosModel1.csv")
modelo.v1.fit <- cfa(modelo.v2, data=dados, std.lv=TRUE) # fazer analise confimatória

summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
