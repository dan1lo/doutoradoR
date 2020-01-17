library(sem)
library(lavaan)
library(semTools)
library(sqldf)
dados<-read.csv2(file = "pesquisaIC.csv",header = TRUE, sep = ";", dec=",") # abrir arquivo

names(dados) # Verificar nomes das colunas
nrow(dados)#Verificar quantas linhas tem
dados<-sqldf("SELECT * FROM dados WHERE satisfacao is not null and instabilidade is not null and burnout_cinismo is not null and burnout_exaustao is not null and burnout_et is not null")
summary(dados$instabilidade) # verificar dados descritivos da coluna instabilidade dentro de dados
write.csv(dados, "dadosTratadosModel1.csv")


View(dados) # visualizar dados

modelo.v1 <- '


burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5+ee4
burnoutCinismo =~ ci1+ci2+ci3+ci4
burnoutEficacia =~ et2+et3+et5+et6+et4


et3 ~~ et6
ci3 ~~ ci4
et3 ~~ et5
ee3 ~~ ee5
ci2 ~~ ci4
#et1 ~~ et2
ee2 ~~ ee4
et6 ~~ et4
et3 ~~ et4
ee1 ~~ ee2
ci2 ~~ ci3
et5 ~~ et4
ee1 ~~ ee3
ee2 ~~ ee5

ee1 ~~ ee5

ee1 ~~ ee4


et2 ~~ et3 

' #verificar modelo

modelo.v1 <- '

STRESS =~ S1+S2+S3
EMOCAO =~ EM1+ EM2 +EM3+ EM4
INTER =~ I1+I2+I3+I4
CRIATIVIDADE =~ C2+C3+C4
TREIN =~ T1+T2+T3+T4

#SATISFACAO=~ sat1+sat2+sat4

#burnoutExaustaoEmocional =~ ee1 +ee2 +ee3+ee5 +ee6
#burnoutCinismo =~ ci1+ci2+ci3+ci4
#burnoutEficacia =~ et1+et2+et3+et5+et6



T1~~T2

EM2 ~~ EM3

#C3 ~~ C4

#I1 ~~ I2
#I2 ~~ I3

#ci3 ~~ ci4
#ci2 ~~ ci4
#ee3 ~~ ee5
#sat1~~sat2
#ci2 ~~ci3




' #verificar modelo


modelo.v1 <- '

STRESS =~ S1+S2+S3
EMOCAO =~ EM1+ EM2 +EM3+ EM4
INTER =~ I1+I2+I3+I4
CRIATIVIDADE =~ C1+C3+C4
TREIN =~ T1+T2+T3+T4


T1~~T2

#EM2 ~~ EM3

#I2 ~~ I3

#I1 ~~ I2
#I2 ~~ I3




' #verificar modelo



modelo.v1.fit <- cfa(modelo.v1, data=dados, std.lv=TRUE) # fazer analise confimatória

summary(modelo.v1.fit, fit.measures = TRUE, rsquare =TRUE, standardized = TRUE) # 
fitMeasures(modelo.v1.fit)
reliability(modelo.v1.fit)
lavInspect(modelo.v1.fit,"cor.lv")
modificationindices(modelo.v1.fit, sort =TRUE, minimum.value = 9)
anova(modelo.v1.fit,modelo.v1.fit)


#regressoes

modelo= lm(burnout_exaustao~crise_ad+stress_ad+criatividade_ad+treinamento_ad+interpessoal_ad, data=dados)
modelo = lm(burnout_et~crise_ad+stress_ad+criatividade_ad+treinamento_ad+interpessoal_ad, data=dados)
modelo = lm(burnout_cinismo~crise_ad+stress_ad+criatividade_ad+treinamento_ad+interpessoal_ad, data=dados)
modelo= lm(satisfacao~crise_ad+stress_ad+criatividade_ad+treinamento_ad+interpessoal_ad, data=dados)
modelo = lm(burnt_total~crise_ad+stress_ad+criatividade_ad+treinamento_ad+interpessoal_ad, data=dados)
modelo= lm(burnout_exaustao~inst_req+inst_tec+inst_equipe, data=dados)
modelo = lm(burnout_et~inst_req+inst_tec+inst_equipe, data=dados)
modelo = lm(burnout_cinismo~inst_req+inst_tec+inst_equipe, data=dados)
modelo= lm(satisfacao~inst_req+inst_tec+inst_equipe, data=dados)
modelo = lm(burnt_total~inst_req+inst_tec+inst_equipe, data=dados)

#regresspes com moderacoes
modelo= lm(burnout_exaustao~crise_ad*instabilidade+stress_ad*instabilidade+criatividade_ad*instabilidade+treinamento_ad*instabilidade+interpessoal_ad*instabilidade, data=dados)
modelo= lm(burnout_cinismo~crise_ad*instabilidade+stress_ad*instabilidade+criatividade_ad*instabilidade+treinamento_ad*instabilidade+interpessoal_ad*instabilidade, data=dados)
modelo= lm(burnout_et~crise_ad*instabilidade+stress_ad*instabilidade+criatividade_ad*instabilidade+treinamento_ad*instabilidade+interpessoal_ad*instabilidade, data=dados)
modelo= lm(satisfacao~crise_ad*instabilidade+stress_ad*instabilidade+criatividade_ad*instabilidade+treinamento_ad*instabilidade+interpessoal_ad*instabilidade, data=dados)


modelo= lm(burnout_exaustao~crise_ad*inst_equipe+stress_ad*inst_equipe+criatividade_ad*inst_equipe+treinamento_ad*inst_equipe+interpessoal_ad*inst_equipe, data=dados)
modelo= lm(burnout_cinismo~crise_ad*inst_equipe+stress_ad*inst_equipe+criatividade_ad*inst_equipe+treinamento_ad*inst_equipe+interpessoal_ad*inst_equipe, data=dados)
modelo= lm(burnout_et~crise_ad*inst_equipe+stress_ad*inst_equipe+criatividade_ad*inst_equipe+treinamento_ad*inst_equipe+interpessoal_ad*inst_equipe, data=dados)
modelo= lm(satisfacao~crise_ad*inst_equipe+stress_ad*inst_equipe+criatividade_ad*inst_equipe+treinamento_ad*inst_equipe+interpessoal_ad*inst_equipe, data=dados)

modelo= lm(burnout_exaustao~crise_ad*inst_req+stress_ad*inst_req+criatividade_ad*inst_req+treinamento_ad*inst_req+interpessoal_ad*inst_req, data=dados)
modelo= lm(burnout_cinismo~crise_ad*inst_req+stress_ad*inst_req+criatividade_ad*inst_req+treinamento_ad*inst_req+interpessoal_ad*inst_req, data=dados)
modelo= lm(burnout_et~crise_ad*inst_req+stress_ad*inst_req+criatividade_ad*inst_req+treinamento_ad*inst_req+interpessoal_ad*inst_req, data=dados)
modelo= lm(satisfacao~crise_ad*inst_req+stress_ad*inst_req+criatividade_ad*inst_req+treinamento_ad*inst_req+interpessoal_ad*inst_req, data=dados)


modelo= lm(burnout_exaustao~crise_ad*inst_equipe, data= dados)
modelo= lm(burnout_exaustao~stress_ad+treinamento_ad, data=dados)


modelo = lm(burnout_et~crise_ad+stress_ad+treinamento_ad, data=dados)
modelo = lm(burnout_cinismo~crise_ad+treinamento_ad, data=dados)
modelo= lm(satisfacao~crise_ad+stress_ad+interpessoal_ad, data=dados)


modelo= lm(burnout_exaustao~crise_ad*inst_equipe+interpessoal_ad*inst_equipe, data=dados)
modelo= lm(burnout_cinismo~crise_ad*inst_equipe+stress_ad*inst_equipe+interpessoal_ad*inst_equipe, data=dados) #sem regressao significante
modelo= lm(burnout_et~crise_ad*inst_equipe+stress_ad*inst_equipe+treinamento_ad*inst_equipe, data=dados)
modelo= lm(satisfacao~crise_ad*inst_equipe+stress_ad*inst_equipe+criatividade_ad*inst_equipe+treinamento_ad*inst_equipe+interpessoal_ad*inst_equipe, data=dados)# instab equipe e criatividade 0.1

modelo= lm(burnout_exaustao~stress_ad*inst_req, data=dados)
modelo= lm(satisfacao~stress_ad*inst_req, data=dados)
modelo= lm(satisfacao~crise_ad+stress_ad+interpessoal_ad, data=dados) #todos em 0,05

modelo= lm(burnout_cinismo~stress_ad*inst_tec+treinamento_ad*inst_tec, data=dados) # tec em treinamento 0.01 e tec em stress 0.05
modelo= lm(burnout_et~crise_ad*inst_tec+stress_ad*inst_tec+treinamento_ad*inst_tec, data=dados) #tec em stress e treinamento 0.01 e 0.05
modelo= lm(satisfacao~crise_ad*inst_tec+stress_ad*inst_tec+criatividade_ad*inst_tec+treinamento_ad*inst_tec+interpessoal_ad*inst_tec, data=dados)# nada re



summary(modelo)
