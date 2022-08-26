
#Teste Scott knott - comparação entre doses


library(ExpDes.pt)
library(ScottKnott)

dir()

dose<-read.table("dose2508.txt",h=T)
attach(dose)

head (dose)


Trat= as.factor(Trat)

res.Dano<-dose$Dano-mean(dose$Dano) #residuo 
res.Dano

shapiro.test(res.Dano) #teste de normalidade do residuo
bartlett.test(dose$Dano, dose$Trat) # teste de homocedasticidade 


## Análise dos Danos

dic(Trat, Dano, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Dano2, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Dano3, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Dano4, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Dano5, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Dano6, quali = TRUE, mcomp = "sk", sigT = 0.05)


## An�lise do N de Lagartas


summary(dose)

dic(Trat, Lag1, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Lag2, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Lag3, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Lag4, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Lag5, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Lagcolheita, quali = TRUE, mcomp = "sk", sigT = 0.05)

## Produtividade


dic(Trat, Peso, quali = TRUE, mcomp = "sk", sigT = 0.05)

dic(Trat, Kg.ha, quali = TRUE, mcomp = "sk", sigT = 0.05)


## M�dia de lagartas


dic(Trat, Medialagartas, quali = TRUE, mcomp = "sk", sigT = 0.05)


##Transformação

sqrt.lagA<-sqrt(dose$Lag1+0.5)
sqrt.lagA

dic(Trat, sqrt.lagA, quali = TRUE, mcomp = "sk", sigT = 0.05)

sqrt.lagB<-sqrt(dose$Lag2+0.5)
sqrt.lagB

dic(Trat, sqrt.lagB, quali = TRUE, mcomp = "sk", sigT = 0.05)

sqrt.lagC<-sqrt(dose$Lag3+0.5)
sqrt.lagC

dic(Trat, sqrt.lagC, quali = TRUE, mcomp = "sk", sigT = 0.05)


detach(dose)


