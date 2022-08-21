dir()

bloco<-read.table("dados.txt",h=T)
attach(bloco)

head (bloco)

library(multcomp)
library(stats)
library(multcompView)
library(emmeans)
library(ggplot2)


Trat= as.factor(Trat)


#avaliacao 1
res.tipoA<-bloco$tipoA-mean(bloco$tipoA) #residuo 
res.tipoA

shapiro.test(res.tipoA) #teste de normalidade do residuo
bartlett.test(bloco$tipoA, bloco$Trat) # teste de homocedasticidade 

#avaliacao 2
res.tipoB<-bloco$tipoB-mean(bloco$tipoB)
res.tipoB

shapiro.test(res.tipoB)
bartlett.test(bloco$tipoB, bloco$Trat)

#avaliacao 3
res.tipoC<-bloco$tipoC-mean(bloco$tipoC)
res.tipoC

shapiro.test(res.tipoC)
bartlett.test(bloco$tipoC, bloco$Trat)

#avaliacao 4
res.tipoD<-bloco$tipoD-mean(bloco$tipoD)
res.tipoD

shapiro.test(res.tipoD)
bartlett.test(bloco$tipoD, bloco$Trat)

#avaliacao 5
res.tipoE<-bloco$tipoE-mean(bloco$tipoE)
res.tipoE

shapiro.test(res.tipoE)
bartlett.test(bloco$tipoE, bloco$Trat)


#Transformacao das medias para raiz quadrada para avaliacao 1

sqrt.tipoA<-sqrt(bloco$tipoA+0.5) 
sqrt.tipoA

res.tipoAT<-sqrt.tipoA-mean(sqrt.tipoA) #residuo das medias transformadas
res.tipoAT

shapiro.test(res.tipoAT) 

m_tipoA=aov(sqrt.tipoA~Trat) #modelo
summary(m_tipoA)
TukeyHSD(m_tipoA, ordered = TRUE)

marginal.model_tipoA = emmeans(m_tipoA , ~ Trat)
contrast(marginal.model_tipoA, method="pairwise", adjust="tukey")
cld(marginal.model_tipoA,level = 0.95, alpha=0.05, Letters=letters, adjust="tukey", decreasing = TRUE)

#Produtividade 
x_tipoA<-mean (sqrt.tipoA)
y_tipoA<-sd(sqrt.tipoA)

cv_tipoA <- y_tipoA / x_tipoA *100
cv_tipoA

#transformacao raiz quadrada para avaliacao 2

sqrt.tipoB<-sqrt(bloco$tipoB+0.5)
sqrt.tipoB

res.tipoBT<-sqrt.tipoB-mean(sqrt.tipoB)
res.tipoBT

shapiro.test(res.tipoBT)

m_tipoB=aov(sqrt.tipoB~Trat) #modelo
summary(m_tipoB)
TukeyHSD(m_tipoB, ordered = TRUE)


marginal.model_tipoB = emmeans(m_tipoB, ~ Trat)
contrast(marginal.model_tipoB, method="pairwise", adjust="tukey")
cld(marginal.model_tipoB,level = 0.95, alpha=0.05, Letters=letters, adjust="tukey", decreasing = TRUE)

#Produtividade 
x_tipoB<-mean (sqrt.tipoB)
y_tipoB<-sd(sqrt.tipoB)

c_tipoB <- y_tipoB / x_tipoB *100
c_tipoB

#transformacao raiz quadrada para avaliacao 3

sqrt.tipoC<-sqrt(bloco$tipoC+0.5)
sqrt.tipoC

res.tipoCT<-sqrt.tipoC-mean(sqrt.tipoC)
res.tipoCT

shapiro.test(res.tipoCT)

m_tipoC=aov(sqrt.tipoC~Trat) #modelo
summary(m_tipoC)
TukeyHSD(m_tipoC, ordered = TRUE)


marginal.model_tipoC = emmeans(m_tipoC , ~ Trat)
contrast(marginal.model_tipoC, method="pairwise", adjust="tukey")
cld(marginal.model_tipoC,level = 0.95, alpha=0.05, Letters=letters, adjust="tukey", decreasing = TRUE)

#Produtividade 
x_tipoC<-mean (sqrt.tipoC)
y_tipoC<-sd(sqrt.tipoC)

cv_tipoC <- y_tipoC / x_tipoC *100
cv_tipoC

#transformacao raiz quadrada para avaliacao 4

sqrt.tipoD<-sqrt(bloco$tipoD+0.5)
sqrt.tipoD

res.tipoDT<-sqrt.tipoD-mean(sqrt.tipoD)
res.tipoDT

shapiro.test(res.tipoDT)

m_tipoD=aov(sqrt.tipoD~Trat) #modelo
summary(m_tipoD)
TukeyHSD(m_tipoD, ordered = TRUE)


marginal.model_tipoD = emmeans(m_tipoD , ~ Trat)
contrast(marginal.model_tipoD, method="pairwise", adjust="tukey")
cld(marginal.model_tipoD,level = 0.95, alpha=0.05, Letters=letters, adjust="tukey", decreasing = TRUE)

#Produtividade 
x_tipoD<-mean (sqrt.tipoD)
y_tipoD<-sd(sqrt.tipoD)

cv_tipoD <- y_tipoD / x_tipoD *100
cv_tipoD
detach(bloco)

#transformacao raiz quadrada para avaliacao 5

sqrt.tipoE<-sqrt(bloco$tipoE+0.5)
sqrt.tipoE

res.tipoET<-sqrt.tipoE-mean(sqrt.tipoE)
res.tipoET

shapiro.test(res.tipoET)

m_tipoE=aov(sqrt.tipoE~Trat) #modelo
summary(m_tipoE)
TukeyHSD(m_tipoE, ordered = TRUE)


marginal.model_tipoE = emmeans(m_tipoE , ~ Trat)
contrast(marginal.model_tipoE, method="pairwise", adjust="tukey")
cld(marginal.model_tipoE,level = 0.95, alpha=0.05, Letters=letters, adjust="tukey", decreasing = TRUE)

#Produtividade 
x_tipoE<-mean (sqrt.tipoE)
y_tipoE<-sd(sqrt.tipoE)

cv_tipoE <- y_tipoE / x_tipoE *100
cv_tipoE

detach(bloco)


