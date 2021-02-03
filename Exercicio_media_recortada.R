install.packages("tidyverse")
library(tidyverse)
library(tidyr) 


# Importar os dados no formato Data frame 
library(readr)
cadastro <- read_csv("amostra_pf1.csv")
View(cadastro)

# Acessando os elementos no data frame [linha,coluna]
cadastro[2,3]
cadastro[2,]
cadastro[,2]


# tipos de variaveis
str(cadastro)

# Medidas resumo 1
summary(cadastro)



# mudar o formato da variavel numerica para texto
cadastro$CEP = factor(cadastro$CEP)
cadastro$CEP_A = factor(cadastro$CEP_A)
cadastro$LATITUDE = factor(cadastro$LATITUDE)
cadastro$LONGITUDE = factor(cadastro$LONGITUDE)
cadastro$DDD_CELULAR = factor(cadastro$DDD_CELULAR)
cadastro$DDD_CELULAR_2 = factor(cadastro$DDD_CELULAR_2)
cadastro$CNPJ_CREDOR = factor(cadastro$CNPJ_CREDOR)
cadastro$STATUS_CONSENTIMENTO = factor(cadastro$STATUS_CONSENTIMENTO)

# tipos de variaveis
str(cadastro)


# Medidas resumo 
summary(cadastro)


# Recomendado para evitar a necessidade de informar o nome 
# do dataframe$nome da variavel
attach(cadastro)

#summarise 
# Apresenta as estatisticas
summarise(cadastro, media= mean(RENDA_PRESUMIDA),desvio_padrao=sd(RENDA_PRESUMIDA))
cv= sd(RENDA_PRESUMIDA)/mean(RENDA_PRESUMIDA);cv

# presen?a de missing na variavel
summarise(cadastro, media= mean(IDADE),desvio_padrao=sd(IDADE),length(IDADE))
cv= sd(IDADE)/mean(IDADE);cv

summarise(cadastro, media= mean(IDADE, na.rm=TRUE),desvio_padrao=sd(IDADE, na.rm=TRUE),length(IDADE))
cv= sd(IDADE, na.rm=TRUE)/mean(IDADE, na.rm=TRUE);cv


#Graficos
hist(RENDA_PRESUMIDA, xlab="Renda presumida (R$)", ylab="Freq. Absoluta", main="Histograma da renda presumida")

boxplot(RENDA_PRESUMIDA, ylab="Renda presumida (R$)", main="Box Plot da renda presumida")


par(mfrow=c(1,2))
par(mar=c(10,4,8,2))
hist(RENDA_PRESUMIDA, xlab="Renda presumida (R$", ylab="Freq. Absoluta", main="Histograma Renda presumida (R$)")
boxplot(RENDA_PRESUMIDA, ylab="Renda presumida (R$)", main="Box Plot Renda presumida (R$) ")



#Selecionar o conjunto de dados
renda =filter(cadastro,RENDA_PRESUMIDA < 10000) 

attach(renda)

hist(renda$RENDA_PRESUMIDA, xlab="Renda presumida (R$)", ylab="Freq. Absoluta", main="Histograma da renda presumida")

boxplot(renda$RENDA_PRESUMIDA, ylab="Renda presumida (R$)", main="Box Plot da renda presumida")


par(mfrow=c(1,2))
par(mar=c(10,4,8,2))
hist(RENDA_PRESUMIDA, xlab="Renda presumida (R$", ylab="Freq. Absoluta", main="Histograma Renda presumida (R$)")
boxplot(RENDA_PRESUMIDA, ylab="Renda presumida (R$)", main="Box Plot Renda presumida (R$) ")



attach(cadastro)
# Medidas resumo da base completa
summary(RENDA_PRESUMIDA)

#Criar a vari?vel outlier_renda na base de dados
cadastro$outlier_renda =cadastro$RENDA_PRESUMIDA
cadastro$outlier_renda =ifelse(cadastro$outlier_renda< -500,"2",ifelse(cadastro$outlier_renda>=-500&cadastro$outlier_renda<784,"1",ifelse(cadastro$outlier_renda>=784&cadastro$outlier_renda<=4208,"0",ifelse(cadastro$outlier_renda>4208&cadastro$outlier_renda<=5492,"1","2"))))

# Quantos outliers? Quantos pontos extremos?
cadastro$outlier_renda=factor(cadastro$outlier_renda)
table(cadastro$outlier_renda)




attach(cadastro)
#(1) Apresente as medidas resumo da vari?vel renda presumida 
summarise(cadastro, media= mean(RENDA_PRESUMIDA),desvio_padrao=sd(RENDA_PRESUMIDA))
cv= sd(RENDA_PRESUMIDA)/mean(RENDA_PRESUMIDA);cv



# (2) Apresente as medidas resumo da vari?vel renda presumida sem os outliers
#Selecionar o conjunto de dados
renda =filter(cadastro,cadastro$outlier_renda == 0) 
summarise(renda, media= mean(RENDA_PRESUMIDA),desvio_padrao=sd(RENDA_PRESUMIDA))
cv= sd(renda$RENDA_PRESUMIDA)/mean(renda$RENDA_PRESUMIDA);cv


#Apresente o histograma sem os outliers
hist(renda$RENDA_PRESUMIDA)
