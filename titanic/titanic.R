library(tidyverse)
library(randomForest)

# carregando os dados
treino = read.csv("train.csv")
teste = read.csv("test.csv")
survived_teste = read.csv("gender_submission.csv")
y = survived_teste$Survived
library(VIM) 
treino_NA = kNN(data = treino,variable = "Age")# tratando valores faltando na variavel Age, usando knn
teste = kNN(data = teste,variable = c("Age","Fare"))

# limpando os dados
summary(treino_NA)
treino_NA$Survived = as.factor(treino_NA$Survived)
treino_NA$Pclass = as.factor(treino_NA$Pclass)
teste$Pclass = as.factor(teste$Pclass)

# Embarked tem valores faltando
treino_NA$Embarked[treino_NA$Embarked == ''] = 'S'
treino_NA$Embarked = as.character(treino_NA$Embarked)
treino_NA$Embarked = as.factor(treino_NA$Embarked)

treino_NA$PassengerId = NULL # o modelo não precisa dessas variaveis
treino_NA$Cabin = NULL
treino_NA$Name = NULL
treino_NA$Ticket = NULL
treino_NA$Age_imp = NULL

teste$PassengerId = NULL # o modelo não precisa dessas variaveis
teste$Cabin = NULL
teste$Name = NULL
teste$Ticket = NULL
teste$Age_imp = NULL
teste$Fare_imp = NULL

# modelo usando randomforest
modelo = randomForest(data=treino_NA, Survived ~ .)

p1 = predict(modelo,teste) # 'prevendo' os valores do teste


