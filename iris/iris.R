data(iris) # carregando o pacote
require(class) # pacote contendo o algoritimo de knn
require(ggplot2) # vizualização
# head(iris)
# plot(iris$Sepal.Length,iris$Petal.Length,col=iris$Species)
qplot(x = iris$Sepal.Length,y = iris$Petal.Length,col=iris$Species,xlab = "Comprimento da Sepala",ylab = "Comprimento da Pétala",main = "Iris",margins = T)
# normalize <- function(x) { 
#   return ((x - min(x)) / (max(x) - min(x))) 
# }

# quantas amostras
amostra = floor(0.70*nrow(iris)) # obtendo 70% dos dados originais para treinamento do modelo

# amostra aleatória
train_ind = sample(x = nrow(iris),size = amostra,replace = F) # coletando uma amostra de tamanho igual a 70% dos dados

# dividindo os dados entre treino e teste, utilizando o índice
treino = iris[train_ind,1:4] # aplicando uma função para normalizar dos dados, para que todos fiquem numa mesma 'escala'. O modelo somente utiliza variáveis numéricas então pegamos somente as colunas de 1 até 4
teste = iris[-train_ind,1:4]
treino.classe = iris[train_ind,5] # recebe somente o 'label' que é a classe a qual queremos estimar o resultado
teste.classe = iris[-train_ind,5]

#criando o modelo
modelo = knn(train = treino,test = teste,k = floor(sqrt(nrow(iris))),cl = treino.classe,prob = T) # o modelo recebe os dados de treino e tenta estimar os de teste, utilizando os 'targets' que são os fatores de classificação

# avaliando o modelo
matriz = as.matrix(table(teste.classe,modelo)) # matriz de confusão do modelo com os valores reais
precisao = sum(diag(matriz))/length(teste.classe) # soma das diagonais da matriz dividido pelo total de elementos
print(matriz);print(c('Precisão:',precisao))

