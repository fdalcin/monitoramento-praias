library(class)
library(gmodels)
library(caret)

accuracy <- function(x) {
  sum(diag(x) / (sum(rowSums(x)))) * 100
}

# Carregando dados limpos
df <-
  read.csv(
    file = 'data/pmp-necropsia-training-complete.csv',
    header = TRUE,
    sep = ',',
  )

# Normaliza os dados das colunas com a tabela Z
df_n <- as.data.frame(scale(df[-1]))

# Separa o os dados para treino e testes 
# 70% para treino
# 30% para testes
sample_size <- floor(0.7 * nrow(df))
train_indices <- sample(seq_len(nrow(df)), size = sample_size)

df_train <- df_n[train_indices, ]
df_test <- df_n[-train_indices, ]

train_labels <- df[train_indices, 1]
test_labels <- df[-train_indices, 1]

# Calular o número de vizinhos para o modelo
knn_amount = floor(sqrt(sample_size))
knn_amount1 = knn_amount + 1

# Executa o modelo de KNN
knn1 <- knn(train = df_train, test = df_test, cl = train_labels, k = knn_amount)
knn2 <- knn(train = df_train, test = df_test, cl = train_labels, k = knn_amount1)

# Matriz de confusão
matriz <- table(knn1, test_labels)
confusionMatrix(matriz)
accuracy(matriz)

# Matriz de confusão
matriz <- table(knn2, test_labels)
confusionMatrix(matriz)
accuracy(matriz)

# k_optm = 1
# for (knn_amount in 1:knn_amount1) {
#   knn_mod <- knn(train = df_train, test = df_test, cl = train_labels, k = knn_amount)
#   k_optm[knn_amount] <- accuracy(table(knn_mod, test_labels))
# }
# 
# which.max(k_optm)
