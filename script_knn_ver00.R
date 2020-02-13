library(class)
library(gmodels)
library(caret)

df_train <- df_n[train_indices, ]
df_test <- df_n[-train_indices, ]

train_labels <- df[train_indices, 1]
test_labels <- df[-train_indices, 1]

set.seed(300)

# Calular o número de vizinhos para o modelo
knn_amount = floor(sqrt(sample_size))
knn_amount1 = knn_amount + 1

# Executa o modelo de KNN
knn1 <- knn(train = df_train, test = df_test, cl = train_labels, k = knn_amount)
knn2 <- knn(train = df_train, test = df_test, cl = train_labels, k = knn_amount1)

# Matriz de confusão
matriz <- table(knn1, test_labels)
confusion_knn1 <- confusionMatrix(matriz)
accuracy(matriz)

# Matriz de confusão
matriz <- table(knn2, test_labels)
confusion_knn2 <- confusionMatrix(matriz)
accuracy(matriz)

# k_optm = 1
# for (knn_amount in 1:knn_amount1) {
#   knn_mod <- knn(train = df_train, test = df_test, cl = train_labels, k = knn_amount)
#   k_optm[knn_amount] <- accuracy(table(knn_mod, test_labels))
# }
# 
# which.max(k_optm)

rm(knn_amount, knn_amount1, df_train, df_test, train_labels, test_labels, matriz)
