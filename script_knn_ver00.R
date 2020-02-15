# Carregando pacotes
library(caret)
library(tidyverse)

# Carregando dados limpos
training <-
  read.csv(
    file = 'data/pmp-necropsia-training.csv',
    header = TRUE,
    sep = ',',
  )

testing <-
  read.csv(
    file = 'data/pmp-necropsia-testing.csv',
    header = TRUE,
    sep = ',',
  )

control <- trainControl(
  method = 'repeatedcv', 
  number = 10, 
  repeats = 10
)

knn_fit <- train(
  interacao_tipo ~ ., 
  training[-c(1, 3:6)], 
  method = 'knn', 
  metric = 'Accuracy',
  trControl = control, 
  preProc = c("center", "scale"),
  tuneLength = 20
)

knn_importance <- varImp(knn_fit, scale = FALSE)

knn_pred <- predict(knn_fit, testing)
knn_confusion <- confusionMatrix(knn_pred, testing$interacao_tipo)

plot(knn_importance) # verificar melhor forma de imprimir o gráfico
plot(knn_fit) # verificar melhor forma de imprimir o gráfico

# Salva o modelo obtido
saveRDS(knn_fit, 'models/k_nearest_neighbors.rds')

rm(control, training, testing)
