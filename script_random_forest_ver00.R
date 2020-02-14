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

grid <- expand.grid(
  mtry = c(2, 4, 8, 16, 20), # nº de variáveis selecionada aleatoriamente
  splitrule = c('gini', 'extratrees'), # regra de split
  min.node.size = c(1, 3, 5) # tamanho mínimo de nós
)

rf_fit <- train(
  interacao_tipo ~ ., 
  training[-c(1, 3:6)], 
  method = 'ranger', 
  metric = 'Accuracy',
  importance = 'impurity',
  trControl = control, 
  tuneGrid = grid
)

rf_importance <- varImp(rf_fit, scale = FALSE)

rf_pred <- predict(rf_fit, testing)
rf_confusion <- confusionMatrix(rf_pred, testing$interacao_tipo)

plot(rf_importance) # verificar melhor forma de imprimir o gráfico
plot(rf_fit) # verificar melhor forma de imprimir o gráfico

# Salva o modelo obtido
saveRDS(rf_fit, 'models/random_forest.rds')

rm(control, grid,training, testing)
