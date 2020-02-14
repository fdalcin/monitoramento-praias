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
  winnow = c(TRUE, FALSE),
  trials = c(2, 4, 8, 12, 14), # nº de iterações
  model = c('tree', 'rules') # modelo
)

dt_fit <- train(
  interacao_tipo ~ ., 
  training[-c(1, 3:6)], 
  method = 'C5.0', 
  metric = 'Accuracy',
  importance = TRUE,
  trControl = control, 
  tuneGrid = grid
)

dt_importance <- varImp(dt_fit, scale = FALSE)

dt_pred <- predict(dt_fit, testing)
dt_confusion <- confusionMatrix(dt_pred, testing$interacao_tipo)

plot(dt_importance) # verificar melhor forma de imprimir o gráfico
plot(dt_fit) # verificar melhor forma de imprimir o gráfico

# Salva o modelo obtido
saveRDS(dt_fit, 'models/decision_tree.rds')

rm(control, grid, training, testing)
