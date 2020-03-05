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
  usekernel = TRUE,
  laplace = c(0:1),
  adjust = c(1:3)
)

system.time(
  nb_fit <- train(
    interacao_tipo ~ ., 
    training[-c(1, 3:6)], 
    method = 'naive_bayes', 
    metric = 'Accuracy',
    importance = TRUE,
    trControl = control, 
    tuneGrid = grid
  )
)

nb_importance <- varImp(nb_fit, scale = FALSE)

nb_pred <- predict(nb_fit, testing)
nb_confusion <- confusionMatrix(nb_pred, testing$interacao_tipo)

plot(nb_importance) # verificar melhor forma de imprimir o gráfico
plot(nb_fit) # verificar melhor forma de imprimir o gráfico

# Salva o modelo obtido
saveRDS(nb_fit, 'models/naive_bayes.rds')

rm(control, grid, training, testing)
