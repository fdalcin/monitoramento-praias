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

svm_fit <- train(
  interacao_tipo ~ ., 
  training[-c(1, 3:6)], 
  method = 'svmPoly', 
  metric = 'Accuracy',
  trControl = control,
  preProc = c("center", "scale")
)

svm_importance <- varImp(svm_fit, scale = FALSE)

svm_pred <- predict(svm_fit, testing)
svm_confusion <- confusionMatrix(svm_pred, testing$interacao_tipo)

plot(svm_importance) # verificar melhor forma de imprimir o gráfico
plot(svm_fit) # verificar melhor forma de imprimir o gráfico

# Salva o modelo obtido
saveRDS(svm_fit, 'models/support_vector_machine.rds')

rm(control, training, testing)
