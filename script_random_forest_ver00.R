library(class)
library(gmodels)
library(caret)
library(randomForest)

df_train <- df_n[train_indices, ]
df_test <- df_n[-train_indices, ]

train_labels <- df[train_indices, 1]
test_labels <- df[-train_indices, 1]

set.seed(300)

rf <- randomForest(df_train, train_labels)

rf_pred <- predict(rf, df_test)

matriz <- table(rf_pred, test_labels)
confusion_rf <- confusionMatrix(matriz)

rm(df_train, df_test, train_labels, test_labels, matriz)
