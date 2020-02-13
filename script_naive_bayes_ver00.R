library(class)
library(gmodels)
library(caret)
library(e1071)

df_train <- df_n[train_indices, ]
df_test <- df_n[-train_indices, ]

train_labels <- df[train_indices, 1]
test_labels <- df[-train_indices, 1]

set.seed(300)

nb <- naiveBayes(df_train, train_labels)

nb_pred <- predict(nb, df_test)

matriz <- table(nb_pred, test_labels)
confusion_nb <- confusionMatrix(matriz)

rm(df_train, df_test, train_labels, test_labels, matriz)
