library(class)
library(gmodels)
library(caret)
library(C50)

df_train <- df_n[train_indices, ]
df_test <- df_n[-train_indices, ]

train_labels <- df[train_indices, 1]
test_labels <- df[-train_indices, 1]

set.seed(300)

dt <- C5.0(df_train, train_labels, trials = 30, control = C5.0Control(earlyStopping = FALSE))

dt_pred <- predict(dt, df_test)

matriz <- table(dt_pred, test_labels)
confusion_dt <- confusionMatrix(matriz)

rm(df_train, df_test, train_labels, test_labels, matriz)
