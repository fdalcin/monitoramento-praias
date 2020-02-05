library(class)
library(gmodels)

# Carregando dados limpos
df <-
  read.csv(
    file = 'data/pmp-necropsia-training-complete.csv',
    header = TRUE,
    sep = ',',
  )

# Normaliza os dados das colunas com a tabela Z
df_n <- as.data.frame(scale(df[-1]))

sample_size <- floor(0.75 * nrow(df))
train_indices <- sample(seq_len(nrow(df)), size = sample_size)

df_train <- df_n[train_indices, ]
df_test <- df_n[-train_indices, ]

df_train_labels <- df[train_indices, 1]
df_test_labels <- df[-train_indices, 1]

df_test_pred <- knn(train = df_train, test = df_test, cl = df_train_labels, k = 21)

CrossTable(x = df_test_labels, y = df_test_pred, prop.chisq = FALSE)
