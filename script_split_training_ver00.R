accuracy <- function(x) {
  sum(diag(x) / (sum(rowSums(x)))) * 100
}

# Carregando dados limpos
df <-
  read.csv(
    file = 'data/pmp-necropsia-training-complete.csv',
    header = TRUE,
    sep = ',',
  )

# Normaliza os dados das colunas com a tabela Z
df_n <- as.data.frame(scale(df[-1]))

set.seed(300)
# Separa o os dados para treino e testes 
# 70% para treino
# 30% para testes
sample_size <- floor(0.7 * nrow(df))
train_indices <- sample(seq_len(nrow(df)), size = sample_size)

# control <- rfeControl(functions=rfFuncs, method = 'cv', number = 10)
# results <- rfe(df_n[,1:20], df[,1], sizes = c(1:20), rfeControl = control)
