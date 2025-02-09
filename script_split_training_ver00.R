# Carregando pacotes
library(tidyverse)

# # Definições para gerar os gráficos
# seta <- grid::arrow(length = grid::unit(0.2, 'cm'), type = 'open')
# 
# default_theme <- function (base_size = 14, base_family = 'Arial') {
#   theme_bw(base_size = base_size, base_family = base_family) %+replace%
#     theme(axis.ticks = element_blank(),
#           axis.line = element_line(arrow = seta, color = 'gray20'),
#           legend.background = element_blank(),
#           legend.key = element_blank(),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           strip.background = element_blank(),
#           plot.background = element_blank(),
#           plot.title = element_text(hjust = 1),
#           complete = TRUE)
# }

# Carregando dados limpos
df <-
  read.csv(
    file = 'data/pmp-necropsia-analysis-full.csv',
    header = TRUE,
    sep = ',',
  )

# Normaliza os dados das colunas com a tabela Z
df[,7:28] <- as.data.frame(scale(df[,7:28]))

  # Base para treino e validação
df_training <- filter(df, !is.na(interacao_tipo))
df_training <- df_training[complete.cases(df_training),]

# Base para inferência final
df_final <- filter(df, is.na(interacao_tipo))

# Separa o os dados para treino e testes 
# 70% para treino
# 30% para testes
train_index <- createDataPartition(df_training$interacao_tipo, p = .7, list = FALSE)

df_train <- df_training[train_index, ]
df_test <- df_training[-train_index, ]

# # Preprocessamento e algumas análises
# preprocessing <- preProcess(
#   select(df_training, -c(1:7)), 
#   method = c("center", "scale", "YeoJohnson", "nzv", "pca")
# )

# # Verifica melhores variáveis
# control <- rfeControl(functions = rfFuncs, method = 'cv', number = 10)
# # Utiliza variáveis de 8:29 para verificar classificação da variável 2 com 2:20 variáveis
# feature_selecion <- rfe(df_training[,8:29], df_training[,2], sizes = c(2:20), rfeControl = control)
# 
# print(feature_selecion)
# predictors(feature_selecion)
# 
# ggplot(feature_selecion) +
#   geom_line(size = 0.5, color = 'gray60') +
#   geom_point(size = 1.5, color = 'gray30') +
#   labs(x = 'Nº de variaveis', y = 'Acurácia') + 
#   default_theme()

write.csv(
  df_train, 
  'data/pmp-necropsia-training.csv', 
  row.names = FALSE
)

write.csv(
  df_test,
  'data/pmp-necropsia-testing.csv',
  row.names = FALSE
)

write.csv(
  df_final,
  'data/pmp-necropsia-final.csv',
  row.names = FALSE
)

rm(df, df_final, df_training, df_train, df_test, train_index, control)
