# Carregando pacotes
library(tidyverse)
library(DataExplorer)
library(ggcorrplot)

# Carregando dados limpos
df <-
  read.csv(
    file = 'data/pmp-necropsia-full.csv',
    header = TRUE,
    sep = ',',
  )

# Colunas para verificar a correlação
correlation_columns = c(
  'condicao_da_carcaca',
  'necropsia_imediata',
  'escore_corporal',
  'interacao_tipo',
  'presenca_de_residuos_solidos',
  'diagnostico_presuntivo_causa',
  'diagnostico_presuntivo_lesao_principal_orgao',
  'diagnostico_presuntivo_lesao_principal_causa',
  'diagnostico_presuntivo_lesaes_secundarias_orgao_1',
  'diagnostico_presuntivo_lesaes_secundarias_causa_1',
  'diagnostico_presuntivo_lesaes_secundarias_orgao_2',
  'diagnostico_presuntivo_lesaes_secundarias_causa_2',
  'diagnostico_presuntivo_motivo',
  'diagnostico_primario_causa',
  'diagnostico_primario_lesao_principal_orgao',
  'diagnostico_primario_lesao_principal_causa',
  'diagnostico_primario_lesaes_secundarias_orgao_1',
  'diagnostico_primario_lesaes_secundarias_causa_1',
  'diagnostico_primario_lesaes_secundarias_orgao_2',
  'diagnostico_primario_lesaes_secundarias_causa_2',
  'diagnostico_primario_motivo',
  'diagnostico_contributivo_causa',
  'diagnostico_contributivo_lesao_principal_orgao',
  'diagnostico_contributivo_lesao_principal_causa',
  'diagnostico_contributivo_lesaes_secundarias_orgao_1',
  'diagnostico_contributivo_lesaes_secundarias_causa_1',
  'diagnostico_contributivo_lesaes_secundarias_orgao_2',
  'diagnostico_contributivo_lesaes_secundarias_causa_2',
  'diagnostico_contributivo_motivo',
  'cc_status',
  'tcs_status',
  'sme_status',
  'sres_status',
  'sc_status',
  'ad_status',
  'su_status',
  'srep_status',
  'slh_status',
  'se_status',
  'os_status',
  'snc_status'
)

# Calculando correlação dos resultados
corr_full <- select(df, correlation_columns)

# Preenche colunas com '' para NA
for(i in correlation_columns) {
  corr_full[, i] <- ifelse(
    corr_full[, i] == '', NA, corr_full[, i]
  )
}

# Correlação das colunas para o modelo
correlation <- round(cor(corr_full), 3)

# Imprime a correlação de todas as colunas selecionadas
ggcorrplot(
  correlation, 
  outline.col = 'white',
  colors = c('#c53030', '#e2e8f0', '#61045f'),
  legend.title = 'Correlação'
)

plot_missing(corr_full)

# Seleciona apenas colunas com no máximo 10% de dados faltantes
corr_full <- corr_full[, c(1:8, 14:16, 30:41)]

# Seleciona somente registros com dados completos
corr_complete <- corr_full[complete.cases(corr_full),]

# Correlação das colunas para o modelo
correlation <- round(cor(corr_complete), 3)

# Imprime a correlação das colunas com dados completos
ggcorrplot(
  correlation, 
  method = 'circle',
  hc.order = TRUE, 
  type = 'lower',
  outline.col = 'white',
  colors = c('#c53030', '#e2e8f0', '#61045f'),
  legend.title = 'Correlação'
)

missing <- filter(corr_full, !is.na(interacao_tipo))
plot_missing(missing)

# Cria um dataframe com a coluna de classificação e todas as colunas que serão utilizadas no modelo
df_training <- data.frame(
  df[, c(1, 5, 44:47)], 
  corr_full[, c(1:3, 5:23)]
)

# Salva em um novo arquivo
write.csv(
  df_training, 
  'data/pmp-necropsia-analysis-full.csv', 
  row.names = FALSE
)

# Removendo variáveis para liberar memória
rm(
  df,
  df_training,
  correlation,
  correlation_columns,
  corr_complete,
  corr_full,
  missing,
  i
)
# interacao_cc <- df_training %>% 
#   group_by(interacao_tipo, cc_status) %>% 
#   summarise(total = n())
# 
# interacao_tcs <- df_training %>%
#   group_by(interacao_tipo, tcs_status) %>% 
#   summarise(total = n())
# 
# interacao_sme <- df_training %>%
#   group_by(interacao_tipo, sme_status) %>%
#   summarise(total = n())
# 
# interacao_sres <- df_training %>%
#   group_by(interacao_tipo, sres_status) %>%
#   summarise(total = n())
# 
# interacao_sc <- df_training %>%
#   group_by(interacao_tipo, sc_status) %>%
#   summarise(total = n())
# 
# interacao_ad <- df_training %>%
#   group_by(interacao_tipo, ad_status) %>%
#   summarise(total = n())
# 
# interacao_su <- df_training %>%
#   group_by(interacao_tipo, su_status) %>%
#   summarise(total = n())
# 
# interacao_srep <- df_training %>%
#   group_by(interacao_tipo, srep_status) %>%
#   summarise(total = n())
# 
# interacao_slh <- df_training %>%
#   group_by(interacao_tipo, slh_status) %>%
#   summarise(total = n())
# 
# interacao_se <- df_training %>%
#   group_by(interacao_tipo, se_status) %>%
#   summarise(total = n())
# 
# interacao_os <- df_training %>%
#   group_by(interacao_tipo, os_status) %>%
#   summarise(total = n())
# 
# interacao_snc <- df_training %>%
#   group_by(interacao_tipo, snc_status) %>%
#   summarise(total = n())
# 
# interacao_d_primario_causa <- df_training %>%
#   group_by(interacao_tipo, diagnostico_primario_causa) %>%
#   summarise(total = n())
# 
# interacao_d_presuntivo_causa <- df_training %>%
#   group_by(interacao_tipo, diagnostico_presuntivo_causa) %>%
#   summarise(total = n())
# 
# interacao_d_presuntivo_orgao <- df_training %>%
#   group_by(
#     interacao_tipo,
#     diagnostico_presuntivo_lesao_principal_causa,
#     diagnostico_presuntivo_lesao_principal_orgao
#   ) %>%
#   summarise(total = n())
# 
# interacao_d_presuntivo <- df_training %>%
#   group_by(
#     interacao_tipo,
#     diagnostico_presuntivo_causa,
#     diagnostico_presuntivo_lesao_principal_causa
#   ) %>%
#   summarise(total = n())
# 
# # Removendo variáveis para liberar memória
# rm(correlation_columns, i, corr_full, corr_complete, correlation, df)
