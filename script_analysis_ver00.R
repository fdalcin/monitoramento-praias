correlation_columns = c(
  'condicao_da_carcaca', 'necropsia_imediata', 'escore_corporal', 'interacao_tipo', 'presenca_de_residuos_solidos', 
  'diagnostico_presuntivo_causa', 'diagnostico_presuntivo_lesao_principal_orgao', 'diagnostico_presuntivo_lesao_principal_causa', 'diagnostico_presuntivo_lesaes_secundarias_orgao_1', 'diagnostico_presuntivo_lesaes_secundarias_causa_1', 'diagnostico_presuntivo_lesaes_secundarias_orgao_2', 'diagnostico_presuntivo_lesaes_secundarias_causa_2', 'diagnostico_presuntivo_motivo',
  'diagnostico_primario_causa', 'diagnostico_primario_lesao_principal_orgao', 'diagnostico_primario_lesao_principal_causa', 'diagnostico_primario_lesaes_secundarias_orgao_1', 'diagnostico_primario_lesaes_secundarias_causa_1', 'diagnostico_primario_lesaes_secundarias_orgao_2', 'diagnostico_primario_lesaes_secundarias_causa_2', 'diagnostico_primario_motivo',
  'diagnostico_contributivo_causa', 'diagnostico_contributivo_lesao_principal_orgao', 'diagnostico_contributivo_lesao_principal_causa', 'diagnostico_contributivo_lesaes_secundarias_orgao_1', 'diagnostico_contributivo_lesaes_secundarias_causa_1', 'diagnostico_contributivo_lesaes_secundarias_orgao_2', 'diagnostico_contributivo_lesaes_secundarias_causa_2', 'diagnostico_contributivo_motivo',
  'cc_status', 'tcs_status', 'sme_status', 'sres_status', 'sc_status', 'ad_status', 'su_status', 'srep_status', 'slh_status', 'se_status', 'os_status', 'snc_status'
)

df_correlation <- select(df_training, correlation_columns)
df_correlation[correlation_columns] <- lapply(df_correlation[correlation_columns], as.numeric)

correlation <- round(cor(df_correlation), 2)

interacao_cc <- df_training %>% group_by(interacao_tipo, cc_status) %>% summarise(total = n())
interacao_tcs <- df_training %>% group_by(interacao_tipo, tcs_status) %>% summarise(total = n())
interacao_sme <- df_training %>% group_by(interacao_tipo, sme_status) %>% summarise(total = n())
interacao_sres <- df_training %>% group_by(interacao_tipo, sres_status) %>% summarise(total = n())
interacao_sc <- df_training %>% group_by(interacao_tipo, sc_status) %>% summarise(total = n())
interacao_ad <- df_training %>% group_by(interacao_tipo, ad_status) %>% summarise(total = n())
interacao_su <- df_training %>% group_by(interacao_tipo, su_status) %>% summarise(total = n())
interacao_srep <- df_training %>% group_by(interacao_tipo, srep_status) %>% summarise(total = n())
interacao_slh <- df_training %>% group_by(interacao_tipo, slh_status) %>% summarise(total = n())
interacao_se <- df_training %>% group_by(interacao_tipo, se_status) %>% summarise(total = n())
interacao_os <- df_training %>% group_by(interacao_tipo, os_status) %>% summarise(total = n())
interacao_snc <- df_training %>% group_by(interacao_tipo, snc_status) %>% summarise(total = n())

residuos_primario_causa <- df_training %>%
  group_by(presenca_de_residuos_solidos, diagnostico_primario_causa) %>%
  summarise(total = n())

residuos_presuntivo_causa <- df_training %>%
  group_by(presenca_de_residuos_solidos, diagnostico_presuntivo_causa) %>%
  summarise(total = n())

residuos_contributivo_causa <- df_training %>%
  group_by(presenca_de_residuos_solidos, diagnostico_contributivo_causa) %>%
  summarise(total = n())

residuos_primario_orgao <- df_training %>%
  group_by(presenca_de_residuos_solidos, diagnostico_primario_lesao_principal_causa, diagnostico_primario_lesao_principal_orgao) %>%
  summarise(total = n())

residuos_presuntivo_orgao <- df_training %>%
  group_by(presenca_de_residuos_solidos, diagnostico_presuntivo_lesao_principal_causa, diagnostico_presuntivo_lesao_principal_orgao) %>%
  summarise(total = n())

residuos_contributivo_orgao <- df_training %>%
  group_by(presenca_de_residuos_solidos, diagnostico_contributivo_lesao_principal_causa, diagnostico_contributivo_lesao_principal_orgao) %>%
  summarise(total = n())

interacao_d_primario_causa <- df_training %>%
  group_by(interacao_tipo, diagnostico_primario_causa) %>%
  summarise(total = n()) # %>%
  # filter(diagnostico_primario_causa != 'indeterminada') %>%
  # filter(diagnostico_primario_causa != '')

interacao_d_presuntivo_causa <- df_training %>%
  group_by(interacao_tipo, diagnostico_presuntivo_causa) %>%
  summarise(total = n()) # %>%
  # filter(diagnostico_presuntivo_causa != 'indeterminada') %>%
  # filter(diagnostico_presuntivo_causa != '')

interacao_d_contributivo_causa <- df_training %>%
  group_by(interacao_tipo, diagnostico_contributivo_causa) %>%
  summarise(total = n()) # %>%
  # filter(diagnostico_contributivo_causa != 'indeterminada') %>%
  # filter(diagnostico_contributivo_causa != '')

interacao_d_primario_orgao <- df_training %>%
  group_by(interacao_tipo, diagnostico_primario_lesao_principal_causa, diagnostico_primario_lesao_principal_orgao) %>%
  summarise(total = n())

interacao_d_presuntivo_orgao <- df_training %>%
  group_by(interacao_tipo, diagnostico_presuntivo_lesao_principal_causa, diagnostico_presuntivo_lesao_principal_orgao) %>%
  summarise(total = n())

interacao_d_contributivo_orgao <- df_training %>%
  group_by(interacao_tipo, diagnostico_contributivo_lesao_principal_causa, diagnostico_contributivo_lesao_principal_orgao) %>%
  summarise(total = n())

interacao_d_primario <- df_training %>%
  group_by(interacao_tipo, diagnostico_primario_causa, diagnostico_primario_lesao_principal_causa) %>%
  summarise(total = n())

interacao_d_presuntivo <- df_training %>%
  group_by(interacao_tipo, diagnostico_presuntivo_causa, diagnostico_presuntivo_lesao_principal_causa) %>%
  summarise(total = n())

interacao_d_contributivo <- df_training %>%
  group_by(interacao_tipo, diagnostico_contributivo_causa, diagnostico_contributivo_lesao_principal_causa) %>%
  summarise(total = n())

# Removendo variáveis para liberar memória
rm(
  correlation_columns
)
