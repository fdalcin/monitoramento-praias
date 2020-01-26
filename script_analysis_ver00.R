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

# 
# cc_interacao <- base_treino %>% group_by(interacao_tipo, cc_status) %>% summarise(total = n())
# tcs_interacao <- base_treino %>% group_by(interacao_tipo, tcs_status) %>% summarise(total = n())
# sme_interacao <- base_treino %>% group_by(interacao_tipo, sme_status) %>% summarise(total = n())
# sres_interacao <- base_treino %>% group_by(interacao_tipo, sres_status) %>% summarise(total = n())
# sc_interacao <- base_treino %>% group_by(interacao_tipo, sc_status) %>% summarise(total = n())
# ad_interacao <- base_treino %>% group_by(interacao_tipo, ad_status) %>% summarise(total = n())
# su_interacao <- base_treino %>% group_by(interacao_tipo, su_status) %>% summarise(total = n())
# srep_interacao <- base_treino %>% group_by(interacao_tipo, srep_status) %>% summarise(total = n())
# slh_interacao <- base_treino %>% group_by(interacao_tipo, slh_status) %>% summarise(total = n())
# se_interacao <- base_treino %>% group_by(interacao_tipo, se_status) %>% summarise(total = n())
# os_interacao <- base_treino %>% group_by(interacao_tipo, os_status) %>% summarise(total = n())
# snc_interacao <- base_treino %>% group_by(interacao_tipo, snc_status) %>% summarise(total = n())
# 
# interacao_orgaos <- base_treino %>% group_by(
#   interacao_tipo,
#   cc_status,
#   tcs_status,
#   sme_status,
#   sres_status,
#   sc_status,
#   ad_status,
#   su_status,
#   srep_status,
#   slh_status,
#   se_status,
#   os_status,
#   snc_status
# ) %>% summarise(total = n())
# 
# primario_indicios <- base_treino %>% 
#   group_by(ha_indicios_de_interacao_antropica, diagnostico_primario_causa) %>% 
#   summarise(total = n())
# 
# presuntivo_indicios <- base_treino %>% 
#   group_by(ha_indicios_de_interacao_antropica, diagnostico_presuntivo_causa) %>% 
#   summarise(total = n())
# 
# contributivo_indicios <- base_treino %>% 
#   group_by(ha_indicios_de_interacao_antropica, diagnostico_contributivo_causa) %>% 
#   summarise(total = n())
# 
# indicios_diagnosticos <- base_treino %>% group_by(
#   ha_indicios_de_interacao_antropica,
#   diagnostico_primario_causa,
#   diagnostico_presuntivo_causa,
#   diagnostico_contributivo_causa,
# ) %>% 
#   summarise(total = n())
# 
# primario_interacao <- base_treino %>% 
#   group_by(interacao_tipo, diagnostico_primario_causa) %>% 
#   summarise(total = n()) # %>% 
#   # filter(diagnostico_primario_causa != 'indeterminada') %>% 
#   # filter(diagnostico_primario_causa != '')
# 
# presuntivo_interacao <- base_treino %>% 
#   group_by(interacao_tipo, diagnostico_presuntivo_causa) %>%
#   summarise(total = n()) # %>%
#   # filter(diagnostico_presuntivo_causa != 'indeterminada') %>% 
#   # filter(diagnostico_presuntivo_causa != '')
# 
# contributivo_interacao <- base_treino %>% 
#   group_by(interacao_tipo, diagnostico_contributivo_causa) %>% 
#   summarise(total = n()) # %>% 
#   # filter(diagnostico_contributivo_causa != 'indeterminada') %>% 
#   # filter(diagnostico_contributivo_causa != '')
# 
# interacao_diagnosticos <- base_treino %>% group_by(
#   interacao_tipo,
#   diagnostico_primario_causa,
#   diagnostico_presuntivo_causa,
#   diagnostico_contributivo_causa,
# ) %>% 
#   summarise(total = n()) # %>% 
#   # filter(diagnostico_primario_causa != 'indeterminada') %>% 
#   # filter(diagnostico_primario_causa != '') %>% 
#   # filter(diagnostico_presuntivo_causa != 'indeterminada') %>% 
#   # filter(diagnostico_presuntivo_causa != '') %>% 
#   # filter(diagnostico_contributivo_causa != 'indeterminada') %>% 
#   # filter(diagnostico_contributivo_causa != '')
# 
# # Base treino e base teste
# utilizar apenas colunas defini

das