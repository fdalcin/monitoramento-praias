# Insights ?

cc_interacao <- base_treino %>% group_by(interacao_tipo, cc_status) %>% summarise(total = n())
tcs_interacao <- base_treino %>% group_by(interacao_tipo, tcs_status) %>% summarise(total = n())
sme_interacao <- base_treino %>% group_by(interacao_tipo, sme_status) %>% summarise(total = n())
sres_interacao <- base_treino %>% group_by(interacao_tipo, sres_status) %>% summarise(total = n())
sc_interacao <- base_treino %>% group_by(interacao_tipo, sc_status) %>% summarise(total = n())
ad_interacao <- base_treino %>% group_by(interacao_tipo, ad_status) %>% summarise(total = n())
su_interacao <- base_treino %>% group_by(interacao_tipo, su_status) %>% summarise(total = n())
srep_interacao <- base_treino %>% group_by(interacao_tipo, srep_status) %>% summarise(total = n())
slh_interacao <- base_treino %>% group_by(interacao_tipo, slh_status) %>% summarise(total = n())
se_interacao <- base_treino %>% group_by(interacao_tipo, se_status) %>% summarise(total = n())
os_interacao <- base_treino %>% group_by(interacao_tipo, os_status) %>% summarise(total = n())
snc_interacao <- base_treino %>% group_by(interacao_tipo, snc_status) %>% summarise(total = n())

interacao_orgaos <- base_treino %>% group_by(
  interacao_tipo,
  cc_status,
  tcs_status,
  sme_status,
  sres_status,
  sc_status,
  ad_status,
  su_status,
  srep_status,
  slh_status,
  se_status,
  os_status,
  snc_status
) %>% summarise(total = n())

primario_indicios <- base_treino %>% 
  group_by(ha_indicios_de_interacao_antropica, diagnostico_primario_causa) %>% 
  summarise(total = n())

presuntivo_indicios <- base_treino %>% 
  group_by(ha_indicios_de_interacao_antropica, diagnostico_presuntivo_causa) %>% 
  summarise(total = n())

contributivo_indicios <- base_treino %>% 
  group_by(ha_indicios_de_interacao_antropica, diagnostico_contributivo_causa) %>% 
  summarise(total = n())

indicios_diagnosticos <- base_treino %>% group_by(
  ha_indicios_de_interacao_antropica,
  diagnostico_primario_causa,
  diagnostico_presuntivo_causa,
  diagnostico_contributivo_causa,
) %>% 
  summarise(total = n())

primario_interacao <- base_treino %>% 
  group_by(interacao_tipo, diagnostico_primario_causa) %>% 
  summarise(total = n()) # %>% 
  # filter(diagnostico_primario_causa != 'indeterminada') %>% 
  # filter(diagnostico_primario_causa != '')

presuntivo_interacao <- base_treino %>% 
  group_by(interacao_tipo, diagnostico_presuntivo_causa) %>%
  summarise(total = n()) # %>%
  # filter(diagnostico_presuntivo_causa != 'indeterminada') %>% 
  # filter(diagnostico_presuntivo_causa != '')

contributivo_interacao <- base_treino %>% 
  group_by(interacao_tipo, diagnostico_contributivo_causa) %>% 
  summarise(total = n()) # %>% 
  # filter(diagnostico_contributivo_causa != 'indeterminada') %>% 
  # filter(diagnostico_contributivo_causa != '')

interacao_diagnosticos <- base_treino %>% group_by(
  interacao_tipo,
  diagnostico_primario_causa,
  diagnostico_presuntivo_causa,
  diagnostico_contributivo_causa,
) %>% 
  summarise(total = n()) # %>% 
  # filter(diagnostico_primario_causa != 'indeterminada') %>% 
  # filter(diagnostico_primario_causa != '') %>% 
  # filter(diagnostico_presuntivo_causa != 'indeterminada') %>% 
  # filter(diagnostico_presuntivo_causa != '') %>% 
  # filter(diagnostico_contributivo_causa != 'indeterminada') %>% 
  # filter(diagnostico_contributivo_causa != '')

# Base treino e base teste
# utilizar apenas colunas definidas