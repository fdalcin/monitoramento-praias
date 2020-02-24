# Carregando pacotes
library(tidyverse)
library(janitor)
library(stringi)
library(stringr)
library(tm)

# Carregando dataframe a partir do arquivo CSV
df <-
  read.csv(
    file = 'data/pmp-necropsia.csv',
    header = TRUE,
    sep = ';',
    dec = ','
  )

# Transformando nomes das colunas em snake_case
df <- df %>% clean_names('snake')

# Colunas desnecessárias a serem removidas
unnecessary_columns = c(
  'identificador_da_ocorrencia',
  'identificador_do_individuo',
  'ficha_de_campo',
  'data_do_obito',
  'participantes',
  'peso_no_momento_da_necropsia',
  'peso',
  'metodo',
  'local_da_necropsia',
  'historico',
  'observacoes',
  'fotos',
  'motivo_da_nao_coleta_conteudo_gastrointestinal',
  'amostras_para_historia_natural_dentes',
  'amostras_para_historia_natural_gonadas',
  'amostras_para_historia_natural_umero_falange',
  'destinacao_da_carcaca',
  'data',
  'local',
  'ponto_lat',
  'ponto_long',
  'fotos_1',
  'arquivos',
  'diagnostico_presuntivo_observaoes',
  'diagnostico_primario_observaoes',
  'diagnostico_contributivo_observaoes',
  'relatorio_administrativo',
  'mesorregiao',
  'habitat',
  'local_trecho_id',
  'local_trecho_nome',
  'local_praia_id',
  'local_praia_nome',
  'local_cidades_i_ds',
  'local_cidades_nomes',
  'local_estados_i_ds',
  'local_estados_nomes',
  'local_tipo',
  'local_estrategia',
  'local_metros',
  'pmp'
)

# Colunas a serem utilizadas no modelo
model_columns = c(
  'codigo',
  'condicao_da_carcaca',
  'necropsia_imediata',
  'escore_corporal',
  'interacao_tipo',
  'interacao_nivel',
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
  'snc_status',
  'necropsia_sexo',
  'necropsia_estagio_de_desenvolvimento',
  'necropsia_classe_do_individuo',
  'necropsia_especie_do_individuo'
)

# Removendo colunas desnecessárias do dataframe
df <- select(df,-unnecessary_columns)

stopWords <- stopwords('pt-BR')
regexStatus <- 'status: ([^,]*)(.*)'
regexMotivo <- ', (motivo: (.*)|(.*))'
regexTipo <- 'tipo: ([^,]*)(.*)'
regexNivel <- ', nivel: (.*)|(.*)'
regexPontuacao <- '[[:punct:]]'
regexEspacosExtras <- '\\s+'
encoding <- 'Latin-ASCII'

# Adicionando coluna indicativa do número de interacoes_antropicas
df$quantidade_interacoes <-
  ifelse(
    grepl(';', df$interacoes_antropicas),
    'multiple',
    ifelse(df$interacoes_antropicas == '', 'none', 'one')
  )

df <- filter(df, quantidade_interacoes != 'multiple')

# Transformar valores das principais colunas para lowercase e remover acentos
df <- df %>% 
  mutate(
    instituicao_executora = stri_trans_general(tolower(instituicao_executora), encoding)
  )

df <- df %>% 
  mutate(
    necropsia_imediata = stri_trans_general(tolower(necropsia_imediata), encoding)
  )

df <- df %>% 
  mutate(
    escore_corporal = stri_trans_general(tolower(escore_corporal), encoding)
  )

df <- df %>% 
  mutate(
    presenca_de_residuos_solidos = stri_trans_general(tolower(presenca_de_residuos_solidos), encoding)
  )

df <- df %>% 
  mutate(
    interacoes_antropicas = stri_trans_general(tolower(interacoes_antropicas), encoding)
  )

# diagnostico_presuntivo
df <- df %>% 
  mutate(
    diagnostico_presuntivo_causa = stri_trans_general(tolower(diagnostico_presuntivo_causa), encoding)
  )

df <- df %>% 
  mutate(
    diagnostico_presuntivo_lesao_principal_orgao = stri_trans_general(
      tolower(diagnostico_presuntivo_lesao_principal_orgao),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_presuntivo_lesao_principal_causa = stri_trans_general(
      tolower(diagnostico_presuntivo_lesao_principal_causa),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_presuntivo_lesaes_secundarias_orgao_1 = stri_trans_general(
      tolower(diagnostico_presuntivo_lesaes_secundarias_orgao_1),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_presuntivo_lesaes_secundarias_causa_1 = stri_trans_general(
      tolower(diagnostico_presuntivo_lesaes_secundarias_causa_1),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_presuntivo_lesaes_secundarias_orgao_2 = stri_trans_general(
      tolower(diagnostico_presuntivo_lesaes_secundarias_orgao_2),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_presuntivo_lesaes_secundarias_causa_2 = stri_trans_general(
      tolower(diagnostico_presuntivo_lesaes_secundarias_causa_2),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_presuntivo_motivo = stri_trans_general(tolower(diagnostico_presuntivo_motivo), encoding)
  )

# diagnostico_primario
df <- df %>% 
  mutate(
    diagnostico_primario_causa = stri_trans_general(tolower(diagnostico_primario_causa), encoding)
  )

df <- df %>% 
  mutate(
    diagnostico_primario_lesao_principal_orgao = stri_trans_general(
      tolower(diagnostico_primario_lesao_principal_orgao),
      encoding
    )
  )

df <- df %>% 
  mutate(diagnostico_primario_lesao_principal_causa = stri_trans_general(
      tolower(diagnostico_primario_lesao_principal_causa),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_primario_lesaes_secundarias_orgao_1 = stri_trans_general(
      tolower(diagnostico_primario_lesaes_secundarias_orgao_1),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_primario_lesaes_secundarias_causa_1 = stri_trans_general(
      tolower(diagnostico_primario_lesaes_secundarias_causa_1),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_primario_lesaes_secundarias_orgao_2 = stri_trans_general(
      tolower(diagnostico_primario_lesaes_secundarias_orgao_2),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_primario_lesaes_secundarias_causa_2 = stri_trans_general(
      tolower(diagnostico_primario_lesaes_secundarias_causa_2),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_primario_motivo = stri_trans_general(tolower(diagnostico_primario_motivo), encoding)
  )

# diagnostico_contributivo
df <- df %>% 
  mutate(
    diagnostico_contributivo_causa = stri_trans_general(tolower(diagnostico_contributivo_causa), encoding)
  )

df <- df %>% 
  mutate(
    diagnostico_contributivo_lesao_principal_orgao = stri_trans_general(
      tolower(diagnostico_contributivo_lesao_principal_orgao),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_contributivo_lesao_principal_causa = stri_trans_general(
      tolower(diagnostico_contributivo_lesao_principal_causa),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_contributivo_lesaes_secundarias_orgao_1 = stri_trans_general(
      tolower(diagnostico_contributivo_lesaes_secundarias_orgao_1),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_contributivo_lesaes_secundarias_causa_1 = stri_trans_general(
      tolower(diagnostico_contributivo_lesaes_secundarias_causa_1),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_contributivo_lesaes_secundarias_orgao_2 = stri_trans_general(
      tolower(diagnostico_contributivo_lesaes_secundarias_orgao_2),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_contributivo_lesaes_secundarias_causa_2 = stri_trans_general(
      tolower(diagnostico_contributivo_lesaes_secundarias_causa_2),
      encoding
    )
  )

df <- df %>% 
  mutate(
    diagnostico_contributivo_motivo = stri_trans_general(tolower(diagnostico_contributivo_motivo), encoding)
  )

# sistemas
df <- df %>% 
  mutate(
    cavidades_corporeas = stri_trans_general(tolower(cavidades_corporeas), encoding)
  )

df <- df %>% 
  mutate(
    tecido_cutaneo_e_subcutaneo = stri_trans_general(tolower(tecido_cutaneo_e_subcutaneo), encoding)
  )

df <- df %>% 
  mutate(
    sistema_musculo_esqueletico = stri_trans_general(tolower(sistema_musculo_esqueletico), encoding)
  )

df <- df %>% 
  mutate(
    sistema_respiratorio = stri_trans_general(tolower(sistema_respiratorio), encoding)
  )

df <- df %>% 
  mutate(
    sistema_cardiovascular = stri_trans_general(tolower(sistema_cardiovascular), encoding)
  )

df <- df %>% 
  mutate(
    aparato_digestorio = stri_trans_general(tolower(aparato_digestorio), encoding)
  )

df <- df %>% 
  mutate(
    sistema_urinario = stri_trans_general(tolower(sistema_urinario), encoding)
  )

df <- df %>% 
  mutate(
    sistema_reprodutivo = stri_trans_general(tolower(sistema_reprodutivo), encoding)
  )

df <- df %>% 
  mutate(
    sistema_linfo_hematopoietico = stri_trans_general(tolower(sistema_linfo_hematopoietico), encoding)
  )

df <- df %>% 
  mutate(
    sistema_endocrino = stri_trans_general(tolower(sistema_endocrino), encoding)
  )

df <- df %>% 
  mutate(
    orgaos_dos_sentidos = stri_trans_general(tolower(orgaos_dos_sentidos), encoding)
  )

df <- df %>%
  mutate(
    sistema_nervoso_central = stri_trans_general(tolower(sistema_nervoso_central), encoding)
  )

# Informações da necropsia
df <- df %>% 
  mutate(
    necropsia_sexo = stri_trans_general(tolower(necropsia_sexo), encoding)
  )

df <- df %>% 
  mutate(
    necropsia_estagio_de_desenvolvimento = stri_trans_general(
      tolower(necropsia_estagio_de_desenvolvimento), 
      encoding
    )
  )

df <- df %>% 
  mutate(
    necropsia_classe_do_individuo = stri_trans_general(tolower(necropsia_classe_do_individuo), encoding)
  )

df <- df %>% 
  mutate(
    necropsia_especie_do_individuo = stri_trans_general(
      tolower(necropsia_especie_do_individuo), 
      encoding
    )
  )

# Separando interacoes_antropicas em interacao_tipo e interacao_nivel
df <-
  extract(
    df,
    interacoes_antropicas,
    into = c('interacao_tipo', 'interacao_remover'),
    regex = regexTipo,
    remove = FALSE
  )

df <-
  extract(
    df,
    interacao_remover,
    into = c('interacao_nivel', 'interacao_remover'),
    regex = regexNivel,
    remove = FALSE
  )

df <- select(df,-c('interacao_remover'))

# interacoes_antropicas - splits into interacao_1, interacao_2 and interacao_3
# clean_df <- separate(clean_df, interacoes_antropicas, into = c('interacao_1', 'interacao_remover'), sep = ';', extra = "merge", remove = FALSE)
# clean_df <- separate(clean_df, interacao_remover, into = c('interacao_2', 'interacao_3'), sep = ';', extra = "merge", remove = TRUE)

# # interacoes_antropica - interacao_1 splits into interacao_1_tipo and interacao_1_nivel
# clean_df <- extract(clean_df, interacao_1, into = c('interacao_1_tipo', 'interacao_1_remover'), regex = regexTipo, remove = TRUE)
# clean_df <- extract(clean_df, interacao_1_remover, into = c('interacao_1_nivel','interacao_1_remover'), regex = regexNivel, remove = TRUE)
# clean_df <- select(clean_df, -c('interacao_1_remover'))

# # interacoes_antropica - interacao_2 splits into interacao_2_tipo and interacao_2_nivel
# clean_df <- extract(clean_df, interacao_2, into = c('interacao_2_tipo', 'interacao_2_remover'), regex = regexTipo, remove = TRUE)
# clean_df <- extract(clean_df, interacao_2_remover, into = c('interacao_2_nivel','interacao_2_remover'), regex = regexNivel, remove = TRUE)
# clean_df <- select(clean_df, -c('interacao_2_remover'))

# # interacoes_antropica - interacao_3 splits into interacao_3_tipo and interacao_3_nivel
# clean_df <- extract(clean_df, interacao_3, into = c('interacao_3_tipo', 'interacao_3_remover'), regex = regexTipo, remove = TRUE)
# clean_df <- extract(clean_df, interacao_3_remover, into = c('interacao_3_nivel','interacao_3_remover'), regex = regexNivel, remove = TRUE)
# clean_df <- select(clean_df, -c('interacao_3_remover'))

# Separando cavidades_corporeas em cc_status, cc_motivo e cc_analise
df <-
  extract(
    df,
    cavidades_corporeas,
    into = c('cc_status', 'cc'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    cc,
    into = c('cc_remover', 'cc_motivo', 'cc_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('cc', 'cc_remover'))

# Separando tecido_cutaneo_e_subcutaneo em tcs_status, tcs_motivo e tcs_analise
df <-
  extract(
    df,
    tecido_cutaneo_e_subcutaneo,
    into = c('tcs_status', 'tcs'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    tcs,
    into = c('tcs_remover', 'tcs_motivo', 'tcs_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('tcs', 'tcs_remover'))

# Separando sistema_musculo_esqueletico em sme_status, sme_motivo e sme_analise
df <-
  extract(
    df,
    sistema_musculo_esqueletico,
    into = c('sme_status', 'sme'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    sme,
    into = c('sme_remover', 'sme_motivo', 'sme_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('sme', 'sme_remover'))

# Separando sistema_respiratorio em sres_status, sres_motivo e sres_analise
df <-
  extract(
    df,
    sistema_respiratorio,
    into = c('sres_status', 'sres'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    sres,
    into = c('sres_remover', 'sres_motivo', 'sres_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('sres', 'sres_remover'))

# Separando sistema_cardiovascular em sc_status, sc_motivo e sc_analise
df <-
  extract(
    df,
    sistema_cardiovascular,
    into = c('sc_status', 'sc'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    sc,
    into = c('sc_remover', 'sc_motivo', 'sc_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('sc', 'sc_remover'))

# Separando aparato_digestorio em ad_status, ad_motivo e ad_analise
df <-
  extract(
    df,
    aparato_digestorio,
    into = c('ad_status', 'ad'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    ad,
    into = c('ad_remover', 'ad_motivo', 'ad_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('ad', 'ad_remover'))

# Separando sistema_urinario em su_status, su_motivo e su_analise
df <-
  extract(
    df,
    sistema_urinario,
    into = c('su_status', 'su'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    su,
    into = c('su_remover', 'su_motivo', 'su_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('su', 'su_remover'))

# Separando sistema_reprodutivo em srep_status, srep_motivo e srep_analise
df <-
  extract(
    df,
    sistema_reprodutivo,
    into = c('srep_status', 'srep'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    srep,
    into = c('srep_remover', 'srep_motivo', 'srep_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('srep', 'srep_remover'))

# Separando sistema_linfo_hematopoietico em slh_status, slh_motivo e slh_analise
df <-
  extract(
    df,
    sistema_linfo_hematopoietico,
    into = c('slh_status', 'slh'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    slh,
    into = c('slh_remover', 'slh_motivo', 'slh_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('slh', 'slh_remover'))

# Separando sistema_endocrino em se_status, se_motivo e se_analise
df <-
  extract(
    df,
    sistema_endocrino,
    into = c('se_status', 'se'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    se,
    into = c('se_remover', 'se_motivo', 'se_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('se', 'se_remover'))

# Separando orgaos_dos_sentidos em os_status, os_motivo e os_analise
df <-
  extract(
    df,
    orgaos_dos_sentidos,
    into = c('os_status', 'os'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    os,
    into = c('os_remover', 'os_motivo', 'os_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('os', 'os_remover'))

# Separando sistema_nervoso_central em snc_status, snc_motivo e snc_analise
df <-
  extract(
    df,
    sistema_nervoso_central,
    into = c('snc_status', 'snc'),
    regex = regexStatus,
    remove = FALSE
  )

df <-
  extract(
    df,
    snc,
    into = c('snc_remover', 'snc_motivo', 'snc_analise'),
    regex = regexMotivo,
    remove = FALSE
  )

df <- select(df,-c('snc', 'snc_remover'))

# Removendo stop words
df$cc_motivo <- removeWords(df$cc_motivo, stopWords)
df$cc_analise <- removeWords(df$cc_analise, stopWords)
df$tcs_motivo <- removeWords(df$tcs_motivo, stopWords)
df$tcs_analise <- removeWords(df$tcs_analise, stopWords)
df$sme_motivo <- removeWords(df$sme_motivo, stopWords)
df$sme_analise <- removeWords(df$sme_analise, stopWords)
df$sres_motivo <- removeWords(df$sres_motivo, stopWords)
df$sres_analise <- removeWords(df$sres_analise, stopWords)
df$sc_motivo <- removeWords(df$sc_motivo, stopWords)
df$sc_analise <- removeWords(df$sc_analise, stopWords)
df$ad_motivo <- removeWords(df$ad_motivo, stopWords)
df$ad_analise <- removeWords(df$ad_analise, stopWords)
df$su_motivo <- removeWords(df$su_motivo, stopWords)
df$su_analise <- removeWords(df$su_analise, stopWords)
df$srep_motivo <- removeWords(df$srep_motivo, stopWords)
df$srep_analise <- removeWords(df$srep_analise, stopWords)
df$slh_motivo <- removeWords(df$slh_motivo, stopWords)
df$slh_analise <- removeWords(df$slh_analise, stopWords)
df$se_motivo <- removeWords(df$se_motivo, stopWords)
df$se_analise <- removeWords(df$se_analise, stopWords)
df$os_motivo <- removeWords(df$os_motivo, stopWords)
df$os_analise <- removeWords(df$os_analise, stopWords)
df$snc_motivo <- removeWords(df$snc_motivo, stopWords)
df$snc_analise <- removeWords(df$snc_analise, stopWords)

# Removendo pontuação e espaços extras
df <- df %>% 
  mutate(interacao_tipo = trimws(
      gsub(
        regexEspacosExtras, ' ',
        gsub(regexPontuacao, ' ', interacao_tipo)
      )
    )
  )

df <- df %>% 
  mutate(cc_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', cc_motivo)
      )
    )
  )

df <- df %>% 
  mutate(cc_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', cc_analise)
      )
    )
  )

df <- df %>% 
  mutate(tcs_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', tcs_motivo)
      )
    )
  )

df <- df %>% 
  mutate(tcs_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', tcs_analise)
      )
    )
  )

df <- df %>% 
  mutate(sme_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', sme_motivo)
      )
    )
  )

df <- df %>% 
  mutate(
      sme_analise = trimws(
        gsub(
          regexEspacosExtras, ' ', 
          gsub(regexPontuacao, ' ', sme_analise)
      )
    )
  )

df <- df %>% 
  mutate(sres_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', sres_motivo)
      )
    )
  )

df <- df %>% 
  mutate(sres_analise = trimws(
      gsub(
        regexEspacosExtras, ' ',
        gsub(regexPontuacao, ' ', sres_analise)
      )
    )
  )

df <- df %>% 
  mutate(sc_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', sc_motivo)
      )
    )
  )

df <- df %>% 
  mutate(sc_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', sc_analise)
      )
    )
  )

df <- df %>% 
  mutate(ad_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', ad_motivo)
      )
    )
  )

df <- df %>% 
  mutate(ad_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', ad_analise)
      )
    )
  )

df <- df %>% 
  mutate(su_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', su_motivo)
      )
    )
  )

df <- df %>% 
  mutate(su_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', su_analise)
      )
    )
  )

df <- df %>% 
  mutate(srep_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', srep_motivo)
      )
    )
  )

df <- df %>% 
  mutate(srep_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', srep_analise)
      )
    )
  )

df <- df %>% 
  mutate(slh_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', slh_motivo)
      )
    )
  )

df <- df %>% 
  mutate(slh_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', slh_analise)
      )
    )
  )

df <- df %>% 
  mutate(se_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', se_motivo)
      )
    )
  )

df <- df %>% 
  mutate(se_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', se_analise)
      )
    )
  )

df <- df %>% 
  mutate(os_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', os_motivo)
      )
    )
  )

df <- df %>% 
  mutate(os_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', os_analise)
      )
    )
  )

df <- df %>% 
  mutate(snc_motivo = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', snc_motivo)
      )
    )
  )

df <- df %>% 
  mutate(snc_analise = trimws(
      gsub(
        regexEspacosExtras, ' ', 
        gsub(regexPontuacao, ' ', snc_analise)
      )
    )
  )

# Alterando valores "vazios"para "indeterminado"
df$diagnostico_presuntivo_lesao_principal_orgao <-
  ifelse(
    df$diagnostico_presuntivo_lesao_principal_orgao == '',
    'indeterminado',
    df$diagnostico_presuntivo_lesao_principal_orgao
  )

df$diagnostico_presuntivo_lesao_principal_causa <-
  ifelse(
    df$diagnostico_presuntivo_lesao_principal_causa == '',
    'indeterminado',
    df$diagnostico_presuntivo_lesao_principal_causa
  )

df$diagnostico_primario_lesao_principal_orgao <-
  ifelse(
    df$diagnostico_primario_lesao_principal_orgao == '',
    'indeterminado',
    df$diagnostico_primario_lesao_principal_orgao
  )

df$diagnostico_primario_lesao_principal_causa <-
  ifelse(
    df$diagnostico_primario_lesao_principal_causa == '',
    'indeterminado',
    df$diagnostico_primario_lesao_principal_causa
  )

df$presenca_de_residuos_solidos <-
  ifelse(
    df$presenca_de_residuos_solidos == '',
    'indeterminado',
    df$presenca_de_residuos_solidos
  )

# Converte colunas do modelo para factor
df[model_columns] = lapply(df[model_columns], as.factor)

# Criando base de análise, treino e validação de modelo
df_analysis <- filter(df, quantidade_interacoes == 'one')
df_analysis <- select(df_analysis, model_columns)

# Criando base para aplicar o modelo final
df_testing <- filter(df, quantidade_interacoes == 'none')
df_testing <- select(df_testing, model_columns)

write.csv(
  df_analysis, 
  'data/pmp-necropsia-analise.csv', 
  row.names = FALSE
)

write.csv(
  df_testing, 
  'data/pmp-necropsia-teste.csv', 
  row.names = FALSE
)

# Removendo variáveis para liberar memória
rm(
  df,
  df_analysis,
  df_testing,
  unnecessary_columns,
  model_columns,
  stopWords,
  regexStatus,
  regexMotivo,
  regexTipo,
  regexNivel,
  regexPontuacao,
  regexEspacosExtras,
  encoding
)
