# Sempre tente usar um verbo no nome da função

# Aqui o `colSums` junto com o `is.na` ajuda a somar a quantidade de NA por col
# Depois é só dividir a quantidade de NA pela quantidade total de linhas na
# tabela, com o uso do `nrow`.
# Na mesma função ja da pra considerar o filtro por UF

calc_na <- function(df, uf = "BR") {
  if (uf != "BR") {
    df <- df %>% filter(uf_acidente == uf)
  }
  na_count <- colSums(is.na(df))
  na_table <- tibble(var = names(na_count), na_qtde = na_count)
  na_table %>% mutate(pna = na_qtde / nrow(df), uf = uf)
}

# Aqui foi calculado a quantidade de acidentes por UF e também a média e o CV
# Deixei como entrada o df, mas pelo o que eu entendi o cv só foi calculado
# para acidentes

calc_cv <- function(df, uf = "BR") {
  if (uf != "BR") {
    df <- df %>%
      filter(uf_acidente == uf, ano_acidente != 2023) %>%
      count(uf_acidente, ano_acidente)
  } else {
    df <- df %>%
      filter(ano_acidente != 2023) %>%
      count(ano_acidente) %>%
      mutate(uf_acidente = "BR")
  }
  df %>%
    mutate(media = mean(n), cv = sd(n) / media) %>%
    nest(acidentes = c(ano_acidente, n))
}

# Por fim, função para criar uma tabela com todos os CV, incluindo o do BR

join_cv <- function(df_uf, df_br) {
    df_uf %>%
      reduce(bind_rows) %>%
      bind_rows(df_br)
}

# Aqui uma função para calcular o pna médio para o BR e os UFs

calc_mean_pna <- function(pna_list) {
  pna_list %>%
    reduce(bind_rows) %>%
    group_by(uf) %>%
    summarise(pna = mean(pna)) %>%
    arrange(-pna)
}

# Codigo antigo -----------------------------------------------------------

# Constroi um dataframe com o % de NAs de cada variável
# vtotal_nas <- function(dados){
#   df <- data.frame(variavel = character(), pna = numeric())
#   for (i in 1:length(colnames(dados))) {
#     v <- colnames(dados)[i]
#     pNA <- as.numeric(prop.table(
#       table(factor(is.na(dados[[i]]), levels = c('FALSE', 'TRUE')))
#     ))[2]
#     pNA <- ifelse(is.na(pNA), 1, pNA)
#     df <- df %>% add_row(variavel = v, pna = pNA)
#   }
#   return(df)
# }


# Separa os dados por unidade federativa

# dados_por_uf <- function(uf, dataset){
#   d <- dataset %>% filter(uf_acidente == uf )
#   return(d)
# }

# TABELA CONJUNTO ACIDENTES #

# Retorna o % de NAs por variável no conjunto Acidentes, em um vetor.
# Necessário transformar em fator, pois a funçao prop.table do is.na retornará
# apenas um valor caso seja 100% ou 0% de NAs.
# Assim, em fator, a funão retornará dois valores,
# um para TRUE e outro para FALSE.

# sep_var_uf <- function(uf){
#   dados <- dados_por_uf(uf)
#   lim_velocidade <- as.numeric(prop.table(
#     table(factor(is.na(dados$lim_velocidade), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   cep_acidente <- as.numeric(prop.table(
#     table(factor(is.na(dados$cep_acidente), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   longitude_acidente <- as.numeric(prop.table(
#     table(factor(is.na(dados$longitude_acidente), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   latitude_acidente <- as.numeric(prop.table(
#     table(factor(is.na(dados$latitude_acidente), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   cond_pista <- as.numeric(prop.table(
#     table(factor(is.na(dados$cond_pista), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   tp_pavimento <- as.numeric(prop.table(
#     table(factor(is.na(dados$tp_pavimento), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   cond_meteorologica <- as.numeric(prop.table(
#     table(factor(is.na(dados$cond_meteorologica), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   bairro_acidente <- as.numeric(prop.table(
#     table(factor(is.na(dados$bairro_acidente), levels = c('FALSE', 'TRUE')))
#   ))[2]
#
#   return(c(
#     lim_velocidade,
#     cep_acidente,
#     longitude_acidente,
#     latitude_acidente,
#     cond_pista,
#     tp_pavimento,
#     cond_meteorologica,
#     bairro_acidente
#   ))
# }

# Retorna um dataframe com % de nas por UF para cada variável.
# Usado na tabela 2.

# tabela_cv <- function(){
#   df = data.frame(
#     uf = character(),
#     lim_velocidade = numeric(),
#     cep_acidente = numeric(),
#     longitude_acidente = numeric(),
#     latitude_acidente = numeric(),
#     cond_pista = numeric(),
#     tp_pavimento = numeric(),
#     cond_meteorologica = numeric(),
#     bairro_acidente = numeric()
#   )
#
#   uf <- c(
#     'AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS',
#     'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO', 'RR', 'SC',
#     'SP', 'SE', 'TO'
#   )
#
#   for (i in 1:27) {
#     dados <- sep_var_uf(uf[i])
#     df <- df %>% add_row(
#       uf = uf[i],
#       lim_velocidade = round(dados[1]*100, 2),
#       cep_acidente = round(dados[2]*100, 2),
#       longitude_acidente = round(dados[3]*100, 2),
#       latitude_acidente = round(dados[4]*100, 2),
#       cond_pista = round(dados[5]*100, 2),
#       tp_pavimento = round(dados[6]*100, 2),
#       cond_meteorologica = round(dados[7]*100, 2),
#       bairro_acidente = round(dados[8]*100, 2)
#     )
#   }
#   return(df)
# }
# vtotal_nas(Acidentes2)
# tabela_total_nas()

# TABELA CONJUNTO VÍTIMAS #

# Retorna o % de NAs por variável no conjunto Vítimas, em um vetor.
# Necessário transformar
# em fator, pois a funçao prop.table do is.na retornará apenas um valor
# caso seja 100% ou 0% de
# NAs. Assim, em fator, a funão retornará dois valores, um para TRUE e
# outro para FALSE.
#
# sep_var_ufv <- function(uf){
#   dados <- dados_por_uf(uf)
#   eq_seg <- as.numeric(prop.table(
#     table(factor(is.na(dados$equip_seguranca), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   susp_alcool <- as.numeric(prop.table(
#     table(factor(is.na(dados$susp_alcool), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   gravidade_lesao <- as.numeric(prop.table(
#     table(factor(is.na(dados$gravidade_lesao), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   faixa_idade <- as.numeric(prop.table(
#     table(factor(is.na(dados$faixa_idade), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   tp_envolvido <- as.numeric(prop.table(
#     table(factor(is.na(dados$tp_envolvido), levels = c('FALSE', 'TRUE')))
#   ))[2]
#   genero <- as.numeric(prop.table(
#     table(factor(is.na(dados$genero), levels = c('FALSE', 'TRUE')))
#   ))[2]
#
#   return(c(
#     eq_seg,
#     susp_alcool,
#     gravidade_lesao,
#     faixa_idade,
#     tp_envolvido,
#     genero
#   ))
# }

#Retorna um dataframe com % de nas por UF para cada variável. Usado na tabela 3.

# tabela_cvv <- function(){
#   df = data.frame(
#     uf = character(),
#     equip_seguranca = numeric(),
#     susp_alcool = numeric(),
#     gravidade_lesao = numeric(),
#     faixa_idade = numeric(),
#     tp_envolvido = numeric(),
#     genero = numeric()
#   )
#   uf <- c(
#     'AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT',
#     'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO',
#     'RR', 'SC', 'SP', 'SE', 'TO'
#   )
#   for (i in 1:27) {
#     dados <- sep_var_ufv(uf[i])
#     df <- df %>% add_row(
#       uf = uf[i],
#       equip_seguranca = round(dados[1]*100, 2),
#       susp_alcool = round(dados[2]*100, 2),
#       gravidade_lesao = round(dados[3]*100, 2),
#       faixa_idade = round(dados[4]*100, 2),
#       tp_envolvido = round(dados[5]*100, 2),
#       genero = round(dados[6]*100, 2)
#     )
#   }
#   return(df)
# }
