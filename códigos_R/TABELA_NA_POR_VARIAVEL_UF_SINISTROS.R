library('tidyverse')
# Tira as variáveis não essenciais dos acidentes
tira_ne <- function(dados){
  dados$num_end_acidente <- NULL
  dados$km_via_acidente <- NULL
  dados$tp_rodovia <- NULL
  dados$tp_cruzamento <- NULL
  dados$tp_curva <- NULL
  dados$tp_pista <- NULL
  dados$ind_guardrail <- NULL
  dados$ind_cantcentral <- NULL
  dados$ind_acostamento <- NULL
  return(dados)
}
Acidentes2 <- tira_ne(Acidentes1)
2+2
#Separa os dados por UF
dados_por_uf <- function(uf){
  d <- Acidentes2 %>% filter(uf_acidente == uf )
  return(d)
}
#Retorna o % de NAs por variável no conjunto Acidentes, em um vetor. Necessário transformar
# em fator, pois a funçao prop.table do is.na retornará apenas um valor caso seja 100% ou 0% de
# NAs. Assim, em fator, a funão retornará dois valores, um para TRUE e outro para FALSE.
sep_var_uf <- function(uf){
  dados <- dados_por_uf(uf)
  lim_velocidade <- as.numeric(prop.table(table(factor(is.na(dados$lim_velocidade), levels = c('FALSE', 'TRUE')))))[2]
  cep_acidente <- as.numeric(prop.table(table(factor(is.na(dados$cep_acidente), levels = c('FALSE', 'TRUE')))))[2]
  longitude_acidente <- as.numeric(prop.table(table(factor(is.na(dados$longitude_acidente), levels = c('FALSE', 'TRUE')))))[2]
  latitude_acidente <- as.numeric(prop.table(table(factor(is.na(dados$latitude_acidente), levels = c('FALSE', 'TRUE')))))[2]
  cond_pista <- as.numeric(prop.table(table(factor(is.na(dados$cond_pista), levels = c('FALSE', 'TRUE')))))[2]
  tp_pavimento <- as.numeric(prop.table(table(factor(is.na(dados$tp_pavimento), levels = c('FALSE', 'TRUE')))))[2]
  cond_meteorologica <- as.numeric(prop.table(table(factor(is.na(dados$cond_meteorologica), levels = c('FALSE', 'TRUE')))))[2]
  bairro_acidente <- as.numeric(prop.table(table(factor(is.na(dados$bairro_acidente), levels = c('FALSE', 'TRUE')))))[2]
  
  return(c(lim_velocidade, cep_acidente, longitude_acidente, latitude_acidente, cond_pista, tp_pavimento,
           cond_meteorologica, bairro_acidente))
}

#Retorna um dataframe com % de nas por UF para cada variável. Usado na tabela 2.
tabela_cv <- function(){
  df = data.frame(uf = character(),
                  lim_velocidade = numeric(),
                  cep_acidente = numeric(),
                  longitude_acidente = numeric(),
                  latitude_acidente = numeric(),
                  cond_pista = numeric(),
                  tp_pavimento = numeric(),
                  cond_meteorologica = numeric(),
                  bairro_acidente = numeric())
  uf <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG',
          'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS',
          'RO', 'RR', 'SC', 'SP', 'SE', 'TO')
  for (i in 1:27){
    dados <- sep_var_uf(uf[i])
    df <- df %>% add_row(uf = uf[i], 
                         lim_velocidade = round(dados[1]*100, 2),
                         cep_acidente = round(dados[2]*100, 2),
                         longitude_acidente = round(dados[3]*100, 2),
                         latitude_acidente = round(dados[4]*100, 2),
                         cond_pista = round(dados[5]*100, 2),
                         tp_pavimento = round(dados[6]*100, 2),
                         cond_meteorologica = round(dados[7]*100, 2),
                         bairro_acidente = round(dados[8]*100, 2))
  }
  return(df)
}
tabela <- tabela_cv()
tabela
2+2
write.csv2(tabela, "tabela.csv2", row.names = FALSE)
2+2
