# Função para calcular o percentual de NA's por UF. Acidentes2 é o database
# Acidentes tirando os campos essenciais (funcao tira_ne)
library('tidyverse')
dados_por_uf <- function(uf){
  d <- Acidentes2 %>% filter(uf_acidente == uf )
  return(d)
}

vtotal_nas <- function(dados){
  df <- data.frame(variavel = character(), pna = numeric())
  for (i in 1:length(colnames(dados))){
    v <- colnames(dados)[i]
    pNA <- as.numeric(prop.table(table(factor(is.na(dados[[i]]), levels = c('FALSE', 'TRUE')))))[2]
    pNA <- ifelse(is.na(pNA), 1, pNA)
    df <- df %>% add_row(variavel = v, pna = pNA)
  }
  return(df)
}

#Funcao que tira os campos não essenciais do database Acidentes
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

#Exemplo percentual NA para Amazonas
am <- dados_por_uf('AM')
nae <- vtotal_nas(Vitimas1)

nae
round(mean(data.frame((nae[2]))$pna),4)


