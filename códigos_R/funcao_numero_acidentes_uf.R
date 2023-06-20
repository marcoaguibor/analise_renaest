# Código para separar número de acidentes por ano, por UF, além de calcular
# o coeficiente de variação

library('tidyverse')
dados_por_uf <- function(uf){
  d <- Acidentes1 %>% filter(uf_acidente == uf )
  return(d)
}

sep_ano_uf <- function(uf){
  dados <- dados_por_uf(uf)
  n2018 <- nrow(dados %>% filter(ano_acidente == 2018))
  n2019 <- nrow(dados %>% filter(ano_acidente == 2019))
  n2020 <- nrow(dados %>% filter(ano_acidente == 2020))
  n2021 <- nrow(dados %>% filter(ano_acidente == 2021))
  n2022 <- nrow(dados %>% filter(ano_acidente == 2022))
  return(c(n2018, n2019, n2020, n2021, n2022))
}

cv <- function(x){
  data <- sep_ano_uf(x)
  c <- (sd(data))/mean(data)
  cat('Dados: ', data,'\n')
  cat('Coeficiente de Variação: ', c)
}

