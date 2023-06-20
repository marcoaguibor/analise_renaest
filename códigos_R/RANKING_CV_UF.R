library('tidyverse')

#Separa os dados por UF
dados_por_uf <- function(uf){
  d <- Acidentes1 %>% filter(uf_acidente == uf )
  return(d)
}

#Separa o número de acidentes por ano
sep_ano_uf <- function(uf){
  dados <- dados_por_uf(uf)
  n2018 <- nrow(dados %>% filter(ano_acidente == 2018))
  n2019 <- nrow(dados %>% filter(ano_acidente == 2019))
  n2020 <- nrow(dados %>% filter(ano_acidente == 2020))
  n2021 <- nrow(dados %>% filter(ano_acidente == 2021))
  n2022 <- nrow(dados %>% filter(ano_acidente == 2022))
  return(c(n2018, n2019, n2020, n2021, n2022))
}
#Calcula o CV
cv <- function(x){
  return(sd(x)/mean(x))
}

#Cria um dataframe com o nome da UF e o respectivo CV
cv_estado <- function(){
  df = data.frame(uf = character(),
                  cv = numeric())
  uf <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG',
          'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS',
          'RO', 'RR', 'SC', 'SP', 'SE', 'TO')
  for (i in 1:27){
    dados <- sep_ano_uf(uf[i])
    coeficiente <- cv(dados)
    df <- df %>% add_row(uf = uf[i], cv = coeficiente)
  }
  return(df)
}

coef_v <- cv_estado()

#Cria um ranking de estados por CV
ranking <- coef_v[order(coef_v$cv, decreasing = TRUE),]
data.frame(ranking$uf, round(ranking$cv*100, 2))
