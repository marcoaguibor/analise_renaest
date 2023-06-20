library('tidyverse')
dados_por_uf <- function(uf){
  d <- Acidentes2 %>% filter(uf_acidente == uf )
  return(d)
}
dados_por_ufv <- function(uf){
  d <- Vitimas1 %>% filter(uf_acidente == uf )
  return(d)
}

#Retorna um dataframe com %NAs por UF no conjunto de Acidentes2 (somente com campos essenciais)
nas_estado <- function(){
  df = data.frame( uf = character(),
  pna = numeric())
  uf <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG',
               'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS',
               'RO', 'RR', 'SC', 'SP', 'SE', 'TO')
  for (i in 1:27){
    d1 <- dados_por_uf(uf[i])
    pna <- as.numeric(prop.table(table(factor(is.na(d1), levels = c('FALSE', 'TRUE')))))[2]
    df <- df %>% add_row(uf = uf[i], pna = pna)
  }
  return(df)
}

#Retorna um dataframe com %NAs por UF no conjunto de Vitimas 
nas_estado2 <- function(){
  df = data.frame( uf = character(),
                   pna = numeric())
  uf <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG',
          'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS',
          'RO', 'RR', 'SC', 'SP', 'SE', 'TO')
  for (i in 1:27){
    d1 <- dados_por_ufv(uf[i])
    pna <- as.numeric(prop.table(table(factor(is.na(d1), levels = c('FALSE', 'TRUE')))))[2]
    df <- df %>% add_row(uf = uf[i], pna = pna)
  }
  return(df)
}
uf[3]
nuf
nuf <- nuf %>% add_row(uf = 'BR', pna = 0.2254)
nuf <- nas_estado()
nuf2 <- nas_estado2()
2+2
nuf2
nuf2 <- nuf2 %>% add_row(uf = 'BR', pna = 0.1559)
nuf1 <- nuf[order(nuf$pna, decreasing = TRUE),]
nuf3 <- nuf2[order(nuf2$pna, decreasing = TRUE),]
nuf3
VCK
data.frame(nuf3$uf, round(nuf3$pna, 4)*100)
data.frame(nuf1$uf, round(nuf1$pna, 4))
VCK <- nas_estado(Vitimas1)
ACK
as.factor(table(Acidentes1$uf_acidente))[1][2]
2+2

#Cria um gráfico com % de campos não informados por UF e Brasil, deixando o BR
#em evidência
ggplot(nuf2, aes(x = reorder(uf, pna*100), y = pna*100)) +
  geom_point(aes(color = pna*100)) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
  xlab("Unidades da federação + BR") +
  ylab("Percentual de campos comuns não informados") +
  ggtitle("Percentual de campos comuns não informados por UF e BR - Vítimas") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(0, 60)) +
  geom_segment(data = subset(nuf2, uf == "BR"),
               aes(x = reorder(uf, pna*100), xend = reorder(uf, pna*100), y = 0, yend = pna*100),
               linetype = "dashed")


