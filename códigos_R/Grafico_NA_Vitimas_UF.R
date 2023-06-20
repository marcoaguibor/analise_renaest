# % de NAs para a base Vítimas
library('tidyverse')
dados_por_uf <- function(uf){
  d <- Vitimas1 %>% filter(uf_acidente == uf )
  return(d)
}

ac <- dados_por_uf('AC')


#Retorna um dataframe com % de nas por variavel no conjunto
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
vtotal_nas(Vitimas1)
d1 <- vtotal_nas(ac)

ggplot(d1, aes(x = reorder(variavel, pna*100), y = pna*100)) +
  geom_point(aes(color = pna*100)) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
  xlab("Variáveis") +
  ylab("Percentual de campos não informados") +
  ggtitle("% Campos não informados Acre: Vítimas 2018 - jan/2023") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0, 100))
