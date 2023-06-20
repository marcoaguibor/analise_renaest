library('tidyverse')

# Função para calcular o % de NAs por coluna, para qualquer conjunto de dados

vtotal_nas <- function(dados){
  df <- data.frame(variavel = character(), pna = numeric())
  for (i in 1:length(colnames(dados))){
    v <- colnames(dados)[i]
    pNA <- as.numeric(prop.table(table(factor(is.na(dados[[i]]), levels = c('FALSE', 'TRUE')))))[2]
    pNA <- ifelse(is.na(pNA), 0, pNA)
    df <- df %>% add_row(variavel = v, pna = pNA)
  }
  return(df)
}
t1 <- vtotal_nas(Acidentes2)
t2 <- t1[order(t1$pna, decreasing = TRUE),]
j1 <- vtotal_nas(Vitimas1)
j2 <- j1[order(j1$pna, decreasing = TRUE), ]
2+2
j2
t2
nas_Acidentes1 <- vtotal_nas(Acidentes1)
nas_Acidentes <- subset(nas_Acidentes, subset = 
                          !(nas_Acidentes$variavel == 'num_end_acidente'))

nasvitimas <- nas_Acidentes
#Plota o gráfico de NAs para o conjunto em estudo
nasvitimas
ggplot(nasvitimas, aes(x = reorder(variavel, pna*100), y = pna*100)) +
  geom_point(aes(color = pna*100)) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
  xlab("Variáveis Essenciais") +
  ylab("Percentual de NAs") +
  ggtitle("Essenciais: Acidentes 2018 - jan2023") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
