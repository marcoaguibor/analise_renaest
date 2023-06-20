library('Tidyverse')
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


#Constroi um dataframe com o % de NAs de cada variável
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

#Plota o gráfico de NAs para o conjunto em estudo
nae <- vtotal_nas(dados)
ggplot(nae, aes(x = reorder(variavel, pna*100), y = pna*100)) +
  geom_point(aes(color = pna*100)) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
  xlab("Variáveis Essenciais") +
  ylab("Percentual de campos não informados") +
  ggtitle("% Campos não informados variáveis essenciais: Sinistros 2018 - jan/2023") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0, 100))
AC_Acidentes$num_end_acidente <- NULL
