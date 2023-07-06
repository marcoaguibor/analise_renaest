library('tidyverse')

#Cálculo de percentual de campos não informados para cada variável por UF
d1 <- vtotal_nas(dados_por_uf(uf))

#Plot do gráfico do percentudal de campos não informados.
ggplot(d1, aes(x = reorder(variavel, pna*100), y = pna*100)) +
  geom_point(aes(color = pna*100)) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
  xlab("Variáveis") +
  ylab("Percentual de campos não informados") +
  ggtitle("% Campos não informados variáveis UF: Conjunto 2018 - jan/2023") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0, 100))


#Gráficos número de sinistros/ano
UF <- sep_ano_uf(uf, Acidentes1)
df <- data.frame(year = c(2018, 2019, 2020, 2021, 2022), count = UF)
mean_value <- mean(df$count)
cv_value <- function(df){
  return(sd(df$count)/mean(df$count))
}

ggplot(df, aes(x = year, y = count)) +
  geom_point(color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_text(aes(label = count, y = count), vjust = -1, color = "black", fontface = "bold", na.rm = TRUE) +
  ylim(min(UF), max(UF)+1000) +
  scale_x_continuous(breaks = seq(2018, 2022, by = 1), limits = c(2018, 2023), labels = function(x) ifelse(x == 2023, "", as.character(x)))+
  xlab("Ano") +
  ylab("Número de sinistros") +
  ggtitle("UF: Ano x Número de sinistros (RENAEST)") +
  annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("Média: ", mean_value, vjust = 1.5, hjust = 0.77)) +
  annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("CV: ", round(cv_value()*100, 3), "%"), vjust = -0.7, hjust = 0.9)


#Gráfico para CV

#Cria um dataframe com o nome da UF e o respectivo CV
cv_estado <- function(){
  df = data.frame(uf = character(),
                  cv = numeric())
  uf <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG',
          'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS',
          'RO', 'RR', 'SC', 'SP', 'SE', 'TO')
  for (i in 1:27){
    dados <- sep_ano_uf(uf[i], Acidentes1)
    coeficiente <- cv_value(dados)
    df <- df %>% add_row(uf = uf[i], cv = coeficiente)
  }
  return(df)
  
  
}

dados1 <- cv(estado)
ggplot(dados1, aes(x = uf, y = (cv_estado)*100)) +
  geom_point(aes(color = cv_estado*100)) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
  xlab("UF") +
  ylab("Coeficiente de Variação (%)") +
  ggtitle("CV de cada UF. Fonte: RENAEST") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))