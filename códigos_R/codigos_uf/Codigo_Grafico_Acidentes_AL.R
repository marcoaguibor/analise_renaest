
al <- sep_ano_uf('AL')
mean(sep_ano_uf('AC'))
cv <- function(x){
  return(sd(x)/mean(x))
}
df <- data.frame(year = c(2018, 2019, 2020, 2021, 2022), count = al)
df
mean_value <- mean(df$count)
cv_value <- sd(df$count)/mean(df$count)
al

ggplot(df, aes(x = year, y = count)) +
  geom_point(color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_text(aes(label = count, y = count), vjust = -1, color = "black", fontface = "bold", na.rm = TRUE) +
  ylim(200, 3000) +
  xlab("Ano") +
  ylab("Número de sinistros") +
  ggtitle("Alagoas: Ano x Número de sinistros (RENAEST)") +
  annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("Média: ", mean_value), vjust = 1.5, hjust = 1) +
  annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("CV: ", round(cv_value*100, 3), "%"), vjust = -0.7, hjust = 1.2)

           