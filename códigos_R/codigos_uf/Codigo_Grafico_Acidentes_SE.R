se <- sep_ano_uf('SE')
df <- data.frame(year = c(2018, 2019, 2020, 2021, 2022), count = se)
df
mean_value <- mean(df$count)
cv_value <- sd(df$count)/mean(df$count)
mean(ce)

ggplot(df, aes(x = year, y = count)) +
  geom_point(color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_text(aes(label = count, y = count), vjust = -1, color = "black", fontface = "bold", na.rm = TRUE) +
  ylim(500, 1100) +
  scale_x_continuous(breaks = seq(2018, 2022, by = 1), limits = c(2018, 2023), labels = function(x) ifelse(x == 2023, "", as.character(x)))+
  xlab("Ano") +
  ylab("Número de sinistros") +
  ggtitle("Sergipe: Ano x Número de sinistros (RENAEST)") +
  annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("Média: ", mean_value), vjust = 1.5, hjust = 0.9) +
  annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("CV: ", round(cv_value*100, 3), "%"), vjust = -0.7, hjust = 0.9)

           