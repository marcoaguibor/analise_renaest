# Aqui eu modifiquei os seus plots.
# 1. O mais importante foi usar o `coord_flip` para trocar os eixos.
# Isso é importante para melhorar a leitura das variáveis.
# 2. Agora a variável mais grave, com menos campos informados,
# fica mais acima do gráfico, deixando mais em evidência.
# 3. Usei a cor azul do observatório, depois eu te passo o nosso manual
# da marca com a paleta de cores.
# 4. Usei o `geom_segment` para auxiliar na visualização do pna.
# 5. Usei o `scales::percent` para deixar o pna em %
# 6. Selecionei uma fonte do google, o fira sans, e utilizei no gráfico.
# Eu evito a fonte padrão para não deixar o grafico igual a muitos outros que
# são elaborados com o ggplot. o pacote `showtext` ajuda no uso de fontes
# customizadas
# 7. O `theme_minimal` ajuda deixar o gráfico mais limpo, com o fundo branco.
# Aqui é mais subjetivo, mas em geral eu acredito que o fundo branco é melhor
# para usar em documentos.


plot_pna <- function(df) {
  ggplot(df, aes(x = reorder(var, pna), y = pna)) +
    geom_segment(aes(xend = var, yend = 0), color = "#00496d") +
    geom_point(color = "#00496d", size = 1.5) +
    theme_minimal(
      base_family = "firasans",
      base_size = 26,
      base_line_size = 0.5
    ) +
    coord_flip() +
    labs(x = "Variáveis", y = "Percentual de campos não informados") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    theme(plot.background = element_rect(fill = "white", color = "white"))
}

# Além das mudanças que eu apliquei nos gráficos anteriores, aqui eu usei
# o `annotate` junto com "label" para melhorar a visualização do texto.
# Também deixei o limite mínimo de sinistros em 0, com o `limits` no
# `scale_y_continuous`

plot_cv_sinistros <- function(df) {
  cv_label <- scales::percent(
    df$cv,
    accuracy = 0.01,
    decimal.mark = ",",
    big.mark = "."
  )

  media_label <- scales::number(
    round(df$media, 0),
    big.mark = ".",
    decimal.mark = ","
  )

  count_acidentes <- df %>% unnest(acidentes)

  count_acidentes %>%
    ggplot(aes(x = ano_acidente, y = n)) +
    geom_point(size = 1.5, color = "#00496d") +
    geom_line(color = "#00496d") +
    geom_label(
      aes(label = scales::number(n, big.mark = '.', decimal.mark = ",")),
      nudge_y = max(count_acidentes$n) * 0.08,
      size = 6
    ) +
    theme_minimal(
      base_family = "firasans",
      base_size = 26,
      base_line_size = 0.5
    ) +
    annotate(
      "label",
      x = 2021.5,
      y = 0.1 * max(count_acidentes$n),
      label = paste0("CV: ", cv_label, "; Média: ", media_label),
      size = 6
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    scale_x_continuous(minor_breaks = NULL) +
    labs(x = "Ano", y = "Número de sinistros") +
    theme(plot.background = element_rect(fill = "white", color = "white"))
}

# Plotando o CV por UF e BR
# Aqui eu decidi incluir o BR com uma cor diferente, com o auxilio do
# `gghighlight`.

plot_cv_total <- function(df) {
  ggplot(df, aes(x = reorder(uf_acidente, cv), y = cv)) +
    geom_segment(aes(xend = uf_acidente, yend = 0), color = "#00496d") +
    geom_point(color = "#00496d", size = 1.5) +
    gghighlight(
      uf_acidente != "BR",
      unhighlighted_params = list(color = "#f7951d", lty = "dashed")
    ) +
    coord_flip() +
    theme_minimal(
      base_family = "firasans",
      base_size = 26,
      base_line_size = 0.5
    ) +
    scale_y_continuous(
      breaks = seq(0, 1.25, 0.25),
      minor_breaks = NULL,
      labels = scales::percent
    ) +
    labs(x = "Localidade", y = "Coeficiente de Variação") +
    theme(plot.background = element_rect(fill = "white", color = "white"))
}

# Plotando o PNA por UF e BR, seguindo as diretrizes dos ultimos plots

plot_pna_mean <- function(df) {
  ggplot(df, aes(x = reorder(uf, pna), y = pna)) +
    geom_segment(aes(xend = uf, yend = 0), color = "#00496d") +
    geom_point(color = "#00496d", size = 1.5) +
    gghighlight(
      uf != "BR",
      unhighlighted_params = list(color = "#f7951d", lty = "dashed")
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    coord_flip() +
    theme_minimal(
      base_family = "firasans",
      base_size = 26,
      base_line_size = 0.5
    ) +
    labs(x = "Localidade", y = "Percentual de campos não informados") +
    theme(plot.background = element_rect(fill = "white", color = "white"))
}

# codigo antigo -----------------------------------------------------------

# Cálculo de percentual de campos não informados para cada variável por UF
# d1 <- vtotal_nas(dados_por_uf(uf))

# Plot do gráfico do percentudal de campos não informados.
# ggplot(d1, aes(x = reorder(variavel, pna*100), y = pna*100)) +
#   geom_point(aes(color = pna*100)) +
#   scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
#   xlab("Variáveis") +
#   ylab("Percentual de campos não informados") +
#   ggtitle("% Campos não informados variáveis UF: Conjunto 2018 - jan/2023") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   scale_y_continuous(limits = c(0, 100))


# Gráficos número de sinistros/ano
# UF <- sep_ano_uf(uf, Acidentes1)
# df <- data.frame(year = c(2018, 2019, 2020, 2021, 2022), count = UF)
# mean_value <- mean(df$count)
# cv_value <- function(df){
#   return(sd(df$count)/mean(df$count))
# }

# ggplot(df, aes(x = year, y = count)) +
#   geom_point(color = "steelblue") +
#   geom_line(color = "steelblue") +
#   geom_text(aes(label = count, y = count), vjust = -1, color = "black", fontface = "bold", na.rm = TRUE) +
#   ylim(min(UF), max(UF)+1000) +
#   scale_x_continuous(breaks = seq(2018, 2022, by = 1), limits = c(2018, 2023), labels = function(x) ifelse(x == 2023, "", as.character(x)))+
#   xlab("Ano") +
#   ylab("Número de sinistros") +
#   ggtitle("UF: Ano x Número de sinistros (RENAEST)") +
#   annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("Média: ", mean_value, vjust = 1.5, hjust = 0.77)) +
#   annotate("text", x = 2022.5 + 0.5, y = max(df$count), label = paste0("CV: ", round(cv_value()*100, 3), "%"), vjust = -0.7, hjust = 0.9)


# Gráfico para CV

# Cria um dataframe com o nome da UF e o respectivo CV
# cv_estado <- function(){
#   df = data.frame(uf = character(),
#                   cv = numeric())
#   uf <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG',
#           'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS',
#           'RO', 'RR', 'SC', 'SP', 'SE', 'TO')
#   for (i in 1:27){
#     dados <- sep_ano_uf(uf[i], Acidentes1)
#     coeficiente <- cv_value(dados)
#     df <- df %>% add_row(uf = uf[i], cv = coeficiente)
#   }
#   return(df)
#
#
# }
#
# dados1 <- cv(estado)
# ggplot(dados1, aes(x = uf, y = (cv_estado)*100)) +
#   geom_point(aes(color = cv_estado*100)) +
#   scale_color_gradient(low = "lightblue", high = "darkblue", name = '%') +
#   xlab("UF") +
#   ylab("Coeficiente de Variação (%)") +
#   ggtitle("CV de cada UF. Fonte: RENAEST") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
