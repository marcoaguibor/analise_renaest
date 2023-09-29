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
    labs(x = '', y ='', title = 'Campos não informados') +
    theme(
      plot.background = element_rect(fill = "white", color = "white"),
      plot.title = element_text(size = 18)
    )
}

plot_pna_colisao <- function(df) {
  ggplot(df, aes(x = reorder(uf, Perc_Col_NE), y = Perc_Col_NE)) +
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
    labs(x = '', y ='', title = 'Colisões não especificadas') +
    theme(
      plot.background = element_rect(fill = "white", color = "white"),
      plot.title = element_text(size = 18)
    )
}