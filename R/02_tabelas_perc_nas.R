dados_por_uf <- function(uf){
  d <- acidentes %>% filter(uf_acidente == uf )
  return(d)
}

calc_na <- function(df, uf = "BR") {
  if (uf != "BR") {
    df <- df %>% filter(uf_acidente == uf)
  }
  na_count <- colSums(is.na(df))
  na_table <- tibble(var = names(na_count), na_qtde = na_count)
  na_table %>% mutate(pna = na_qtde / nrow(df), uf = uf)
}

calc_cv <- function(df, uf = "BR") {
  if (uf != "BR") {
    df <- df %>%
      filter(uf_acidente == uf, ano_acidente != 2023) %>%
      count(uf_acidente, ano_acidente)
  } else {
    df <- df %>%
      filter(ano_acidente != 2023) %>%
      count(ano_acidente) %>%
      mutate(uf_acidente = "BR")
  }
  df %>%
    mutate(media = mean(n), cv = sd(n) / media) %>%
    nest(acidentes = c(ano_acidente, n))
}

join_cv <- function(df_uf, df_br) {
    df_uf %>%
      reduce(bind_rows) %>%
      bind_rows(df_br)
}

calc_mean_pna <- function(pna_list) {
  pna_list %>%
    reduce(bind_rows) %>%
    group_by(uf) %>%
    summarise(pna = mean(pna)) %>%
    arrange(-pna)
}

calc_colisao_uf <- function(uf) {
  if (uf == 'BR') {
    counts <- table(factor(acidentes$tp_acidente))
    valor_colisao <- ifelse('COLISAO' %in% names(counts), counts['COLISAO'], 0)
  } else {
    dados_uf <- dados_por_uf(uf)
    counts <- table(factor(dados_uf$tp_acidente))
    valor_colisao <- ifelse('COLISAO' %in% names(counts), counts['COLISAO'], 0)
  }
  proporcao_colisao_uf <- valor_colisao / sum(counts, na.rm = TRUE)
  return(proporcao_colisao_uf)
}

calc_tabela_colisao <- function() {
  df <- data.frame(uf = character(), Perc_Col_NE = numeric())
  tp_colisao <- c(
    'COLISAO', 'COLISAO FRONTAL', 'COLISAO TRASEIRA', 'COLISAO LATERAL',
    'COLISAO TRANSVERSAL', 'ENGAVAMENTO'
  )
  for (uf in lista_uf) {
    proporcao_colisao_uf <- calc_colisao_uf(uf)
    df <- df %>% add_row(uf = uf, Perc_Col_NE = proporcao_colisao_uf)
  }
  df <- df %>% add_row(uf = 'BR', Perc_Col_NE = calc_colisao_uf('BR'))
  return(df)
}
