# Aqui da pra usar as funções do `tidyverse`:
# Use o `mutate` para modificar as colunas e o `select` para manter as
# variáveis de interesse

arrange_acidentes <- function() {
  acidentes %>%
    mutate(
      num_end_acidente = if_else(
        num_end_acidente == "00000",
        NA,
        num_end_acidente
      ),
      across(c(
        tp_rodovia,
        tp_cruzamento,
        ind_acostamento,
        ind_cantcentral,
        ind_guardrail,
        tp_pista,
        lim_velocidade,
        tp_curva,
        tp_acidente,
        fase_dia,
        cond_meteorologica,
        cond_pista,
        tp_pavimento,
      ), ~if_else(.x == "NAO INFORMADO", NA, .x)),
      cep_acidente = if_else(cep_acidente == "00000000", NA, cep_acidente),
      hora_acidente = if_else(hora_acidente == "999999", NA, hora_acidente)
    ) %>%
    select(-c(
      num_end_acidente,
      km_via_acidente,
      tp_rodovia,
      tp_cruzamento,
      tp_curva,
      tp_pista,
      ind_guardrail,
      ind_cantcentral,
      ind_acostamento
    ))
}

# Aqui mesma coisa - da pra arrumar `vitimas` com as funções do `tidyverse`

arrange_vitimas <- function() {
  vitimas %>%
    mutate(across(c(
      susp_alcool,
      equip_seguranca,
      gravidade_lesao,
      tp_envolvido,
      genero,
      faixa_idade
    ), ~if_else(.x == "NAO INFORMADO", NA, .x)))
}
