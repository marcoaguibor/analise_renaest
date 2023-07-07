# Uma boa estrutura para se seguir em projetos é sempre criar um arquivo
# `main.R` que carrega todas os dados e bibliotecas necessarias e cria o link
# entre outros scripts do projeto, assim mantendo uma raiz. Isso facilita na
# execução: quando precisar rodar o codigo, é só rodar o `main.R`

# setup -------------------------------------------------------------------

library(tidyverse)
library(showtext)
library(gghighlight)

source("R/organizacao_dados.R")
source("R/tabelas_perc_nas.R")
#source("R/graficos_pna_sinistros_cv.R")

font_add_google(name = "Fira Sans", family = "firasans")
showtext_auto()

# load --------------------------------------------------------------------

load("data/acidentes.rda")
load("data/vitimas.rda")

# arrange -----------------------------------------------------------------

acidentes <- arrange_acidentes()
vitimas <- arrange_vitimas()

# table -------------------------------------------------------------------

# Com o uso do `map` foi possivel criar um loop para calcular o NA
# considerando cada UF e o total do BR

lista_uf <- unique(acidentes$uf_acidente) # Lista dos UF

acidentes_na_uf <- map(c("BR", lista_uf), ~calc_na(acidentes, .x))
vitimas_na_uf <- map(c("BR", lista_uf), ~calc_na(vitimas, .x))

# Aqui o `map` ajudou a calcular o cv para cada estado tambem

acidentes_cv <- map(lista_uf, ~calc_cv(acidentes, .x))
acidentes_cv_br <- calc_cv(acidentes)

# tabela com cv de todos uf e BR

total_cv <- join_cv(acidentes_cv, acidentes_cv_br)

# Tabela com as medias de pna por estado e BR

pna_mean_acidentes <- calc_mean_pna(acidentes_na_uf)
pna_mean_vitimas <- calc_mean_pna(vitimas_na_uf)

# plot --------------------------------------------------------------------

grafico_pna_acidentes_uf <- map(acidentes_na_uf, plot_pna)
grafico_pna_vitimas_uf <- map(vitimas_na_uf, plot_pna)

grafico_cv <- map(acidentes_cv, plot_cv_sinistros)

grafico_cv_total <- plot_cv_total(total_cv)

grafico_pna_acidentes_mean <- plot_pna_mean(pna_mean_acidentes)
grafico_pna_vitimas_mean <- plot_pna_mean(pna_mean_vitimas)
