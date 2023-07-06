# Uma boa estrutura para se seguir em projetos é sempre criar um arquivo
# `main.R` que carrega todas os dados e bibliotecas necessarias e cria o link
# entre outros scripts do projeto, assim mantendo uma raiz. Isso facilita na
# execução: quando precisar rodar o codigo, é só rodar o `main.R`

# setup -------------------------------------------------------------------

library(tidyverse)

source("R/organizacao_dados.R")
source("R/tabelas_perc_nas.R")
# source("R/graficos_pna_sinistros_cv.R")

# load --------------------------------------------------------------------

load("data/acidentes.rda")
load("data/vitimas.rda")

# arrange -----------------------------------------------------------------

acidentes <- arrange_acidentes()
vitimas <- arrange_vitimas()

# table -------------------------------------------------------------------

acidentes_na <- calc_na(acidentes)
vitimas_na <- calc_na(vitimas)

## Com o uso do `map` foi possivel criar um loop para calcular o NA
## considerando cada UF

acidentes_na_uf <- map(unique(acidentes$uf_acidente), ~calc_na(acidentes, .x))
vitimas_na_uf <- map(unique(vitimas$uf_acidente), ~calc_na(vitimas, .x))

acidentes_cv <- map(unique(acidentes$uf_acidente), ~calc_cv(acidentes, .x))
