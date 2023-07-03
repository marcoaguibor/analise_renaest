# Uma boa estrutura para se seguir em projetos é sempre criar um arquivo
# `main.R` que carrega todas os dados e bibliotecas necessarias e cria o link
# entre outros scripts do projeto, assim mantendo uma raiz. Isso facilita na
# execução: quando precisar rodar o codigo, é só rodar o `main.R`

# setup -------------------------------------------------------------------

library(tidyverse)

source("R/organizacao_dados.R")

# load --------------------------------------------------------------------

load("data/acidentes.rda")
load("data/vitimas.rda")

# arrange -----------------------------------------------------------------

acidentes_limpo <- arrange_acidentes()
vitimas_limpo <- arrange_vitimas()
