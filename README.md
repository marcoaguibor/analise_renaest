# Análise dos dados do RENAEST

## Introdução

O RENAEST (Registro Nacional de Acidentes e Estatísticas de Trânsito) foi criado em 2006, a partir da resolução nº 208 do CONTRAN (Conselho Nacional de Trânsito). O Registro conta com dados sobre data, local e tipo de sinistro de trânsito, além de informações sobre as características das vítimas, como sexo, faixa etária e quantidade de envolvidos. O RENAEST é uma base de dados pública, que pode ser acessado em <https://www.gov.br/infraestrutura/pt-br/assuntos/transito/conteudo-Senatran/registro-nacional-de-acidentes-e-estatisticas-de-transito>

## Objetivo

O objetivo do código incluído neste repositório é analisar a qualidade dos dados que estão sendo fornecidos pelo RENAEST de forma aberta.

## Estrutura

.
├── R
│   ├── 00_main.R
│   ├── 01_organizacao_dados.R
│   ├── 02_tabelas_perc_nas.R
│   ├── 03_graficos_pna_sinistros_cv.R
│   └── 04_export.R
├── data
│   ├── acidentes.rda
│   └── vitimas.rda
├── data-raw
│   ├── acidentes.R
│   └── vitimas.R
└── plot
