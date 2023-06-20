library('tidyverse')

#Separa os dados por UF
dados_por_uf <- function(uf){
  d <- Vitimas1 %>% filter(uf_acidente == uf )
  return(d)
}

#Retorna o % de NAs por variável no conjunto Vítimas, em um vetor. Necessário transformar
# em fator, pois a funçao prop.table do is.na retornará apenas um valor caso seja 100% ou 0% de
# NAs. Assim, em fator, a funão retornará dois valores, um para TRUE e outro para FALSE.
sep_var_ufv <- function(uf){
  dados <- dados_por_uf(uf)
  eq_seg <- as.numeric(prop.table(table(factor(is.na(dados$equip_seguranca), levels = c('FALSE', 'TRUE')))))[2]
  susp_alcool <- as.numeric(prop.table(table(factor(is.na(dados$susp_alcool), levels = c('FALSE', 'TRUE')))))[2]
  gravidade_lesao <- as.numeric(prop.table(table(factor(is.na(dados$gravidade_lesao), levels = c('FALSE', 'TRUE')))))[2]
  faixa_idade <- as.numeric(prop.table(table(factor(is.na(dados$faixa_idade), levels = c('FALSE', 'TRUE')))))[2]
  tp_envolvido <- as.numeric(prop.table(table(factor(is.na(dados$tp_envolvido), levels = c('FALSE', 'TRUE')))))[2]
  genero <- as.numeric(prop.table(table(factor(is.na(dados$genero), levels = c('FALSE', 'TRUE')))))[2]
  
  return(c(eq_seg, susp_alcool, gravidade_lesao, faixa_idade, tp_envolvido, genero))
}

#Retorna um dataframe com % de nas por UF para cada variável. Usado na tabela 3.
tabela_cvv <- function(){
  df = data.frame(uf = character(),
                  equip_seguranca = numeric(),
                  susp_alcool = numeric(),
                  gravidade_lesao = numeric(),
                  faixa_idade = numeric(),
                  tp_envolvido = numeric(),
                  genero = numeric())
  uf <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG',
          'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS',
          'RO', 'RR', 'SC', 'SP', 'SE', 'TO')
  for (i in 1:27){
    dados <- sep_var_ufv(uf[i])
    df <- df %>% add_row(uf = uf[i], 
                         equip_seguranca = round(dados[1]*100, 2),
                         susp_alcool = round(dados[2]*100, 2),
                         gravidade_lesao = round(dados[3]*100, 2),
                         faixa_idade = round(dados[4]*100, 2),
                         tp_envolvido = round(dados[5]*100, 2),
                         genero = round(dados[6]*100, 2))
  }
  return(df)
}
tabela <- tabela_cvv()
2+2
tabela  
