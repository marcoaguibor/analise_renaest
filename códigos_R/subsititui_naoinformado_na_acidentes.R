library('tidyverse')

Acidentes <- Acidentes_DadosAbertos_20230512
# Cria uma função para separar o conjunto de dados por ano

a <- function(ano){
  if (ano > 2022 | ano < 2018){
    stop('Ano inválido')
  }
  d <- Acidentes %>% 
              filter(ano_acidente == ano)
  return(d)
}

table(Acidentes$tp_rodovia)

# ------------------------------------------------------------------------------ #
# Arrumar os dados, colocando os devidos NA

Acidentes$num_end_acidente <- replace(Acidentes$num_end_acidente, 
                                      Acidentes$num_end_acidente == '00000', NA)

#--------------------------------------------------------------------------#
Acidentes$tp_rodovia <- replace(Acidentes$tp_rodovia, 
                                Acidentes$tp_rodovia == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#

Acidentes$tp_cruzamento <- replace(Acidentes$tp_cruzamento, 
                                Acidentes$tp_cruzamento == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#

Acidentes$ind_acostamento <- replace(Acidentes$ind_acostamento, 
                                   Acidentes$ind_acostamento == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#

Acidentes$ind_cantcentral <- replace(Acidentes$ind_cantcentral, 
                                     Acidentes$ind_cantcentral == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#
Acidentes$ind_guardrail <- replace(Acidentes$ind_guardrail, 
                                     Acidentes$ind_guardrail == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#

Acidentes$tp_pista <- replace(Acidentes$tp_pista, 
                                   Acidentes$tp_pista == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#

Acidentes$lim_velocidade <- replace(Acidentes$lim_velocidade, 
                              Acidentes$lim_velocidade == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#

Acidentes$tp_curva <- replace(Acidentes$tp_curva,
                              Acidentes$tp_curva == 'NAO INFORMADO', NA)
#--------------------------------------------------------------------------#

Acidentes$cep_acidente <- replace(Acidentes$cep_acidente,
                                  Acidentes$cep_acidente == '00000000', NA)
#--------------------------------------------------------------------------#

Acidentes$tp_acidente <- replace(Acidentes$tp_acidente,
                              Acidentes$tp_acidente == 'NAO INFORMADO', NA)
sort(prop.table(table(Acidentes$tp_acidente, useNA = 'always')))
#--------------------------------------------------------------------------#

Acidentes$fase_dia <- replace(Acidentes$fase_dia,
                              Acidentes$fase_dia == 'NAO INFORMADO', NA)
prop.table(table(Acidentes$fase_dia, useNA = 'always')) * 100
#--------------------------------------------------------------------------#

Acidentes$cond_meteorologica <- replace(Acidentes$cond_meteorologica,
                                 Acidentes$cond_meteorologica == 'NAO INFORMADO', NA)
sort(prop.table(table(Acidentes$cond_meteorologica, useNA = 'always')))
#--------------------------------------------------------------------------#

table(is.na(Acidentes$end_acidente)) #Confere que end_acidente já tem NAs
table(is.na(Acidentes$bairro_acidente)) #Confere que bairro já tem NAs

#--------------------------------------------------------------------------#

sort(table(Acidentes$hora_acidente),decreasing = TRUE) #Coloca o 999999 como possível NA
Acidentes$hora_acidente <- replace(Acidentes$hora_acidente,
                                   Acidentes$hora_acidente == '999999', NA)
prop.table(table(is.na(Acidentes$hora_acidente)))

#--------------------------------------------------------------------------#
prop.table(table(is.na(Acidentes$cond_pista)))
Acidentes$cond_pista <- replace(Acidentes$cond_pista,
                                        Acidentes$cond_pista == 'NAO INFORMADO', NA)

#--------------------------------------------------------------------------#

table(Acidentes$tp_pavimento, useNA = 'always')
prop.table(table(is.na(Acidentes$tp_pavimento)))
Acidentes$tp_pavimento <- replace(Acidentes$tp_pavimento,
                                Acidentes$tp_pavimento == 'NAO INFORMADO', NA)

#--------------------------------------------------------------------------#
table(Acidentes$qtde_acid_com_obitos, useNA = 'always') # 0 = não, 1 = sim
table(Acidentes$qtde_envolvidos, useNA = 'always') # O que significa 0?
table(Acidentes$qtde_feridosilesos, useNA = 'always')
table(Acidentes$qtde_obitos, useNA = 'always')

#--------------------------------------------------------------------------#
sort(table(Acidentes$num_acidente), decreasing =  FALSE)
#Salva o arquivo Acidentes no local dado pelo getwd() 

write.csv(Acidentes, file = 'Acidentes1.csv', row.names = FALSE)
getwd()
2+2

rm(Acidentes)

