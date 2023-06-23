library('tidyverse')

Acidentes <- read.csv2('data/acidentes_jan23')

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
table(Acidentes$qtde_acid_com_obitos, useNA = 'always') 
table(Acidentes$qtde_envolvidos, useNA = 'always') 
table(Acidentes$qtde_feridosilesos, useNA = 'always')
table(Acidentes$qtde_obitos, useNA = 'always')

#--------------------------------------------------------------------------#

#Funcao que tira os campos não essenciais do database Acidentes
tira_ne <- function(dados){
  dados$num_end_acidente <- NULL
  dados$km_via_acidente <- NULL
  dados$tp_rodovia <- NULL
  dados$tp_cruzamento <- NULL
  dados$tp_curva <- NULL
  dados$tp_pista <- NULL
  dados$ind_guardrail <- NULL
  dados$ind_cantcentral <- NULL
  dados$ind_acostamento <- NULL
  return(dados)
}

Acidentes <- tira_ne(Acidentes)
#Salvar o arquivo .csv como Acidentes1

Vitimas1 <- read.csv2('data/vitimas_jan23')

#------------------------------------------------------------------------------#
Vitimas$susp_alcool <- replace(Vitimas$susp_alcool, 
                               Vitimas$susp_alcool == 'NAO INFORMADO', NA)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
Vitimas$equip_seguranca <- replace(Vitimas$equip_seguranca, 
                                   Vitimas$equip_seguranca == 'NAO INFORMADO', NA)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
Vitimas$gravidade_lesao <- replace(Vitimas$gravidade_lesao,
                                   Vitimas$gravidade_lesao == 'NAO INFORMADO', NA)
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
Vitimas$tp_envolvido <- replace(Vitimas$tp_envolvido,
                                Vitimas$tp_envolvido == 'NAO INFORMADO', NA)
#-------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
Vitimas$genero <- replace(Vitimas$genero,
                          Vitimas$genero == 'NAO INFORMADO', NA)
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
Vitimas$faixa_idade<- replace(Vitimas$faixa_idade,
                              Vitimas$faixa_idade == 'NAO INFORMADO', NA)
#-------------------------------------------------------------------------------#

#Salvar o arquivo .csv como Vitimas1