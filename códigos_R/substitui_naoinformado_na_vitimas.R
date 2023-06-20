library('tidyverse')
Vitimas <- Vitimas_DadosAbertos_20230512
rm(Vitimas_DadosAbertos_20230512)
prop.table(table(Vitimas$genero))
prop.table(table(Vitimas$susp_alcool))
prop.table(table(Vitimas$qtde_obitos))

sort(table(Acidentes1$fase_dia), decreasing= TRUE)           

sort(table(Vitimas$qtde_obitos), decreasing = TRUE)



Vitimas$susp_alcool <- replace(Vitimas$susp_alcool, 
                               Vitimas$susp_alcool == 'NAO INFORMADO', NA)


Vitimas$equip_seguranca <- replace(Vitimas$equip_seguranca, 
                               Vitimas$equip_seguranca == 'NAO INFORMADO', NA)

Vitimas$gravidade_lesao <- replace(Vitimas$gravidade_lesao,
                                   Vitimas$gravidade_lesao == 'NAO INFORMADO', NA)

Vitimas$tp_envolvido <- replace(Vitimas$tp_envolvido,
                                   Vitimas$tp_envolvido == 'NAO INFORMADO', NA)

Vitimas$genero <- replace(Vitimas$genero,
                                Vitimas$genero == 'NAO INFORMADO', NA)

Vitimas$faixa_idade<- replace(Vitimas$faixa_idade,
                          Vitimas$faixa_idade == 'NAO INFORMADO', NA)

write.csv(Vitimas, file = 'Vitimas1.csv', row.names = FALSE)
2+2
