library(readxl)
## Leitura dos dados, compilados das bases da CAPES e calculados a partir da portaria
## N de bolsas por programa em 2018 (https://geocapes.capes.gov.br/geocapes/)
bolsas.18 <- read_excel("bolsas2108.xlsx") %>% as.data.frame()
names(bolsas.18) <- gsub(" ", ".", names(bolsas.18)) 
## Dados dos programas PROEX (https://dadosabertos.capes.gov.br/)
## com cotas de bolsas calculadas pela portaria 21 (ou seja, aplicacao dos Anexos I a III)
proex.m <- read.csv2("mestrados_em_2018_PROEX_com_media_titulados_idh_cotas_portarias_capes.csv", as.is=TRUE)
proex.d <- read.csv2("doutorados_em_2018_PROEX_com_media_titulados_idh_cotas_portarias_capes.csv", as.is=TRUE)
## Adiciona o numero de bolsas PROEX aos dados dos programas
proex.m2 <- merge(proex.m,
      bolsas.18[bolsas.18$Programa.Fomento == "PROEX",c("Código.Programa", "MESTRADO")],
      by.x = "CD_PROGRAMA_IES", by.y = "Código.Programa")
names(proex.m2)[names(proex.m2)=="MESTRADO"] <- "bolsas.em.2018"
proex.d2 <- merge(proex.d,
      bolsas.18[bolsas.18$Programa.Fomento == "PROEX",c("Código.Programa", "DOUTORADO.PLENO")],
      by.x = "CD_PROGRAMA_IES", by.y = "Código.Programa")
names(proex.d2)[names(proex.d2)=="DOUTORADO.PLENO"] <- "bolsas.em.2018"
## Calcula estimativa da cota de bolsas em marco de 2020
## Esta estimativa supoe que o n de bolsas em fev de 2020 era a mesma de dezembro de 2018,
## que é o ultimo dado disponivel na CAPES. Deve ser uma boa aproximacao para os programas PROEX,
## que nao tiveram recolhimentos de bolsas CAPES em 2019
proex.m2$est.bolsas.2020 <- with(proex.m2,
                                 portaria.limites.v(cota = cota.portaria, bolsas.antes = bolsas.em.2018, CD_CONCEITO_CURSO))
proex.d2$est.bolsas.2020 <- with(proex.d2,
                                 portaria.limites.v(cota = cota.portaria, bolsas.antes = bolsas.em.2018, CD_CONCEITO_CURSO))

## Salva as planilhas
write.csv2(proex.m2, file="mestrados_proex_estimativa_variacao_bolsas.csv")
write.csv2(proex.d2, file="doutorados_proex_estimativa_variacao_bolsas.csv")

