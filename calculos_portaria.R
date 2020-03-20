source("funcoes.R")
library(ggplot2)
library(tidyr)
library(readxl)
################################################################################
## Leitura dos dados
################################################################################
## Dados dos  discentes 
## Do site de dados abertos da CAPES
disc.2015 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/dc2568b7-20b0-4d92-980d-dcf2485b5517/resource/08e7765f-cd76-4c7b-a29a-46e216dd79cf/download/br-capes-colsucup-discentes-2013a2016-2017-12-02_2015.csv", as.is=TRUE, encoding = "latin1")

disc.2016 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/dc2568b7-20b0-4d92-980d-dcf2485b5517/resource/cfbcb060-d6af-4c34-baa7-16ef259273f7/download/br-capes-colsucup-discentes-2013a2016-2017-12-02_2016.csv", as.is = TRUE, encoding = "latin1")

disc.2017 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/b7003093-4fab-4b88-b0fa-b7d8df0bcb77/resource/2207af02-21f6-466e-a690-46f26a2804d6/download/ddi-br-capes-colsucup-discentes-2017-2018-07-01.csv", as.is = TRUE, encoding = "latin1")

disc.2018 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/b7003093-4fab-4b88-b0fa-b7d8df0bcb77/resource/37fde9f4-bb94-4806-85d4-5d744f7f76ef/download/br-capes-colsucup-discentes-2018-2019-10-01.csv", as.is = TRUE, encoding = "latin1")

## Dados docentes dos cursos
## Do site de dados abertos da CAPES
doc.2015 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/35eab2f8-5a64-4619-b3f1-63a2e6690cfa/resource/75eea9d5-1542-4cfd-8ed9-d540d3eef344/download/br-capes-colsucup-docente-2013a2016-2017-12-02_2015.csv", as.is = TRUE, encoding = "latin1")

doc.2016 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/35eab2f8-5a64-4619-b3f1-63a2e6690cfa/resource/922bc0d1-90eb-4939-9167-03831f732f72/download/br-capes-colsucup-docente-2013a2016-2017-12-02_2016.csv", as.is = TRUE, encoding = "latin1")

doc.2017 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/57f86b23-e751-4834-8537-e9d33bd608b6/resource/d918d02e-7180-4c7c-be73-980f9a8c09b5/download/br-capes-colsucup-docente-2017-2018-08-10.csv", as.is = TRUE, encoding = "latin1")

doc.2018 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/57f86b23-e751-4834-8537-e9d33bd608b6/resource/08d2a6bd-ac0f-4c25-ab89-8457288e15a6/download/br-capes-colsucup-docente-2018-2019-10-01.csv", as.is = TRUE, encoding = "latin1")

## N medio de docentes no periodo 2015-2018
docentes.15.18 <- rbind( f2(doc.2015), f2(doc.2016), f2(doc.2017), f2(doc.2018)) %>%
    group_by(CD_PROGRAMA_IES) %>%
    summarise(media.docentes = mean(n.doc), media.permanentes = mean(n.perm)) %>%
    as.data.frame

## Tabelas de colegios por nome das areas
colegios <- data.frame(NM_GRANDE_AREA_CONHECIMENTO = sort(unique(disc.2015$NM_GRANDE_AREA_CONHECIMENTO)), colegio = NA)
colegios$colegio[1:3] <- "Vida"
colegios$colegio[c(4,7,9)] <- "Exatas"
colegios$colegio[c(5,6,8)] <- "Humanidades"

## Programas ativos em 2018
progr.18 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/bdaf1399-29ae-4920-b74f-513f11dbed68/resource/44680e7b-ec1c-433f-b9a5-288ce72d8073/download/br-capes-colsucup-curso-2018-2019-10-01.csv", as.is=TRUE, encoding = "latin1")

## tabela IDHM (https://www.br.undp.org/content/brazil/pt/home/idh0/rankings/idhm-municipios-2010.html)
idhm <- read.csv2("IDHM_municipios_2010.csv", as.is=TRUE, row.names=1)
## Nao e necessario, mas separei nome do municipio e sigla do estado em variaveis
idhm$sigla.estado <- substring(rownames(idhm),
                               first = unlist(gregexpr(pattern ="\\(",rownames(idhm)))+1,
                               last = unlist(gregexpr(pattern ='\\(',rownames(idhm)))+2)
idhm$nome.municipio <- substring(rownames(idhm),
                               first = 1,
                               last = unlist(gregexpr(pattern ='\\(',rownames(idhm)))-2)
## Converte nomes das linhas em maiusculas, para cruzar com a tabela da CAPES
rownames(idhm) <- toupper(rownames(idhm))
idhm <- idhm[, c("nome.municipio", "sigla.estado", "idhm")]
## Acrescenta idhm do dataframe acima à tabela dos programas ativos em 2018
progr.18$municipio.estado <- paste(progr.18$NM_MUNICIPIO_PROGRAMA_IES, " (",
                                   progr.18$SG_UF_PROGRAMA,")", sep="")
progr.18$idhm <- idhm[progr.18$municipio.estado, "idhm"]

################################################################################
## Adiciona N de bolsas em 2018
################################################################################
## Leitura dos dados, compilados das bases da CAPES e calculados a partir da portaria
## N de bolsas por programa em 2018 (https://geocapes.capes.gov.br/geocapes/)
## Clique em "Visao analitica", então em "Área de avalicão" e então na tabela na única área de avaliação que aparece que é "INDEFINIDA".
## Isso vai gerar uma tabela de bolsas por programas, que entendo que é para todos os programas.
## Já fiz isso e importei a tabela para xlsx, que vai em anexo.
bolsas.18 <- as.data.frame( read_excel("bolsas2108.xlsx") )
## Muda nomes para facilitar manipulação
names(bolsas.18) <- gsub(" ", ".", names(bolsas.18))
## Seleciona apenas bolsas DS, PROEX, PROSUC, PROSUP
## Mestrados
bolsas.mestr.18.programas <-
    bolsas.18 %>%
    select(Código.Programa, Programa.Fomento, MESTRADO) %>%
    filter(Programa.Fomento %in% c("DS", "PROEX", "PROSUC", "PROSUP")& !is.na(Código.Programa)) %>%
    group_by(Código.Programa, Programa.Fomento) %>%
    summarise(mestrado = sum(MESTRADO)) %>%
    ungroup() %>%
    pivot_wider(names_from = Programa.Fomento, values_from = mestrado) %>%
    mutate(n.bolsas.18.DS = ifelse(is.na(DS), 0 , DS),
           n.bolsas.18.PROEX = ifelse(is.na(PROEX), 0, PROEX),
           n.bolsas.18.PROSUP = ifelse(is.na(PROSUP), 0, PROSUP),
           n.bolsas.18.PROSUC = ifelse(is.na(PROSUC), 0, PROSUC),
           n.bolsas.18 = n.bolsas.18.DS + n.bolsas.18.PROEX + n.bolsas.18.PROSUC) %>%
    select(Código.Programa, n.bolsas.18.DS:n.bolsas.18) %>%
    as.data.frame()
## Doutorados
bolsas.dout.18.programas <-
    bolsas.18 %>%
    select(Código.Programa, Programa.Fomento, DOUTORADO.PLENO) %>%
    filter(Programa.Fomento %in% c("DS", "PROEX", "PROSUC", "PROSUP")& !is.na(Código.Programa)) %>%
    group_by(Código.Programa, Programa.Fomento) %>%
    summarise(doutorado = sum(DOUTORADO.PLENO)) %>%
    ungroup() %>%
    pivot_wider(names_from = Programa.Fomento, values_from = doutorado) %>%
    mutate(n.bolsas.18.DS = ifelse(is.na(DS), 0 , DS),
           n.bolsas.18.PROEX = ifelse(is.na(PROEX), 0, PROEX),
           n.bolsas.18.PROSUP = ifelse(is.na(PROSUP), 0, PROSUP),
           n.bolsas.18.PROSUC = ifelse(is.na(PROSUC), 0, PROSUC),
           n.bolsas.18 = n.bolsas.18.DS + n.bolsas.18.PROEX + n.bolsas.18.PROSUC) %>%
    select(Código.Programa, n.bolsas.18.DS:n.bolsas.18) %>%
    as.data.frame()

################################################################################
## Adiciona N medio de bolsas em 2015-2018
################################################################################
## Media de bolsas de mestado e doutorado dos programas DS, PROSUC, PROSSUP e PROEX
## Objetos com n de bolsas em cada ano (ver funcao "junta.bolsas" em funcoes.R)
bolsas.dm.15 <- junta.bolsas(dir = "/home/paulo/work/pg_usp/coordenacao/CAPES/dados/geocapes/bolsas/",
                             padrao = "2015_")
bolsas.dm.16 <- junta.bolsas(dir = "/home/paulo/work/pg_usp/coordenacao/CAPES/dados/geocapes/bolsas/",
                             padrao = "2016_")
bolsas.dm.17 <- junta.bolsas(dir = "/home/paulo/work/pg_usp/coordenacao/CAPES/dados/geocapes/bolsas/",
                             padrao = "2017_")
bolsas.dm.18 <- junta.bolsas(dir = "/home/paulo/work/pg_usp/coordenacao/CAPES/dados/geocapes/bolsas/",
                          padrao = "2018_")
## Junta todos e calcula media de n de bolsas no periodo
bolsas.dm.15.18 <-
    rbind(bolsas.dm.15, bolsas.dm.16, bolsas.dm.17, bolsas.dm.18) %>%
    group_by(Código.Programa) %>%
    summarise(m.bolsas.mestr = mean(n.bolsas.m), m.bolsas.dout = mean(n.bolsas.d)) %>%
    as.data.frame()


################################################################################
### Calculos de n bolsas pelas portarias ###
################################################################################
## Calculo de n e media de titulados por programa, no periodo 2015-2018 ##
## Mestrado
mestrados.15.18 <- rbind( f1(disc.2015), f1(disc.2016), f1(disc.2017), f1(disc.2018)) %>%
    group_by(CD_PROGRAMA_IES,NM_GRANDE_AREA_CONHECIMENTO) %>%
    summarise(n.discentes = sum(n.disc), media.discentes = mean(n.disc),
              n.titulados = sum(n.tit), media.titulados = mean(n.tit)) %>%
    ungroup() %>%
    left_join(colegios)
## Doutorados
doutorados.15.18 <- rbind( f1(disc.2015, titulo="DOUTORADO"), f1(disc.2016, titulo="DOUTORADO"),
           f1(disc.2017, titulo="DOUTORADO"), f1(disc.2018, titulo="DOUTORADO")) %>%
    group_by(CD_PROGRAMA_IES,NM_GRANDE_AREA_CONHECIMENTO) %>%
    summarise(n.discentes = sum(n.disc), media.discentes = mean(n.disc),
              n.titulados = sum(n.tit), media.titulados = mean(n.tit)) %>%
    ungroup() %>%
    left_join(colegios)

## Junta dados dos programas e de docentes ao dataframe com os cálculos criados acima ## 
mestrados.15.18.id <- merge(
    merge(progr.18[progr.18$NM_GRAU_CURSO=="MESTRADO",],docentes.15.18, by = "CD_PROGRAMA_IES"),
    mestrados.15.18, by = "CD_PROGRAMA_IES"
)

doutorados.15.18.id <- merge(
    merge(progr.18[progr.18$NM_GRAU_CURSO=="DOUTORADO",],docentes.15.18, by = "CD_PROGRAMA_IES", all.x=TRUE),
    doutorados.15.18, by = "CD_PROGRAMA_IES"
)

## Faixas de IDH de cada curso (Anexos II das portarias)
mestrados.15.18.id$idhm.faixa <- cut(mestrados.15.18.id$idhm, breaks = c(0, 0.599, 0.699, 0.799, 1),
                                     labels = c("Baixo", "Medio", "Alto", "Muito Alto") )
doutorados.15.18.id$idhm.faixa <- cut(doutorados.15.18.id$idhm, breaks = c(0, 0.599, 0.699, 0.799, 1),
                                     labels = c("Baixo", "Medio", "Alto", "Muito Alto") )
## Faixas de N medio de titulados (Anexo III)
## mestrados
mestrados.15.18.id$tmc.faixa[mestrados.15.18.id$colegio == "Vida"] <-
    cut(mestrados.15.18.id$media.titulados[mestrados.15.18.id$colegio == "Vida"],
        c(-1, 7.545, 19.965, 32.385, 500),
        labels = c("Baixa", "Media", "Alta", "Muito Alta"))
mestrados.15.18.id$tmc.faixa[mestrados.15.18.id$colegio == "Exatas"] <-
    cut(mestrados.15.18.id$media.titulados[mestrados.15.18.id$colegio == "Exatas"],
        c(-1, 5.982, 24.823, 43.664, 500),
        labels = c("Baixa", "Media", "Alta", "Muito Alta"))
mestrados.15.18.id$tmc.faixa[mestrados.15.18.id$colegio == "Humanidades"] <-
    cut(mestrados.15.18.id$media.titulados[mestrados.15.18.id$colegio == "Humanidades"],
        c(-1, 5.778, 29.123, 52.469, 500),
        labels = c("Baixa", "Media", "Alta", "Muito Alta"))
mestrados.15.18.id$tmc.faixa <- factor(mestrados.15.18.id$tmc.faixa, levels=1:4, labels=c("Baixa", "Media", "Alta", "Muito Alta"))
## Doutorados
doutorados.15.18.id$tmc.faixa[doutorados.15.18.id$colegio == "Vida"] <-
    cut(doutorados.15.18.id$media.titulados[doutorados.15.18.id$colegio == "Vida"],
        c(-1, 5.299, 16.702, 28.104, 500),
        labels = c("Baixa", "Media", "Alta", "Muito Alta"))
doutorados.15.18.id$tmc.faixa[doutorados.15.18.id$colegio == "Exatas"] <-
    cut(doutorados.15.18.id$media.titulados[doutorados.15.18.id$colegio == "Exatas"],
        c(-1, 3.118, 19.119, 35.120, 500),
        labels = c("Baixa", "Media", "Alta", "Muito Alta"))
doutorados.15.18.id$tmc.faixa[doutorados.15.18.id$colegio == "Humanidades"] <-
    cut(doutorados.15.18.id$media.titulados[doutorados.15.18.id$colegio == "Humanidades"],
        c(-1, 2.566, 23.270, 43.974, 500),
        labels = c("Baixa", "Media", "Alta", "Muito Alta"))
doutorados.15.18.id$tmc.faixa <- factor(doutorados.15.18.id$tmc.faixa, levels=1:4, labels=c("Baixa", "Media", "Alta", "Muito Alta"))

## Calcula N de bolsas atribuidas a cada programa pela portaria, sem levar em conta os limites
## de perda ou acréscimo máximo em relação oa numero atual
## Este valor serve para projetar a política no longo prazo, nao é o numero de bolsa concedido ao peograma em 2020 ##
### Bolsas de mestrado
mestrados.15.18.id$cota.portaria <- with(mestrados.15.18.id,
                                      portaria.mestr.v(colegio = colegio, conceito = as.integer(CD_CONCEITO_CURSO),
                                                       idh = idhm, tmc = media.titulados))
### Bolsas de doutorado
doutorados.15.18.id$cota.portaria <- with(doutorados.15.18.id,
                                      portaria.dout.v(colegio = colegio, conceito = as.integer(CD_CONCEITO_CURSO),
                                                      idh = idhm, tmc = media.titulados))
## Junta numero de bolsas DS, PROEX, PROSUC e PROSUP de cada programa em 2018
mestrados.15.18.id <- merge(mestrados.15.18.id, bolsas.mestr.18.programas,
                            by.x = "CD_PROGRAMA_IES", by.y = "Código.Programa",
                            all.x=TRUE)
doutorados.15.18.id <- merge(doutorados.15.18.id, bolsas.dout.18.programas,
                            by.x = "CD_PROGRAMA_IES", by.y = "Código.Programa",
                            all.x=TRUE)

## Junta media de bolsas entre 2015 e 2018
mestrados.15.18.id <- merge(mestrados.15.18.id, bolsas.dm.15.18[, -3],
                            by.x = "CD_PROGRAMA_IES", by.y = "Código.Programa", all.x = TRUE)

doutorados.15.18.id <- merge(doutorados.15.18.id, bolsas.dm.15.18[, -2],
              by.x = "CD_PROGRAMA_IES", by.y = "Código.Programa", all.x = TRUE)


### Apenas programas PROEX
mestrados.proex <- filter(mestrados.15.18.id, as.integer(CD_CONCEITO_CURSO) > 5)
doutorados.proex <- filter(doutorados.15.18.id, as.integer(CD_CONCEITO_CURSO) > 5)
## Verificando se o numero de programas PROEX confere com o informado na plataforma Sucupira
## https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/programa/quantitativos/quantitativoConceito.jsf;jsessionid=-3cbrx3zDA5dZAcvG+VrUme8.sucupira-213
## Mestrados: 303 cursos nota 6 e 183 cursos nota 7 no Sucupira
nrow(mestrados.proex) == 303 + 183
## Doutorados: 307 cursos nota 6 e 185 cursos nota 7
nrow(doutorados.proex) == 307 + 185

## Exporta para planilhas em csv
write.csv2(mestrados.15.18.id, file = "mestrados_em_2018_com_media_titulados_idh_cotas_portarias_capes.csv",
           row.names=FALSE)
write.csv2(doutorados.15.18.id, file = "doutorados_em_2018_com_media_titulados_idh_cotas_portarias_capes.csv" ,
           row.names=FALSE)
write.csv2(mestrados.proex, file = "mestrados_em_2018_PROEX_com_media_titulados_idh_cotas_portarias_capes.csv",
           row.names=FALSE)
write.csv2(doutorados.proex, file = "doutorados_em_2018_PROEX_com_media_titulados_idh_cotas_portarias_capes.csv",
           row.names=FALSE)





