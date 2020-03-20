library(dplyr)
library(ggplot2)

## Leitura dos dados: planilha de discentes no site de dados abertos da CAPES
## Do site de dados abertos da CAPES

disc.2015 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/dc2568b7-20b0-4d92-980d-dcf2485b5517/resource/08e7765f-cd76-4c7b-a29a-46e216dd79cf/download/br-capes-colsucup-discentes-2013a2016-2017-12-02_2015.csv", as.is=TRUE, encoding = "latin1")

disc.2016 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/dc2568b7-20b0-4d92-980d-dcf2485b5517/resource/cfbcb060-d6af-4c34-baa7-16ef259273f7/download/br-capes-colsucup-discentes-2013a2016-2017-12-02_2016.csv", as.is = TRUE, encoding = "latin1")

disc.2017 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/b7003093-4fab-4b88-b0fa-b7d8df0bcb77/resource/2207af02-21f6-466e-a690-46f26a2804d6/download/ddi-br-capes-colsucup-discentes-2017-2018-07-01.csv", as.is = TRUE, encoding = "latin1")

disc.2018 <- read.csv2("https://dadosabertos.capes.gov.br/dataset/b7003093-4fab-4b88-b0fa-b7d8df0bcb77/resource/37fde9f4-bb94-4806-85d4-5d744f7f76ef/download/br-capes-colsucup-discentes-2018-2019-10-01.csv", as.is = TRUE, encoding = "latin1")

## Tabelas de colegios por nome das areas
colegios <- data.frame(NM_GRANDE_AREA_CONHECIMENTO = sort(unique(disc.2015$NM_GRANDE_AREA_CONHECIMENTO)), colegio = NA)
colegios$colegio[1:3] <- "Vida"
colegios$colegio[c(4,7,9)] <- "Exatas"
colegios$colegio[c(5,6,8)] <- "Humanidades"

## Programas PROEX em 2018
id.proex <- unique(disc.2018$CD_PROGRAMA_IES[disc.2018$CD_CONCEITO_PROGRAMA > 5])

## Soma o n de titulos por programa
## Criei 3 funcoes que fazem calculos alternativos
## Considera todos os titulados de todos os programas
f1 <- function(x){
    x %>%
        filter(NM_SITUACAO_DISCENTE == "TITULADO") %>%
        group_by(CD_PROGRAMA_IES, NM_GRANDE_AREA_CONHECIMENTO) %>%
        summarise(n.mest = sum(DS_GRAU_ACADEMICO_DISCENTE=="MESTRADO"&
                               (NM_GRAU_PROGRAMA=="MESTRADO"|NM_GRAU_PROGRAMA=="MESTRADO/DOUTORADO")),
                  n.dout = sum(DS_GRAU_ACADEMICO_DISCENTE=="DOUTORADO"&
                  (NM_GRAU_PROGRAMA=="DOUTORADO"|NM_GRAU_PROGRAMA=="MESTRADO/DOUTORADO"))) %>%
        as.data.frame()
}
## Considera apenas os titulados dos programas PROEX de cada ano
## Acho pouco provavel e nao da diferencas em relacao a seguinte
f2 <- function(x){
    x %>%
        filter(NM_SITUACAO_DISCENTE == "TITULADO" & CD_CONCEITO_PROGRAMA > 5) %>%
        group_by( CD_PROGRAMA_IES, NM_GRANDE_AREA_CONHECIMENTO) %>%
        summarise(n.mest = sum(DS_GRAU_ACADEMICO_DISCENTE=="MESTRADO"&
                               (NM_GRAU_PROGRAMA=="MESTRADO"|NM_GRAU_PROGRAMA=="MESTRADO/DOUTORADO")),
                  n.dout = sum(DS_GRAU_ACADEMICO_DISCENTE=="DOUTORADO"&
                  (NM_GRAU_PROGRAMA=="DOUTORADO"|NM_GRAU_PROGRAMA=="MESTRADO/DOUTORADO"))) %>%
        as.data.frame()
}
## Considera os titulados apenas dos programas que sao PROEX em 2018 
f3 <- function(x){
    x %>%
        filter(NM_SITUACAO_DISCENTE == "TITULADO" & CD_PROGRAMA_IES %in% id.proex) %>%
        group_by( CD_PROGRAMA_IES, NM_GRANDE_AREA_CONHECIMENTO) %>%
        summarise(n.mest = sum(DS_GRAU_ACADEMICO_DISCENTE=="MESTRADO"&
                               (NM_GRAU_PROGRAMA=="MESTRADO"|NM_GRAU_PROGRAMA=="MESTRADO/DOUTORADO")),
                  n.dout = sum(DS_GRAU_ACADEMICO_DISCENTE=="DOUTORADO"&
                  (NM_GRAU_PROGRAMA=="DOUTORADO"|NM_GRAU_PROGRAMA=="MESTRADO/DOUTORADO"))) %>%
        as.data.frame()
}
## Uma funcao que junta os titulados filtrados de cada ano, soma este titulados por programa,
## e associa ao colegio.
## Argumento "funcao" define qual das funcoes acima sera usada para selecionar os programas
## dos quais serao feitas as totalizações de titulados
Fjunta <- function(funcao=c("f1", "f2", "f3")){
    Fx <- match.fun(funcao)
    rbind( Fx(disc.2015), Fx(disc.2016), Fx(disc.2017), Fx(disc.2018)) %>%
        group_by(CD_PROGRAMA_IES,NM_GRANDE_AREA_CONHECIMENTO) %>%
        summarise(N.mest = sum(n.mest), N.dout = sum(n.dout),
                  m.mest = mean(n.mest), m.dout = mean(n.dout),
                  N.mest.y = N.mest/4, N.dout.y = N.dout/4) %>%
        ungroup() %>%
        left_join(colegios)
}

## Calculos de totais e medias de titulados por curso
tit.15.18.all <- Fjunta("f1")
tit.15.18.proex <- Fjunta("f2")
tit.15.18.proex.em.18 <- Fjunta("f3")

## Calculo das medias e sds pelos tres metodos
## funcao para os calculos
## Medias das médias
f.mean <- function(x){
    x %>%
        group_by(colegio) %>%
        summarise(mean.mest = mean(m.mest), sd.mest = sd(m.mest),
                  mean.dout = mean(m.dout), sd.dout = sd(m.dout))
}
## Medias das razoes de n de titulados
f.mean.y <- function(x){
    x %>%
        group_by(colegio) %>%
        summarise(mean.mest = mean(N.mest.y), sd.mest = sd(N.mest.y),
                  mean.dout = mean(N.dout.y), sd.dout = sd(N.dout.y))
}
## Totais
f.tot <- function(x, log = FALSE){
    if(log){
    y <- x %>%
        group_by(colegio) %>%
        summarise(mean.N.mest = mean(log(N.mest)), sd.N.mest = sd(log(N.mest)),
                  mean.N.dout = mean(log(N.dout)), sd.N.dout = sd(log(N.dout)))
    }
    else{
        y <- x %>%
            group_by(colegio) %>%
            summarise(mean.N.mest = mean(N.mest), sd.N.mest = sd(N.mest),
                      mean.N.dout = mean(N.dout), sd.N.dout = sd(N.dout))
    }
    return(y)
}
## Histogramas: medias e totais com distribuição muito assimetrica
tit.15.18.all %>%
    ggplot(aes(m.mest))+
    geom_histogram() +
    facet_grid(~colegio)
hist.all.m <- tit.15.18.all %>%
    filter(colegio=="Vida") %>%
    ggplot(aes(m.mest))+
    geom_histogram() +
    ggtitle("Ciências da vida, mestrado, todos os programas")
hist.all.d <- tit.15.18.all %>%
    filter(colegio=="Vida") %>%
    ggplot(aes(m.dout))+
    geom_histogram()+
    ggtitle("Ciências da vida, doutorado, todos os programas")


tit.15.18.proex.em.18 %>%
    ggplot(aes(m.mest))+
    geom_histogram() +
    facet_grid(~colegio)
hist.proex.m <- tit.15.18.proex.em.18 %>%
    filter(colegio=="Vida") %>%
    ggplot(aes(m.mest))+
    geom_histogram()+
    ggtitle("Ciências da vida, mestrado, Programas PROEX")
hist.proex.d <- tit.15.18.proex.em.18 %>%
    filter(colegio=="Vida") %>%
    ggplot(aes(m.dout))+
    geom_histogram() +
    ggtitle("Ciências da vida, doutorado, Programas PROEX")

pdf(file = "Medias_das_medias_de_titulados%01d.pdf", onefile =FALSE)
print(hist.all.m)
print(hist.all.d)
print(hist.proex.m)
print(hist.proex.d)
dev.off()

## Medias e de desvio-padrão das médias anuais dos titulados de cada curso, por colegio ##
## (Nenhum deles bate com os valores do anexo III, principalmente com as médias)
## Usando titulados de todos os cursos:  media bem mais baixas que as do anexo III
f.mean(tit.15.18.all)
## Titulados de todos os cursos que eram PROEX em cada ano: médias mais altas a bem mais altas 
f.mean(tit.15.18.proex)
## Titulados apenas dos cursos que foram PROEX em 2018: muito parecido com o anterior
f.mean(tit.15.18.proex.em.18)

## Total de titulados de cada curso dividido por quatro: poderia dar diferente se muitos cursos foram criados no meior do período
## Usando titulados de todos os cursos:  media bem mais baixas que as do anexo III, principalmente de dout
f.mean.y(tit.15.18.all)
## Titulados de todos os cursos que eram PROEX em cada ano: médias mais altas a bem mais altas 
f.mean(tit.15.18.proex)
## Titulados apenas dos cursos que foram PROEX em 2018: muito parecido com o anterior
f.mean(tit.15.18.proex.em.18)

## Totais
## O mesmo para total de defesas no período 15-18: como era de se esperar, sao numero da ordem de quatro vezes os acima
## Usando titulados de todos os cursos:  media bem mais baixas que as do anexo III
f.tot(tit.15.18.all)
## Titulados de todos os cursos que eram PROEX em cada ano: médias mais altas a bem mais altas 
f.tot(tit.15.18.proex)
## Titulados apenas dos cursos que foram PROEX em 2018: muito parecido com o anterior
f.tot(tit.15.18.proex.em.18)



################################################################################
## Adendo:
## Entre os cursos há alguns com numeros maximos bem altos. Verifiquei abaixo o programa com o maior númeo 9Direito da USP)
summary(tit.15.18.all)
tit.15.18.all[tit.15.18.all$N.mest==max(tit.15.18.all$N.mest),]
max.m <- tit.15.18.all$CD_PROGRAMA_IES[tit.15.18.all$N.mest==max(tit.15.18.all$N.mest)]
## Verificando  o nome na planilha original: Direito na USP
unique(disc.2015[disc.2015$CD_PROGRAMA_IES==max.m, c("NM_ENTIDADE_ENSINO","NM_PROGRAMA_IES")])
## Investigando os mestrado de 2015 neste curso:
disc.2015 %>%
    filter(CD_PROGRAMA_IES==max.m&NM_SITUACAO_DISCENTE=="TITULADO"&DS_GRAU_ACADEMICO_DISCENTE=="MESTRADO") %>%
    nrow()
## 165 teses defendidas em 2015! Verifiquei na biblioteca digital da USP e há 150 teses da Faculdade de Direito este ano
## https://www.teses.usp.br/index.php?option=com_jumi&fileid=19&Itemid=87&lang=pt-br&g=3&b0=Faculdade%20de%20Direito&c0=u&o0=AND&b1=2015&c1=a&o1=AND
