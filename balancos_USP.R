source("funcoes.R")
library(ggplot2)
options(dplyr.width = Inf)

usp.proex <- read.csv2("balanco_bolsas_PROEX_USP_2020_revisado.csv", as.is=TRUE)
usp.ds <- read.csv2("balanco_bolsas_DS_USP_2020_revisado.csv", as.is=TRUE)
usp <- rbind(usp.proex,usp.ds)

usp.mestr <- merge(mestrados.15.18.id, usp[,-c(5,7,9) ], by.x = "CD_PROGRAMA_IES", by.y = "codigo.programa", all.y=TRUE)
usp.mestr <- usp.mestr[!is.na(usp.mestr$n.bolsas.18), ] ## retira cursos que só têm doutorado
usp.dout <- merge(doutorados.15.18.id, usp[,-c(4,6,8) ], by.x = "CD_PROGRAMA_IES", by.y = "codigo.programa", all.y=TRUE)
## verificando cursos que nao estao na lista da CAPES de 2018
usp.dout %>%
    filter(is.na(colegio)) %>%
    select(nome.programa)
usp.dout$colegio[is.na(usp.dout$colegio)] <- c("Humanidades", "Exatas", "Humanidades", "Exatas", "Exatas",
                                                                "Humanidades", "Humanidades", "Exatas", "Humanidades","Humanidades",
                                                                "Vida", "Humanidades", "Vida", "Humanidades", "Humanidades",
                                                                "Exatas", "Vida", "Exatas")

## Perdas em relacao a 2018 e 2020
## Totais
usp.mestr %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(m.fev.20, na.rm=TRUE),
              N.port = sum(m.portaria, na.rm=TRUE), N.cota = sum(cota.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.18 = N.port-N.18,  dif.port.20 = (N.port - N.20), dif.cota.port=(N.cota-N.port),
              difp.port.18= dif.port.18/N.18, difp.port.20 = dif.port.20/N.20, difp.port.cota = dif.cota.port/N.port)

usp.dout %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(d.fev.20, na.rm=TRUE),
              N.port = sum(d.portaria, na.rm=TRUE), N.cota = sum(cota.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.18 = N.port-N.18,  dif.port.20 = (N.port - N.20), dif.port.cota=(N.cota-N.port),
              difp.port.18= dif.port.18/N.18, difp.port.20 = dif.port.20/N.20, difp.port.cota = dif.port.cota/N.port)

## Por colegios
usp.mestr %>%
    group_by(colegio) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(m.fev.20, na.rm=TRUE), N.port = sum(m.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.20 = (N.port - N.20), dif.port.18 = N.port-N.18,
              difp.port.18= dif.port.18/N.18, difp.port.20 = dif.port.20/N.20)

usp.dout %>%
    group_by(colegio) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(d.fev.20, na.rm=TRUE), N.port = sum(d.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.20 = (N.port - N.20), dif.port.18 = N.port-N.18,  
              difp.port.18= dif.port.18/N.18, difp.port.20 = dif.port.20/N.20)
## Por areas de avaliacao
usp.mestr %>%
    group_by(NM_AREA_AVALIACAO) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(m.fev.20, na.rm=TRUE), N.port = sum(m.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.20 = (N.port - N.20), dif.port.18 = N.port-N.18,
              difp.port.18= 100*dif.port.18/N.18, difp.port.20 = 100*dif.port.20/N.20) %>%
    arrange(difp.port.18) %>%
    print(n=50)

usp.dout %>%
    group_by(NM_AREA_AVALIACAO) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(d.fev.20, na.rm=TRUE), N.port = sum(d.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.18 = N.port-N.18,  dif.port.20 = (N.port - N.20),
              difp.port.18= 100*dif.port.18/N.18, difp.port.20 = 100*dif.port.20/N.20) %>%
    arrange(difp.port.18) %>%
    print(n=50)

## Por conceito
usp.mestr %>%
    group_by(CD_CONCEITO_CURSO) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(m.fev.20, na.rm=TRUE), N.port = sum(m.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.20 = (N.port - N.20), dif.port.18 = N.port-N.18,
              difp.port.18= 100*dif.port.18/N.18, difp.port.20 = 100*dif.port.20/N.20) %>%
    arrange(difp.port.18)

usp.dout %>%
    group_by(CD_CONCEITO_CURSO) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(d.fev.20, na.rm=TRUE), N.port = sum(d.portaria, na.rm=TRUE),
              dif.20.18 = N.20-N.18, dif.port.18 = N.port-N.18,  dif.port.20 = (N.port - N.20),
              difp.port.18= 100*dif.port.18/N.18, difp.port.20 = 100*dif.port.20/N.20) %>%
    arrange(difp.port.18)

## Bolsas / discentes
usp.mestr %>%
    group_by(NM_AREA_AVALIACAO) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(m.fev.20, na.rm=TRUE), N.port = sum(m.portaria, na.rm=TRUE),
              disc.18 = sum(media.discentes),
              bd.18 = N.18/disc.18, bd.20 = N.20/disc.18, bd.port = N.port/disc.18) %>%
    print(n=50) 

usp.dout %>%
    group_by(NM_AREA_AVALIACAO) %>%
    summarise(N.18 = sum(n.bolsas.18, na.rm=TRUE), N.20 = sum(d.fev.20, na.rm=TRUE), N.port = sum(d.portaria, na.rm=TRUE),
              disc.18 = sum(media.discentes),
              bd.18 = N.18/disc.18, bd.20 = N.20/disc.18, bd.port = N.port/disc.18) %>%
    print(n=50)


p1 <-usp.mestr %>%
    mutate(bd.18 = n.bolsas.18/media.discentes, bd.20 = m.fev.20/media.discentes, bd.port = m.portaria/media.discentes,
           odds.18.por=(bd.port-bd.18)/bd.18,  odds.20.por=(bd.port-bd.20)/bd.20) %>%
    ggplot(aes(bd.20, odds.20.por)) +
    geom_point()

p1 + facet_wrap(~CD_CONCEITO_CURSO, scales="free")
p1 + facet_wrap(~colegio, scales="free")

p2 <-  usp.dout %>%
    mutate(bd.18 = n.bolsas.18/media.discentes, bd.20 = d.fev.20/media.discentes, bd.port = d.portaria/media.discentes,
           odds.18.por=(bd.port-bd.18)/bd.18, odds.20.por=(bd.port-bd.20)/bd.20) %>%
    ggplot(aes(bd.20, odds.20.por)) +
    geom_point()

p2 + facet_wrap(~CD_CONCEITO_CURSO, scales="free")
p2 + facet_wrap(~colegio, scales="free")
