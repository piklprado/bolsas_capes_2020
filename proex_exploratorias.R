################################################################################
## Exploratorias proex
################################################################################
## Verificando a Eco USP, que conheço
filter(proex.m2, CD_PROGRAMA_IES=="33002010116P8") ## Consta uma a mais que tínhamos em fev/2020
filter(proex.d2, CD_PROGRAMA_IES=="33002010116P8") ## Confere
## IFT UNESP
filter(proex.m2, CD_PROGRAMA_IES=="33015015001P7") 
filter(proex.d2, CD_PROGRAMA_IES=="33015015001P7")
## Biodiversidade
filter(proex.d2,  NM_AREA_AVALIACAO=="BIODIVERSIDADE") %>%
    mutate(diferenca = est.bolsas.2020 - bolsas.em.2018) %>%
    select(NM_PROGRAMA_IES, SG_ENTIDADE_ENSINO, cota.portaria:diferenca) %>%
    write.csv2(file="temp.csv")
filter(proex.m2,  NM_AREA_AVALIACAO=="BIODIVERSIDADE") %>%
    select(NM_PROGRAMA_IES, SG_ENTIDADE_ENSINO, cota.portaria:est.bolsas.2020)
## Total de bolsas antes e depois: aumento
apply(proex.m2[,c("bolsas.em.2018", "est.bolsas.2020")], 2, sum)
apply(proex.d2[,c("bolsas.em.2018", "est.bolsas.2020")], 2, sum)

## Variacao em n de bolsas
## Por colegio e conceito
proex.d2 %>%
    group_by(CD_CONCEITO_CURSO, colegio) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18)
## Por area de avaliacao
proex.d2 %>%
    group_by(NM_AREA_AVALIACAO ) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18) %>%
    arrange(dif.p) %>%
## Por area conceito e status juridico
proex.d2 %>%
    group_by(CD_CONCEITO_CURSO, CS_STATUS_JURIDICO, ) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18)
## status juridico
proex.d2 %>%
    group_by(CS_STATUS_JURIDICO, ) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18)
proex.m2 %>%
    group_by(CS_STATUS_JURIDICO, ) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18)

## Por faixa de IDH e de n medio de titulados
proex.d2 %>%
    group_by(idhm.faixa, tmc.faixa ) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18) %>% print(n=24)
## Regiao
proex.d2 %>%
    group_by(NM_REGIAO) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18)
proex.d2 %>%
    group_by(CD_CONCEITO_CURSO, NM_REGIAO) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18)
proex.d2 %>%
    group_by(NM_REGIAO,colegio) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18)
## Razao media bolsas/discentes em 2018: melhorou um pouco
proex.d2 %>%
    mutate(bolsa.disc.18 = bolsas.em.2018/media.discentes, bolsa.disc.20 = est.bolsas.2020/media.discentes) %>%
    group_by(colegio) %>%
    summarise(bd.18 = mean(bolsa.disc.18), bd.20 = mean(bolsa.disc.20))

## Graficos
## Bolsas antes e depois: n absolutos e percentual de variacao
proex.d2 %>%
    ggplot(aes(bolsas.em.2018, est.bolsas.2020)) +
    geom_point() +
    geom_abline() +
    facet_grid(colegio~CD_CONCEITO_CURSO)
## EM relacao ao n de bolsas em 2018
proex.d2 %>%
    mutate(diferenca = est.bolsas.2020 - bolsas.em.2018, dif.p = diferenca/bolsas.em.2018) %>%
    ggplot(aes(bolsas.em.2018, dif.p)) +
    geom_point() +
    facet_grid(~colegio)
## EM classes
proex.d2 %>%
    mutate(diferenca = est.bolsas.2020 - bolsas.em.2018, dif.p = diferenca/bolsas.em.2018,
           bolsas.2018 = cut(bolsas.em.2018, c(0,21,42,max(bolsas.em.2018)+1))) %>%
    ggplot(aes(bolsas.2018, dif.p)) +
    geom_boxplot() +
    facet_grid(~colegio)
## relacao com n de discentes
proex.d2 %>%
    mutate(diferenca = est.bolsas.2020 - bolsas.em.2018, dif.p = diferenca/bolsas.em.2018) %>%
    ggplot(aes(media.discentes, dif.p)) +
    geom_point() +
    facet_grid(~colegio) + scale_x_log10()

## relacao entre titulados e docentes
proex.d2 %>%
    ggplot(aes(media.docentes, media.titulados)) +
    geom_point() +
    facet_grid(colegio~CD_CONCEITO_CURSO) +
    scale_x_log10() +
    scale_y_log10()
## Por area conhecimento
proex.d2 %>%
    ggplot(aes(media.docentes, media.titulados)) +
    geom_point() +
    facet_wrap(~NM_GRANDE_AREA_CONHECIMENTO.x)


## Sem relacao com IDHM
proex.d2 %>%
    mutate(dif.p =(est.bolsas.2020 - bolsas.em.2018)/bolsas.em.2018) %>% 
    ggplot(aes(idhm, dif.p)) +
    geom_point() +
    facet_grid(colegio~CD_CONCEITO_CURSO)
## relacao com media de titulados
proex.d2 %>%
    mutate(dif.p =(est.bolsas.2020 - bolsas.em.2018)/bolsas.em.2018) %>% 
    ggplot(aes( media.titulados, dif.p)) +
    geom_point() +
    facet_grid(colegio~CD_CONCEITO_CURSO)
## Media discentes
proex.d2 %>%
    mutate(dif.p =(est.bolsas.2020 - bolsas.em.2018)/bolsas.em.2018) %>% 
    ggplot(aes( media.discentes, dif.p)) +
    geom_point() +
    facet_grid(colegio~CD_CONCEITO_CURSO) + scale_x_log10()

## Relacao entre n de bolsas em 2018 e media de titulados
proex.d2 %>%
    ggplot(aes(bolsas.em.2018,media.titulados)) +
    geom_point() +
    facet_grid(colegio~CD_CONCEITO_CURSO)
