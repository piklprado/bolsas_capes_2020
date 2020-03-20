## Simulacoes de aplicacao das portarias 18, 20 e 21 da CAPES à situação em 2018
## funcoes
source("funcoes.R")
## Leitura dos dados: dados dos mestradoe  doutorados, produzidos pelo script "calculos_portaria.R")
mestrados <- read.csv2("mestrados_em_2018_com_media_titulados_idh_cotas_portarias_capes.csv", as.is=TRUE)
doutorados <- read.csv2("doutorados_em_2018_com_media_titulados_idh_cotas_portarias_capes.csv", as.is=TRUE)

## Matriz de transicao em n de programas. De conceitos 3 a 7 (colunas) para conceitos 1 a 7 (linhas)
## de http://avaliacaoquadrienal.capes.gov.br/resultado-da-avaliacao-quadrienal-2017-2
 cm1 <- matrix(
    c(1,1,0,0,0,0,0, ## transicao de conceito 2 para  outros nao informada, coloquei meio a meio
      8,83,1212,472,5,0,0,
      0,7,118,915,316,3,0,
      0,0,3,108,388,121,1,
      0,0,0,2,49,155,64,
      0,0,0,0,7,19,119),
    ncol=6)
## Converte a matriz acima em probabilidades, dividindo cada coluna pelo seu total
## nao é estritamente necessário pq a simulacao faz isto internamente, mas fica mais claro o que a simulacao faz
cm1.p <- sweep(cm1, MARGIN = 2, STATS = apply(cm1, 2, sum), FUN = "/")

## Valores ANEXO III
anexoIII <- expand.grid(colegio = c("Vida", "Humanidades", "Exatas"), curso = c("D","M"))
anexoIII$mean.tmc <- c(11.001, 12.918, 11.118, 13.755, 17.451, 15.402)
anexoIII$sd.tmc <-   c(5.710,  10.352,  8.000,  6.210, 11.763,  9.420)


## Simulacao com n de bolsas afetando n medio de titulacoes
## Esta numa funcao para facilitar, mas só funsciona para os objetos específicos,
## nao tem pretensão de ser uam funcao genérica para qq tabela de dados dos programas,
## apenas para  a que fizemos com os dados de 2018, que é o ponto de partida da simulação
## Argumentos
## colegio: colegio do programa.
## curso: "M"estrado ou "D"doutorado
## matriz.trans = matriz de transicao entre conceitos na avaliacao, no formato do objeto cm1.p acima
## simula.avaliacao: simula a ocorrência de uma avaliação em 2021, de acordo com a matriz de transição do argumento acima?
## atualiza.criterios: atualiza os criterios de n médio de titulacao (anexos III das portarias),
## recalculando a média e desvio-padrão do número de titulados nos anos seguintes. Se FALSE usa os critérios do argumento "criterios" em todas as iterações
## se.fit: a simulacao projeta a media de titulados nos ultimos 4 anos em cada iteração por uma regressão linear do
## n de titulados ~ n medio de bolsas nos 4 anos anteriores, em escala log. Se 'se.fit=TRUE' estas projecoes usam os erros padrão dos estimados pela regressão. Se "se.fit=FALSE" as projeções são apenas os estimados médios pela regressão, sem adicionar erros.
f.sim1 <- function(colegio=c("Vida","Humanidades","Exatas"),
                   curso=c("M","D"),
                   matriz.trans = cm1.p,
                   criterios = anexoIII,
                   simula.avaliacao = TRUE,
                   atualiza.criterios = FALSE,
                   se.fit = TRUE){
    colegio <- match.arg(colegio)
    curso <- match.arg(curso)
    ## Media e sd do n medio de titulados (Anexos III)
    mean.tmc  <- criterios$mean.tmc[anexoIII$colegio==colegio&anexoIII$curso==curso]
    sd.tmc <- criterios$sd.tmc[anexoIII$colegio==colegio&anexoIII$curso==curso]
    ## 1 . Extrai os dados iniciais: situacao em 2018
    if(curso =="D"){
        simulacao <- doutorados[doutorados$colegio==colegio&
                                         !is.na(doutorados$n.bolsas.18)&
                                         doutorados$AN_INICIO_PREVISTO<2016,
                                         c("CD_PROGRAMA_IES","NM_AREA_AVALIACAO","CD_CONCEITO_CURSO", "idhm", "media.titulados",
                                           "n.bolsas.18", "m.bolsas.dout")]
        names(simulacao) <- c("Código.Programa","area.avaliacao","conceito.18", "idh", "tmc.15.18", "n.bolsas.18", "m.bolsas.15.18")
        simulacao$conceito.18 <- as.integer(simulacao$conceito.18)
        ## Junta n de bolsas 15 a 17
        ## AINDA POR VERIFICAR: Em alguns anos alguns programas ficam com NA em bolsas.
        ## Programas que estavam com zero bolsas este ano? Seriam programas cujas bolsas estavam todas nas Pro-Reitorias?
        simulacao <- merge(simulacao, bolsas.dm.17[, c("Código.Programa","n.bolsas.d")], all.x=TRUE)
        names(simulacao)[names(simulacao)=="n.bolsas.d"] <- "n.bolsas.17"
        simulacao <- merge(simulacao, bolsas.dm.16[, c("Código.Programa","n.bolsas.d")], all.x=TRUE)
        names(simulacao)[names(simulacao)=="n.bolsas.d"] <- "n.bolsas.16"
        simulacao <- merge(simulacao, bolsas.dm.15[, c("Código.Programa","n.bolsas.d")], all.x=TRUE)
        names(simulacao)[names(simulacao)=="n.bolsas.d"] <- "n.bolsas.15"
    }
    if(curso=="M"){
        simulacao <- mestrados[mestrados$colegio==colegio&
                                        !is.na(mestrados$n.bolsas.18)&
                                        mestrados$AN_INICIO_PREVISTO<2018&
                                        mestrados$CD_CONCEITO_CURSO%in%c("2","3","4","5","6","7"),
                                        c("CD_PROGRAMA_IES","NM_AREA_AVALIACAO", "CD_CONCEITO_CURSO", "idhm", "media.titulados",
                                          "n.bolsas.18", "m.bolsas.mestr")]
        names(simulacao) <- c("Código.Programa","area.avaliacao", "conceito.18", "idh", "tmc.15.18", "n.bolsas.18", "m.bolsas.15.18")
        simulacao$conceito.18 <- as.integer(simulacao$conceito.18)
        ## Junta n de bolsas 15 a 17
        ## AINDA POR VERIFICAR: Em alguns anos alguns programas ficam com NA em bolsas.
        ## Programas que estavam com zero bolsas este ano? Seriam programas cujas bolsas estavam todas nas Pro-Reitorias?
        simulacao <- merge(simulacao, bolsas.dm.17[, c("Código.Programa","n.bolsas.m")], all.x=TRUE)
        names(simulacao)[names(simulacao)=="n.bolsas.m"] <- "n.bolsas.17"
        simulacao <- merge(simulacao, bolsas.dm.16[, c("Código.Programa","n.bolsas.m")], all.x=TRUE)
        names(simulacao)[names(simulacao)=="n.bolsas.m"] <- "n.bolsas.16"
        simulacao <- merge(simulacao, bolsas.dm.15[, c("Código.Programa","n.bolsas.m")], all.x=TRUE)
        names(simulacao)[names(simulacao)=="n.bolsas.m"] <- "n.bolsas.15"
    }

    ## 2. Regressao entre tempo medio de titulacao e n de bolsas
    simulacao.lm <- lm(log(tmc.15.18 + 0.1) ~ log(m.bolsas.15.18 + 0.1), data = simulacao)

    ## 3. Projecoes ##
    ## Primeiro ano (2020)
    ## Numero de bolsas projetado para 2020, a partir dos dados de 2018
    simulacao$n.bolsas.20 <- with(simulacao,
                                  NF.bolsas.v(conceito = conceito.18, idh = idh, tmc = tmc.15.18, bolsas.antes = n.bolsas.18,
                                              colegio=colegio,
                                              mean.tmc = mean.tmc,
                                              sd.tmc = sd.tmc,
                                              curso =curso))
    ## Calcula o n medio de bolsas de cada curso dos ultimos 4 anos
    simulacao$m.bolsas.16.20 <- apply(simulacao[,c("n.bolsas.16", "n.bolsas.17", "n.bolsas.18", "n.bolsas.20")], 1,
                                      mean, na.rm=TRUE)
    ## N medio de titulados com previstos pela regressao
    simulacao$tmc.16.20 <- exp(sim.tc(simulacao.lm,
                                      newdata = data.frame(m.bolsas.15.18=simulacao$m.bolsas.16.20),
                                      se.fit=se.fit)) +0.1
    ## Segundo ano (2021) ##
    ## Atualiza os criterios de tmc do ANEXO III com as novas medias de n de titulados dos ultimos 4 anos
    if(atualiza.criterios){
        mean.tmc <-  mean(simulacao$tmc.16.20)
        sd.tmc <- sd(simulacao$tmc.16.20)
    }
    simulacao$n.bolsas.21 <- with(simulacao,
                                  NF.bolsas.v(conceito = conceito.18, idh = idh, tmc = tmc.16.20, bolsas.antes = n.bolsas.20,
                                              colegio=colegio,
                                              mean.tmc = mean.tmc,
                                              sd.tmc = sd.tmc,
                                              curso =curso))
    ## N medio de bolsas dos ultimos 4 anos
    simulacao$m.bolsas.17.21 <- apply(simulacao[,c("n.bolsas.17", "n.bolsas.18", "n.bolsas.20", "n.bolsas.21")],
                                      1, mean, na.rm=TRUE)
    ## N medio de titulados pela regressao
    simulacao$tmc.17.21 <- exp( sim.tc(simulacao.lm,
                                       newdata = data.frame(m.bolsas.15.18=simulacao$m.bolsas.17.21),
                                       se.fit = se.fit) )+0.1
    ## Simula as mudancas de conceitos em 2022 com a avaliacao e matris de transicao entre conceitos do quadrienio passado
    ## Note que o conceito nao depende de nenhum indicador, acontece ao acaso de acordo com a matriz de transicao
    if(simula.avaliacao)
        simulacao$conceito.22 <- sapply(simulacao$conceito.18, sim.avaliacao, m.transicao = cm1.p)
    else
        simulacao$conceito.22 <- simulacao$conceito.18 
    ## Terceiro ano (2022)
    if(atualiza.criterios){
        mean.tmc  <-  mean(simulacao$tmc.17.21)
        sd.tmc  <-  sd(simulacao$tmc.17.21)
    }
    simulacao$n.bolsas.22 <- with(simulacao,
                                  NF.bolsas.v(conceito = conceito.22, idh = idh, tmc = tmc.17.21, bolsas.antes = n.bolsas.21,
                                              colegio=colegio,
                                              mean.tmc = mean.tmc,
                                              sd.tmc = sd.tmc,
                                              curso =curso))
    ## N medio de bolsas dos ultimos 4 anos
    simulacao$m.bolsas.18.22 <- apply(simulacao[,c("n.bolsas.18", "n.bolsas.20", "n.bolsas.21", "n.bolsas.22")],
                                      1, mean, na.rm=TRUE)
    ## N medio de titulados previstos pela regressao
    simulacao$tmc.18.22 <- exp(sim.tc(simulacao.lm,
                                      newdata = data.frame(m.bolsas.15.18=simulacao$m.bolsas.18.22), se.fit=se.fit)) +0.1
    ## Retorna resultados
    return(simulacao)
}

## Resultados ##
## Perdas por areas de avaliacao, Ciencias da Vida
## Doutorado
nrep <- 19
tmp1 <- f.sim1(colegio = "Vida", curso = "D", se.fit=TRUE)%>%
    group_by(area.avaliacao) %>%
    summarise(Nbolsas.18 = sum(n.bolsas.18), Nbolsas.22=sum(n.bolsas.22),
              dif.p = 100*(sum(n.bolsas.22) - sum(n.bolsas.18))/sum(n.bolsas.18)) %>%
        as.data.frame()
for(i in 1:nrep){
    tmp1 <- rbind(tmp1,
                  f.sim1(colegio = "Vida", curso = "D", se.fit=TRUE)%>%
    group_by(area.avaliacao) %>%
    summarise(Nbolsas.18 = sum(n.bolsas.18), Nbolsas.22=sum(n.bolsas.22),
              dif.p = 100*(sum(n.bolsas.22) - sum(n.bolsas.18))/sum(n.bolsas.18)) %>%
        as.data.frame())
}
sim.areas.v.dout  <- aggregate(tmp1[,-1], by=list(area.avaliacao=tmp1$area.avaliacao), mean)
## Mestrado
nrep <- 19
tmp1 <- f.sim1(colegio = "Vida", curso = "M", se.fit=TRUE)%>%
    group_by(area.avaliacao) %>%
    summarise(Nbolsas.18 = sum(n.bolsas.18), Nbolsas.22=sum(n.bolsas.22),
              dif.p = 100*(sum(n.bolsas.22) - sum(n.bolsas.18))/sum(n.bolsas.18)) %>%
        as.data.frame()
for(i in 1:nrep){
    tmp1 <- rbind(tmp1,
                  f.sim1(colegio = "Vida", curso = "M", se.fit=TRUE)%>%
    group_by(area.avaliacao) %>%
    summarise(Nbolsas.18 = sum(n.bolsas.18), Nbolsas.22=sum(n.bolsas.22),
              dif.p = 100*(sum(n.bolsas.22) - sum(n.bolsas.18))/sum(n.bolsas.18)) %>%
        as.data.frame())
}
sim.areas.v.mestr  <- aggregate(tmp1[,-1], by=list(area.avaliacao=tmp1$area.avaliacao), mean)


## Perdas de bolsas e n de titulados por colegio e curso
## Tabela para guardar resultados
sim.result.bolsas <- expand.grid(colegio = c("Vida", "Humanidades", "Exatas"), curso = c("D","M"), stringsAsFactors=FALSE)
sim.result.bolsas$dif.total <- NA
sim.result.bolsas$dif.2 <- NA
sim.result.bolsas$dif.3 <- NA
sim.result.bolsas$dif.4 <- NA
sim.result.bolsas$dif.5 <- NA
sim.result.bolsas$dif.6 <- NA
sim.result.bolsas$dif.7 <- NA
sim.result.tmc <- sim.result.bolsas
## Repete a simulacao 20 vezes e registra diferenca percentual de bolsas e de media de titulados, total e por conceito
nrep <- 20
tmp1  <- tmp2 <- matrix(nrow= nrep, ncol=ncol(sim.result.bolsas)-2)
for(j in 1:nrow(sim.result.bolsas)){
    for(i in 1:nrep){
        tmp <-  f.sim1(colegio = sim.result.bolsas$colegio[j], curso = sim.result.bolsas$curso[j], se.fit=TRUE)
        tmp1[i,1] <- with(tmp, 100*(sum(n.bolsas.22) - sum(n.bolsas.18))/sum(n.bolsas.18))
        tmp2[i,1] <- with(tmp, 100*(sum(tmc.18.22) - sum(tmc.15.18))/sum(tmc.15.18))
        tmp1[i,2:ncol(tmp1)] <-
            tmp %>%
            group_by(conceito.18) %>%
            summarise(perdas = 100*(sum(n.bolsas.22) - sum(n.bolsas.18))/sum(n.bolsas.18)) %>%
            as.data.frame() %>% select(perdas) %>% unlist()
        tmp2[i,2:ncol(tmp1)] <-
            tmp %>%
            group_by(conceito.18) %>%
            summarise(perdas = 100*(sum(tmc.18.22) - sum(tmc.15.18))/sum(tmc.15.18)) %>%
            as.data.frame() %>% select(perdas) %>% unlist()
    }
    sim.result.bolsas[j,3:ncol(sim.result.bolsas)] <- apply(tmp1,2,mean)
    sim.result.tmc[j,3:ncol(sim.result.tmc)] <- apply(tmp2,2,mean)
}

## Diferenca percentual de bolsas em relacao a 2018
sim.result.bolsas
## Diferencas  percentuais n medio titulados em relacao a 2018
sim.result.tmc

write.csv2(sim.result.bolsas, file="perdas_bolsas_simulacao_18-22.csv")
write.csv2(sim.result.tmc, file="perdas_titulados_simulacao_18-22.csv")
write.csv2(sim.areas.v.mestr, file="perdas_bolsas_por_area_vida_mestr_simulacao_18.22.csv")
write.csv2(sim.areas.v.dout, file="perdas_bolsas_por_area_vida_dout_simulacao_18.22.csv")
