library(dplyr)
library(tidyr)
library(readxl)
## Funcao que soma os titulados, apenas dos programas que têm o respectivo titulo e estao na lista de 2018
f1 <- function(x, titulo = "MESTRADO"){
    x %>%
        filter(CD_PROGRAMA_IES%in%progr.18$CD_PROGRAMA_IES &
               (NM_GRAU_PROGRAMA==titulo|NM_GRAU_PROGRAMA=="MESTRADO/DOUTORADO")) %>%
        group_by(CD_PROGRAMA_IES, NM_GRANDE_AREA_CONHECIMENTO) %>%
        summarise(n.disc = sum(DS_GRAU_ACADEMICO_DISCENTE==titulo),
            n.tit = sum(DS_GRAU_ACADEMICO_DISCENTE==titulo & NM_SITUACAO_DISCENTE == "TITULADO")) %>%
        as.data.frame()
}

## Funcao que soma n de docentes e de docentes permamentes
f2 <- function(x){
    x %>% filter(CD_PROGRAMA_IES%in%progr.18$CD_PROGRAMA_IES) %>%
      group_by(CD_PROGRAMA_IES) %>%
        summarise(n.doc = n(), n.perm = sum(DS_CATEGORIA_DOCENTE=="PERMANENTE")) %>%
        as.data.frame()        
}

### Calculos de n bolsas pelas portarias ###
## Funcoes para os calculos das cotas para ser aplicada as planilhas de dados reais
portaria.mestr <- function(colegio, conceito, idh, tmc){
    fIDH <- approxfun( x = c(0.5, 0.6, 0.7,0.8),y = c(2,1.5,1.25,1),
                      method = "constant", yleft = 2, yright = 1)
    if(colegio=="Vida"){
        fC <- approxfun(x=1:7, y = c(2,2,4,8,11,13,14), method="constant")
        fTM <- approxfun( x = c(0, 7.5449, 19.9649, 32.3849),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
    else if(colegio=="Exatas"){
        fC <- approxfun(x=1:7, y = c(2,2,4,9,11,14,15), method="constant")
        fTM <- approxfun( x = c(0, 5.9819, 24.8229, 43.6639),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
    else if(colegio=="Humanidades"){
        fC <- approxfun(x=1:7, y = c(2,2,4,8,11,13,14), method="constant")
        fTM <- approxfun( x = c(0, 5.7779, 29.1229, 52.4989),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
    inicial  <-  fC(conceito)
    final <- inicial*fIDH(idh)*fTM(tmc)
    if(conceito<3)
        final <- 2
    return(round(final))
}

## Doutorado
portaria.dout <- function(colegio, conceito, idh, tmc){
    fIDH <- approxfun( x = c(0.5, 0.6, 0.7,0.8),y = c(2,1.5,1.25,1),
                      method = "constant", yleft = 2, yright = 1)
    if(colegio=="Vida"){
        fC <- approxfun(x=1:7, y = c(0,0,0,12,16,19,21), method="constant")
        fTM <- approxfun( x = c(0, 5.2989, 16.7019, 28.1039),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
    else if(colegio=="Exatas"){
        fC <- approxfun(x=1:7, y = c(0,0,0,12,16,19,21), method="constant")
        fTM <- approxfun( x = c(0, 3.1179, 19.1189, 35.1199),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
    else if(colegio=="Humanidades"){
        fC <- approxfun(x=1:7, y = c(0,0,0,10,15,18,20), method="constant")
        fTM <- approxfun( x = c(0, 2.5659, 23.2699, 43.9739),y = c(0.5, 1, 1.5, 3),
                          method = "constant", yleft = 0.5, yright = 3)
    }
    inicial  <-  fC(conceito)
    final <- inicial*fIDH(idh)*fTM(tmc)
    return(round(final))
}


## Calculo do n de bolsas a partir do vigente, alocando-se os limite maximos de ganhos e perdas
## (Para PROEX: perda máxima de 10%, ganho máximo de 30%, para os demais perda e ganhos máximos de 10%)
portaria.limites <- function(cota, bolsas.antes, conceito, curso=c("M","D")){
    diferenca <- (cota - bolsas.antes)/bolsas.antes
    if(bolsas.antes<1)
        y <- 0
    else {
        if(diferenca < - 0.1)
            y <- bolsas.antes*0.9
        else if(diferenca > 0.3 & as.integer(conceito)>5 & ((curso=="M"&cota>=6)|cota>=8))
            y <- bolsas.antes*1.3
        else if(diferenca > 0.1 & as.integer(conceito)<=5)
            y <- bolsas.antes*1.1
        else
            y <- cota
        if(as.integer(conceito)<=5 & y < 5)
            y <- ceiling(y)
        else
            y <- round(y)
    }
    return(y)
}

## Vetoriza as funcoes acima, para fazer os calculos por linha do dataframe (tecnicalidade)
portaria.mestr.v <- Vectorize(portaria.mestr)
portaria.dout.v <- Vectorize(portaria.dout)
portaria.limites.v <- Vectorize(portaria.limites)


## Funcao que retorna os estratos definidos pelos anexos II e III
portaria.faixas <- function(curso=c("M","D"),colegio, idh, tmc){
    fIDH <- approxfun( x = c(0.5, 0.6, 0.7,0.8),y = c("Baixo", "Medio", "Alto", "Muito Alto"),
                      method = "constant", yleft = 2, yright = 1)
    if(colegio=="Vida"){
        fC <- approxfun(x=1:7, y = c(2,2,4,8,11,13,14), method="constant")
        fTM <- approxfun( x = c(0, 7.5449, 19.9649, 32.3849),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
    else if(colegio=="Exatas"){
        fC <- approxfun(x=1:7, y = c(2,2,4,9,11,14,15), method="constant")
        fTM <- approxfun( x = c(0, 5.9819, 24.8229, 43.6639),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
    else if(colegio=="Humanidades"){
        fC <- approxfun(x=1:7, y = c(2,2,4,8,11,13,14), method="constant")
        fTM <- approxfun( x = c(0, 5.7779, 29.1229, 52.4989),y = c(0.5, 1, 1.5, 3),
                         method = "constant", yleft = 0.5, yright = 3)
    }
}



## Junta planilhas de bolsas por area em uma unica e filtra apenas bolsas mestrado e doutorado
## dos programas DS, PROSUC, PROSSUP e PROEX
junta.bolsas <- function(diretorio, padrao){
    nomes <- dir(diretorio, pattern=padrao)
    y <- as.data.frame(read_excel(paste(diretorio, nomes[1], sep="")))
    if(length(nomes)>1)
        for(i in 2:length(nomes))
            y <- rbind(y,  as.data.frame(read_excel(paste(diretorio, nomes[i], sep=""))))
    names(y) <- gsub(" ", ".", names(y))
    y %<>%
        filter(Programa.Fomento %in% c("DS", "PROEX", "PROSUC", "PROSUP")& !is.na(Código.Programa)) %>%
        select(Ano, Código.Programa, MESTRADO, DOUTORADO.PLENO) %>%
        group_by(Código.Programa, Ano) %>%
        summarise(n.bolsas.m = sum(MESTRADO), n.bolsas.d = sum(DOUTORADO.PLENO)) %>%
        as.data.frame()
    return(y)
}

################################################################################
## Funcoes para simulacao
################################################################################
## Calcula cotas de bolsas antes da aplicacao dos limites de ganho
## ou perda (calculados pela funcao anterior)
NE.bolsas <- function(colegio, conceito, idh, tmc, mean.tmc, sd.tmc, curso = c("M", "D")){
    fIDH <- approxfun( x = c(0.5, 0.6, 0.7,0.8),y = c(2,1.5,1.25,1),
                      method = "constant", yleft = 2, yright = 1)
    fTM <- approxfun( x = c(0, mean.tmc-sd.tmc, mean.tmc+sd.tmc, mean.tmc+3*sd.tmc),y = c(0.5, 1, 1.5, 3),
                     method = "constant", yleft = 0.5, yright = 3)
    if(curso == "M"){
        if(colegio=="Vida")
            fC <- approxfun(x=1:7, y = c(2,2,4,8,11,13,14), method="constant")
        else if(colegio=="Exatas")
            fC <- approxfun(x=1:7, y = c(2,2,4,9,11,14,15), method="constant")
        else if(colegio=="Humanidades")
            fC <- approxfun(x=1:7, y = c(2,2,4,8,11,13,14), method="constant")
    }
    if(curso == "D"){
        if(colegio=="Vida")
            fC <- approxfun(x=1:7, y = c(0,0,0,12,16,19,21), method="constant")
        else if(colegio=="Exatas")
            fC <- approxfun(x=1:7, y = c(0,0,0,12,16,19,21), method="constant")
        else if(colegio=="Humanidades")
            fC <- approxfun(x=1:7, y = c(0,0,0,10,15,18,20), method="constant")
    }    
    inicial  <-  fC(conceito)
    final <- inicial*fIDH(idh)*fTM(tmc)
    return(round(final))
}

NE.bolsas.v <- Vectorize(NE.bolsas, c("colegio", "conceito", "idh", "tmc"))

## Simula a mudança de conceito no ano de avaliação, usando uma matriz de transicao de conceitos
## matriz de transicao deve ser 7 x 6 com as probabilidades de transicao de (colunas) para (linhas)
## cada conceito CAPES, ordenados de 1 a 7
sim.avaliacao <- function(conceito, m.transicao){
    index <- as.integer(conceito)-1
    sample(1:7, size =1, prob = m.transicao[,index])
    }

## Numero final de bolsas: calcula cotas e entao aplica os limites
NF.bolsas  <-  function(conceito, idh, tmc, bolsas.antes, colegio, mean.tmc, sd.tmc, curso = c("M", "D")){
    cotas <- NE.bolsas(colegio, conceito, idh, tmc, mean.tmc, sd.tmc, curso)
    portaria.limites(cotas, bolsas.antes, conceito, curso)
}
NF.bolsas.v <- Vectorize(NF.bolsas, c("conceito", "idh", "tmc", "bolsas.antes"))

## Sorteia valores a partir dos ajustes de uma regressao linear gaussiana
sim.tc <- function(lm.obj, newdata, se.fit = TRUE){
        previstos <- predict(lm.obj, newdata, se.fit = se.fit)
    if(se.fit)
        y <- with(previstos, rnorm(length(fit), mean = fit, sd = se.fit))
    else
        y <- previstos
    return(y)
    }
