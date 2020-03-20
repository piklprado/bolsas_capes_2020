source("funcoes.R")
library(logNormReg)

## Relação en N de bolsas e N de titulados por colegio
## Escala original
doutorados.15.18.id %>%
    filter(AN_INICIO_PREVISTO < 2015) %>%
    ggplot(aes(m.bolsas.dout, media.titulados)) +
    geom_point() +
    facet_wrap( ~ colegio)
## Escala log + 0.1
doutorados.15.18.id %>%
    filter(AN_INICIO_PREVISTO < 2015) %>%
    mutate(m.bolsas.dout = m.bolsas.dout + 0.1, media.titulados = media.titulados+0.1) %>% 
    ggplot(aes(m.bolsas.dout, media.titulados)) +
    geom_point() +
    facet_wrap( ~ colegio) +
    scale_y_log10() +
    scale_x_log10()
## Regressão sem transformacao log com erros normais

## Mestrados
mestrado.modelo.tit.bolsas <- lm(media.titulados ~ m.bolsas.mestr*colegio, data = mestrados.15.18.id,
                              subset = AN_INICIO_PREVISTO < 2015)
summary(mestrado.modelo.tit.bolsas)
## Previstos pelo modelo para 1 a 120 bolsas
new.data <- expand.grid(colegio = unique(mestrados.15.18.id$colegio), m.bolsas.mestr = 0:120)
mestrado.modelo.tit.bolsas.pred <- predict(mestrado.modelo.tit.bolsas, newdata = new.data, interval = "prediction") %>%
    cbind(new.data, .)

## Doutorados
doutorado.modelo.tit.bolsas <- lm(media.titulados ~ m.bolsas.dout*colegio, data = doutorados.15.18.id,
                              subset = AN_INICIO_PREVISTO < 2015)
summary(doutorado.modelo.tit.bolsas)
## Previstos pelo modelo para 1 a 120 bolsas
new.data <- expand.grid(colegio = unique(doutorados.15.18.id$colegio), m.bolsas.dout = 0:120)
doutorado.modelo.tit.bolsas.pred <- predict(doutorado.modelo.tit.bolsas, newdata = new.data, interval = "prediction") %>%
    cbind(new.data, .)


## Regressão com log de x e y + 0.1, com erros normais
## Transforma as variaveis
mestrados.15.18.id  <- 
    mutate(mestrados.15.18.id, l.media.titulados = log(media.titulados + 0.1), l.m.bolsas = log(m.bolsas.mestr + 0.1))
doutorados.15.18.id  <- 
    mutate(doutorados.15.18.id, l.media.titulados = log(media.titulados + 0.1), l.m.bolsas = log(m.bolsas.dout+ 0.1))

## Mestrados
mestrado.modelo.log.tit.bolsas <- lm(l.media.titulados ~ l.m.bolsas*colegio, data = mestrados.15.18.id,
                              subset = AN_INICIO_PREVISTO < 2015)
summary(mestrado.modelo.log.tit.bolsas)

## Previstos pelo modelo para 1 a 120 bolsas
new.data <- expand.grid(colegio = unique(mestrados.15.18.id$colegio), l.m.bolsas = log((0:120)+0.1))
mestrado.modelo.log.tit.bolsas.pred <- predict(mestrado.modelo.log.tit.bolsas, newdata = new.data, interval = "prediction") %>%
    cbind(new.data, .) %>%
    mutate(m.bolsas = exp(l.m.bolsas) - 0.1, fit.m.tit = exp(fit) - 0.1, lwr.m.tit = exp(lwr) - 0.1, upr.m.tit = exp(upr) - 0.1)

## Doutorados
doutorado.modelo.log.tit.bolsas <- lm(l.media.titulados ~ l.m.bolsas*colegio, data = doutorados.15.18.id,
                              subset = AN_INICIO_PREVISTO < 2015)
summary(doutorado.modelo.log.tit.bolsas)

## Previstos pelo modelo para 1 a 120 bolsas
new.data <- expand.grid(colegio = unique(doutorados.15.18.id$colegio), l.m.bolsas = log((0:120)+0.1))
doutorado.modelo.log.tit.bolsas.pred <- predict(doutorado.modelo.log.tit.bolsas, newdata = new.data, interval = "prediction") %>%
    cbind(new.data, .) %>%
    mutate(m.bolsas = exp(l.m.bolsas) - 0.1, fit.m.tit = exp(fit) - 0.1, lwr.m.tit = exp(lwr) - 0.1, upr.m.tit = exp(upr) - 0.1)



## salva csvs
write.csv2(mestrado.modelo.tit.bolsas.pred, file = "mestrados_previstos_tituladosXbolsas.csv")
write.csv2(doutorado.modelo.tit.bolsas.pred, file = "doutorados_previstos_tituladosXbolsas.csv")
write.csv2(mestrado.modelo.log.tit.bolsas.pred, file = "mestrados_previstos_tituladosXbolsas_log.csv")
write.csv2(doutorado.modelo.log.tit.bolsas.pred, file = "doutorados_previstos_tituladosXbolsas_log.csv")
