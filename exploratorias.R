## Depende do codigo calculo_portaria.R
################################################################################
## Analises preliminares e graficos
################################################################################
## Conferência dos valores nos Anexos III das portaria
## (medias e sds dos numeros totais e médias anuais de titulos)
## Mestrado: bem proximo dos valores do anexo III ##
mestrados.15.18 %>%
        group_by(colegio) %>%
        summarise(mean.m = mean(m.mest), sd.m = sd(m.mest),
                  mean.N = mean(n.mest), sd.N = sd(n.mest))
## Doutorado: ainda com medias mais baixa e desvios mais altos que nos anexos.
## Possivelmente há ainda zeros que não foram considerados pela CAPES
doutorados.15.18 %>%
    group_by(colegio) %>%
    summarise(mean.m = mean(m.dout), sd.m = sd(m.dout),
              mean.N = mean(n.dout), sd.N = sd(n.dout)) 

## Histogramas medias e totais de titulados com distribuição muito assimetrica
hist.all.m <- mestrados.15.18 %>%
    ggplot(aes(m.mest))+
    geom_histogram() +
    facet_grid(~colegio)+
    ggtitle("Mestrados")
hist.all.d <- doutorados.15.18 %>%
    ggplot(aes(m.dout))+
    geom_histogram() +
    facet_grid(~colegio)+
    ggtitle("Doutorados")

pdf(file = "Medias_das_medias_de_titulados%01d.pdf", onefile =FALSE)
print(hist.all.m)
print(hist.all.d)
dev.off()

## Calculos das portarias
## Verificando a Eco USP, que conheço
filter(mestrados.15.18.id, CD_PROGRAMA_IES=="33002010116P8") ## Confere
filter(doutorados.15.18.id, CD_PROGRAMA_IES=="33002010116P8") ## Confere

## Histogramas dos calculos da portaria
## Cotas x conceito e N medio de titulados
cotasXconceito.m <- mestrados.15.18.id %>%
    ggplot(aes(CD_CONCEITO_CURSO, N.portaria))+
    geom_boxplot() +
    facet_grid(~colegio) +
    ggtitle("Mestrados")
cotasXconceito.d <- doutorados.15.18.id %>%
    ggplot(aes(CD_CONCEITO_CURSO, N.portaria))+
    geom_boxplot() +
    facet_grid(~colegio) +
    ggtitle("Doutorados")
cotasXtmc.m <- mestrados.15.18.id %>%
    ggplot(aes(m.mest,N.portaria))+
    geom_point() +
    facet_grid(colegio~CD_CONCEITO_CURSO) +
    ggtitle("Mestrados")
cotasXtmc.d <- doutorados.15.18.id %>%
    filter(as.integer(CD_CONCEITO_CURSO)>3)%>%
    ggplot(aes(m.dout,N.portaria))+
    geom_point() +
    facet_grid(colegio~CD_CONCEITO_CURSO) +
    ggtitle("Doutorados")

## Diferencas entre cotas e bolsas em 2018
doutorados.15.18.id %>%
    group_by(colegio) %>%
    summarise(N.bolsas.18 = sum(n.bolsas.18, na.rm=TRUE), N.bolsas.cota= sum(cota.portaria, na.rm=TRUE),
              difp.bolsas = (N.bolsas.cota-N.bolsas.18)/N.bolsas.18)

doutorados.15.18.id %>%
    group_by(CD_CONCEITO_CURSO) %>%
    summarise(N.bolsas.18 = sum(n.bolsas.18, na.rm=TRUE), N.bolsas.cota= sum(cota.portaria, na.rm=TRUE),
              difp.bolsas = (N.bolsas.cota-N.bolsas.18)/N.bolsas.18)

pdf(file = "Cotas_portaria%01d.pdf", onefile =FALSE)
print(cotasXconceito.m)
print(cotasXconceito.d)
print(cotasXtmc.m)
print(cotasXtmc.d)
dev.off()


## Relacoes entre n de titulos, docentes, discentes, bolsas

doutorados.15.18.id %>%
     ggplot(aes(media.docentes, media.titulados)) +
     geom_point() +
    facet_wrap(~NM_AREA_AVALIACAO)

## Razao titulados/docentes por conceito
doutorados.15.18.id %>%
    mutate(tit.por.doc=media.titulados/media.docentes) %>%
    ggplot(aes(CD_CONCEITO_CURSO, tit.por.doc)) +
    geom_boxplot() +
    facet_wrap(~colegio)

doutorados.15.18.id %>%
    mutate(tit.por.doc=media.titulados/media.docentes) %>%
    ggplot(aes(CD_CONCEITO_CURSO, tit.por.doc)) +
    geom_boxplot() +
    facet_wrap(~NM_GRANDE_AREA_CONHECIMENTO.x)

## Distribuicoes
## IDH
doutorados.15.18.id %>%
    ggplot(aes(idhm)) +
    geom_histogram(aes(y=..density..)) +
    facet_wrap(~ CD_CONCEITO_CURSO)

doutorados.15.18.id %>%
    ggplot(aes(CD_CONCEITO_CURSO, idhm)) +
    geom_boxplot() +
    facet_wrap(~ colegio)

## N medio de titulados
p1 <- doutorados.15.18.id %>%
    ggplot(aes(media.titulados)) +
    geom_histogram(aes(y=..density..))
p1+ scale_x_log10() +
    facet_wrap(~ CD_CONCEITO_CURSO) 
p1+ scale_x_log10() + facet_wrap(~ colegio) 

doutorados.15.18.id %>%
    ggplot(aes(CD_CONCEITO_CURSO, media.titulados)) +
    geom_boxplot() +
    facet_wrap(~ colegio)
