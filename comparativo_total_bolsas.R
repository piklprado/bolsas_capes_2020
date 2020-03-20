library(dplyr)
library(readxl)
## Leitura dos dados, compilados das bases da CAPES e calculados a partir da portaria
## N de bolsas por programa em 2018 (https://geocapes.capes.gov.br/geocapes/)
## Clique em "Visao analitica", então em "Área de avalicão" e então na tabela na única área de avaliação que aparece que é "INADEFINIDA". Isso vai gerar uma tablea de bolsas por programas, que entendo que é para todos os programas.
## Já fiz isso e importei a tabela para xlsx, que vai em anexo.
bolsas.18 <- read_excel("bolsas2108.xlsx") %>% as.data.frame()
## Muda nomes para facilitar manipulação
names(bolsas.18) <- gsub(" ", ".", names(bolsas.18))
## N de bolsas
n.bolsas.2018 <-
    aggregate(bolsas.18[bolsas.18$Programa.Fomento%in%c("DS", "PROEX", "PROSUC", "PROSUP"), c("MESTRADO", "DOUTORADO.PLENO")],
          by = list(programa=bolsas.18$Programa.Fomento[bolsas.18$Programa.Fomento%in%c("DS", "PROEX", "PROSUC", "PROSUP")]), sum) 
n.bolsas.2018$totais <- apply(n.bolsas.2018[,-1],1, sum)
