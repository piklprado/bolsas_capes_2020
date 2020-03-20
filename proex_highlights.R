source("estimativas_cotas_proex.R")

## Minhas conclusoes até agora sobre a variacao no n de bolsas de programas PROEX

## 1. O total de bolsas neste grupo teve um discreto aumento
## Total de bolsas antes e depois: aumento
## Mestrado
apply(proex.m2[,c("bolsas.em.2018", "est.bolsas.2020")], 2, sum)
## Dourtorado
apply(proex.d2[,c("bolsas.em.2018", "est.bolsas.2020")], 2, sum)

## No entanto, 15 das 49 áreas de avaliação tiveram perdas de bolsas de doutorado, variando de
proex.m2 %>%
    group_by(NM_AREA_AVALIACAO ) %>%
    summarise(N.18 = sum(bolsas.em.2018), N.20 = sum(est.bolsas.2020),
              diferenca = N.20-N.18, dif.p = diferenca/N.18) %>%
    arrange(dif.p) %>%
    print(n=50)
## 
