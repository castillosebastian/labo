require("data.table")
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)

rm( list=ls() )
gc()

ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

mejor      <-  0.7
peor      <- 0.501
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) 
ids_juegan  <- 1:100  
resultados  <- data.table("id" = 1:100)


experimentos = 100

final_results = vector()

for(e in 1:experimentos){
  
  jugadores_score = vector()
  
for(i in 1:100){
  
  resultado_tiro = vector()
  tiro_numero = vector()
  
  for(j in 1:149){
    tiro_numero[[j]] = j
    resultado_tiro[[j]] = mapply(ftirar, jugadores[i], 1)
  }
  
  df = tibble(tiro_numero, resultado_tiro) %>% mutate(cumsum = cumsum(resultado_tiro)) %>% 
    rowwise() %>% mutate(score = cumsum/tiro_numero)
  
  df = df[110:149, ]
  
  jugadores_score[[i]] = df$score[which.max(df$score)]
  
}

resultado <- tibble(jugadores = ids_juegan, score = jugadores_score)

final_results[[e]] = resultado$jugadores[which.max(resultado$score)] == 100

}

sum(final_results)/experimentos
