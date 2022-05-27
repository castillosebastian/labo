require("data.table")
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)

rm( list=ls() )
gc()


  
  ftirar  <- function( prob, qty )
  {
    return( sum( runif(qty) < prob ) )
  }
  
  mejor      <-  0.7
  peloton    <-  ( 501:599 ) / 1000
  jugadores  <-  c( peloton, mejor ) 
  ids_juegan  <- 1:100   
  
  resultados  <- data.table("id" = 1:100)
  tablero  <- data.table("id" = 1:100, tiros_totales = 0)
  
  cantidad_tiros = vector()
  
  for(i in 2:139){
    resultados[[i]] = mapply( ftirar, jugadores, 1)
  }
  
  resultadost = as_tibble(t(resultados))
  
  resultadost = janitor::row_to_names(resultadost, 1) 
  
  resultadost_sampled = resultadost %>% 
    bind_rows(sample_n(resultadost, 1000000, replace = TRUE)) # %>% mutate_all(funs(. / 30)) uso cuando prueba 30 tiros
  
  rondas = rep(1:23, 43500)
  rondas = rondas[1:nrow(resultadost_sampled)]
  
  resultadost_sampled = resultadost_sampled %>% 
    mutate(rondas = rondas)
  
  df = as_tibble(t(resultadost_sampled))
  
  
  nrow(resultadost_sampled)/42000
  
  
  indice_enceste = colMeans(as.matrix(resultadost_sampled))
  
  resultado_final = tibble( id = ids_juegan, indice_enceste = indice_enceste)
  
  total_aciertos[[j]] = resultado_final$id[which.max(resultado_final$indice_enceste)] == 100
  


cat(sum(total_aciertos)/nexperimentos)



