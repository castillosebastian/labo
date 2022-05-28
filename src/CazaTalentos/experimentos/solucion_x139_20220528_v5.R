require("data.table")
library(purrr)
library(dplyr)
library(stringr)

rm( list=ls() )
gc()

#set.seed(10237)
#set.seed(777781)
#set.seed(777787)

nexperimentos = 10
tiros_finales = 1
iter = 139 # 139 tiros por cada jugador
iter_sample = 100# realizo 100 muestras de 90 tiros de cada jugador para obtener un promedio por muestra

# guarda resultados
total_aciertos = vector()

for (j in 1:nexperimentos){
  
  ftirar  <- function( prob, qty )
  {
    return( sum( runif(qty) < prob ) )
  }

  mejor      <-  0.7
  peloton    <-  ( 501:599 ) / 1000
  jugadores  <-  c( peloton, mejor ) 
  ids_juegan  <- 1:100   
  
  resultados  <- data.table("id" = ids_juegan)
  cantidad_tiros = vector()
  
  for(i in 2:iter){
    resultados[[i]] = mapply( ftirar, jugadores, tiros_finales)
  }
  
  #obtengo muchas muestras de estos resultados y proceso promedio de enceste
  resultadost = as_tibble(t(resultados))
  
  resultadost = janitor::row_to_names(resultadost, 1) 
  
  tablero = tibble(id = 0, indice_enceste = 0)
  
  for(i in 1:iter_sample){
    
    resultadost_sampled = resultadost %>% 
      bind_rows(sample_n(resultadost, 120, replace = F)) # %>% mutate_all(funs(. / 30)) 
    
    indice_enceste = colSums(as.matrix(resultadost_sampled))
    
    resultado_final = tibble( id = ids_juegan, indice_enceste = indice_enceste) %>% 
      arrange(desc(indice_enceste)) %>% 
      slice(1:3)
    
    tablero = bind_rows(tablero, resultado_final)
    
  }
  
  tablero = tablero %>% group_by(id) %>% summarise(indice_enceste = sum(indice_enceste))
  
  total_aciertos[[j]] = tablero$id[which.max(tablero$indice_enceste)] == 100
  
  cat( tablero$id[which.max(tablero$indice_enceste)], '\n')
 
}
cat(sum(total_aciertos)/nexperimentos)


