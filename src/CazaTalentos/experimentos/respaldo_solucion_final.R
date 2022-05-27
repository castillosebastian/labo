require("data.table")
library(purrr)
library(dplyr)
library(stringr)

aciertos_totales = vector()
nexperimentos = 100
total_aciertos = vector()

for (j in 1:nexperimentos){
  
  
  ftirar  <- function( prob, qty )
  {
    return( sum( runif(qty) < prob ) )
  }

  mejor      <-  0.7
  peloton    <-  ( 501:599 ) / 1000
  jugadores  <-  c( peloton, mejor ) 
  ids_juegan  <- 1:100   #los jugadores que participan en la ronda,
  
  resultados  <- data.table("id" = 1:100)
  cantidad_tiros = vector()
  
  
  resultados  <- data.table("id" = 1:100)
  tiros = 10 # aumentar
  
  for(i in 2:16){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros)
    
    cantidad_tiros[[i]] = nrow(resultados) * tiros
    
    resultados = resultados[order(resultados[[i]],decreasing=TRUE)][1:nrow(resultados)-1,]
    
    ids_juegan = resultados$id
    
  }
  
  resultados <- resultados %>% 
    mutate(total = select(., contains("V")) %>% rowSums()) 
  
  total_aciertos[[j]] = resultados$id[which.max(resultados$total)] == 100

}

cat(sum(total_aciertos)/nexperimentos)
cat(sum(cantidad_tiros, na.rm = T))







