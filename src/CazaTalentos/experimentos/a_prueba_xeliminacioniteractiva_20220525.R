require("data.table")
library(purrr)
library(dplyr)
library(stringr)


rm( list=ls() )
gc()

nexperimentos = 5

gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}

for (j in 1:nexperimentos){
  
  ftirar  <- function( prob, qty )
  {
    return( sum( runif(qty) < prob ) )
  }
  
  tiros = 40
  mejor      <-  0.7
  peloton    <-  ( 501:599 ) / 1000
  jugadores  <-  c( peloton, mejor ) 
  ids_juegan  <- 1:100   
  resultados  <- data.table("id" = 1:100)
  cantidad_tiros = vector()
  total_aciertos = vector()
  
  for(i in 2:101){
  
  resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros)
  
  cantidad_tiros[[i]] = nrow(resultados) * tiros * (i-1)
  
  resultados = resultados[order(resultados[[i]],decreasing=TRUE)][1:(nrow(resultados)-1),]
  
  ids_juegan = resultados$id
  }
  
  total_aciertos[[j]] = resultados$id == 100
  tiros_totales = sum(cantidad_tiros, na.rm = T)
}

cat(sum(total_aciertos)/nexperimentos)
cat(tiros_totales)


