require("data.table")
library(purrr)
library(dplyr)
library(stringr)

rm( list=ls() )
gc()

nexperimentos = 100
tiros = 4
total_aciertos = vector()

for (j in 1:nexperimentos){
  
  gimnasio_tirar  <- function(  pids,  pcantidad )
  {
    GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
    res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
    
    return( res )
  }
  
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
  
  for(i in 2:100){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros)
    
    tablero$tiros_totales  =  tablero$tiros_totales +  resultados[[i]]
    
    #resultados = resultados[order(resultados[[i]],decreasing=TRUE)][1:(nrow(resultados)-5),]
    
    # Tengo que eliminar más jugadores por ronda
    
    tablero = tablero[order(tablero$tiros_totales, decreasing = TRUE)]
    
    jugador_descalificado = tablero$id[which.min(tablero$tiros_totales)]
    
    tablero = tablero[ id != jugador_descalificado,]
    
    resultados = resultados[id != jugador_descalificado,]
    
    cantidad_tiros[[i]] = nrow(resultados) * tiros
    
    ids_juegan = tablero$id
  }
  
  
  total_aciertos[[j]] = tablero$id == 100
  tiros_totales = sum(cantidad_tiros, na.rm = T)
  
}
cat(sum(total_aciertos)/nexperimentos)
cat(tiros_totales)