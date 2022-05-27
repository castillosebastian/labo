require("data.table")
library(purrr)
library(dplyr)
library(stringr)

rm( list=ls() )
gc()

set.seed( 10237)

nexperimentos = 100
tiros_iniciales = 48
tiros = 9
total_aciertos = vector()
supera_preseleccion = vector()


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
  
  
  tablero  <- data.table("id" = 1:100, tiros_totales = 0)
  
  tablero$tiros_totales = mapply( ftirar, jugadores, tiros_iniciales)
  cantidad_tiros = vector()
  cantidad_tiros[[1]] = tiros_iniciales * 100
  
  promedio  <- mean(tablero$tiros_totales)
  ids_juegan = tablero$id[tablero$tiros_totales >= promedio]
  tablero = tablero[tablero$tiros_totales >= promedio]
  
  supera_preseleccion[[j]] = 100 %in% ids_juegan
  
  iter = nrow(tablero)
  
  resultados  <- data.table("id" = ids_juegan)
  
  for(i in 2:iter){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros)
    
    tablero$tiros_totales  =  tablero$tiros_totales +  resultados[[i]]
    
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
cat(sum(supera_preseleccion)/nexperimentos)
cat(sum(total_aciertos)/nexperimentos)
cat(tiros_totales)