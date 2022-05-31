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
tiros_finales = 5
iter = 30
# guarda resultados
total_aciertos = vector()
# control interno del algoritmo: veo si jugador.7 supera 1a eliminación (tiros_iniciales * primer_iter)
supera_preseleccion = vector()


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
  tablero  <- data.table("id" = 1:100, tiros_totales = 0)
  cantidad_tiros[[1]] = length(ids_juegan) * 1
  
  for(i in 2:iter){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros_finales)
    
    cantidad_tiros[[i]] = nrow(resultados) * tiros_finales
    
    tablero$tiros_totales  =  tablero$tiros_totales +  resultados[[i]]
    
    tablero = tablero[order(tablero$tiros_totales, decreasing = TRUE)][1:(nrow(tablero)-i),]
    
    ids_juegan = tablero$id
    
    resultados = resultados[ids_juegan,]
  
  }
  
  total_aciertos[[j]] = tablero$id[which.max(tablero$tiros_totales)] == 100
    
  tiros_totales = sum(cantidad_tiros, na.rm = T)
  
}
cat(sum(total_aciertos)/nexperimentos)
cat(tiros_totales)