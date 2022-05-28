require("data.table")
library(purrr)
library(dplyr)
library(stringr)

rm( list=ls() )
gc()

set.seed(10237)
#set.seed(777781)
#set.seed(777787)

nexperimentos = 10000
primer_inter = 10
tiros_iniciales = 2
tiros_finales = 3
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
  tablero  <- data.table("id" = 1:100, tiros_totales = 0)
  cantidad_tiros = vector()
  
  for(i in 2:primer_inter){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros_iniciales)
    
    cantidad_tiros[[i]] = nrow(resultados) * tiros_iniciales
    
    tablero$tiros_totales  =  tablero$tiros_totales +  resultados[[i]]
    
    tablero = tablero[order(tablero$tiros_totales, decreasing = TRUE)]
    
    jugador_descalificado = tablero$id[which.min(tablero$tiros_totales)]
    
    tablero = tablero[ id != jugador_descalificado,]
    
    resultados = resultados[id != jugador_descalificado,]
    
    ids_juegan = tablero$id
  }
  
  
  supera_preseleccion[[j]] = 100 %in% ids_juegan
  iter = nrow(tablero)
  resultados  <- data.table("id" = ids_juegan)
  cantidad_tiros2 = vector()
  
  for(i in 2:iter){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros_finales)
    
    cantidad_tiros2[[i]] = nrow(resultados) * tiros_finales
    
    tablero$tiros_totales  =  tablero$tiros_totales +  resultados[[i]]
    
    tablero = tablero[order(tablero$tiros_totales, decreasing = TRUE)]
    
    jugador_descalificado = tablero$id[which.min(tablero$tiros_totales)]
    
    tablero = tablero[ id != jugador_descalificado,]
    
    resultados = resultados[id != jugador_descalificado,]
    
    ids_juegan = tablero$id
  }
  
  cantidad_tiros3 = c(cantidad_tiros, cantidad_tiros2)
  
  total_aciertos[[j]] = tablero$id == 100
  tiros_totales = sum(cantidad_tiros3, na.rm = T)
  
}
cat(sum(supera_preseleccion)/nexperimentos)
cat(sum(total_aciertos)/nexperimentos)
cat(tiros_totales)