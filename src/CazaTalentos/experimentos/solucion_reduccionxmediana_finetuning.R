require("data.table")
library(purrr)
library(dplyr)
library(stringr)

nexperimentos = 1000
tiros_preseleccion = 78
tiros_eliminatorias = 49
supera_preseleccion = vector()
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
  cantidad_tiros = vector()
  
  # Preseleccion------------------------------------------------------
  
  cantidad_tiros[[1]] <- tiros_preseleccion * length(ids_juegan)
  aciertos = mapply( ftirar, jugadores[ids_juegan], tiros_preseleccion)
  resultados$V0 = aciertos  
  mediana  <- resultados[ ids_juegan, median(V0) ]
  ids_juegan = resultados$id[resultados$V0 >= mediana]
  resultados  <- resultados[ V0 >= mediana,  ]
  supera_preseleccion[[j]] = 100 %in% ids_juegan
  
  # Rondas Eliminatorias----------------------------------------------
  
  for(i in 3:8){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros_eliminatorias)
    cantidad_tiros[[i]] = nrow(resultados) * tiros_eliminatorias
    resultados = resultados[order(resultados[[i]],decreasing=TRUE)][1:(nrow(resultados)-5),]
    ids_juegan = resultados$id
    
  }
  
  resultados <- resultados %>% mutate(total = select(., contains("V")) %>% rowSums()) 
  total_aciertos[[j]] = resultados$id[which.max(resultados$total)] == 100

}

cat(sum(supera_preseleccion)/nexperimentos)
cat(sum(total_aciertos)/nexperimentos)
cat(sum(cantidad_tiros, na.rm = T))
