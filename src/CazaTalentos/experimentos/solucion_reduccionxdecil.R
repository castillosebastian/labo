require("data.table")
library(purrr)
library(dplyr)
library(stringr)

rm( list=ls() )
gc()


nexperimentos = 100
total_aciertos = vector()
tiros_preseleccion = 500
tiros_eliminatorias = 200


for (j in 1:nexperimentos){
 
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
  # mediana  <- resultados[ ids_juegan, median(V0) ]
  # ids_juegan = resultados$id[resultados$V0 >= mediana]
  # resultados  <- resultados[ V0 >= mediana,  ]
  # cuartil3  <- quantile(resultados$V0)[[4]]
  # ids_juegan = resultados$id[resultados$V0 >= cuartil3]
  # resultados  <- resultados[ V0 >= cuartil3,  ]
  resultados <- resultados %>% arrange(desc(V0)) %>% slice(1:10)
  ids_juegan = resultados$id
  
  # Rondas Eliminatorias----------------------------------------------
  for(i in 3:11){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros_eliminatorias)
    
    cantidad_tiros[[i]] = nrow(resultados) * tiros_eliminatorias
    
    resultados = resultados[order(resultados[[i]],decreasing=TRUE)][1:(nrow(resultados)-1),]
    
    ids_juegan = resultados$id
    
  }
  
  resultados <- resultados %>% mutate(total = select(., contains("V")) %>% rowSums()) 
  
  total_aciertos[[j]] = resultados$id[which.max(resultados$total)] == 100

}

cat(sum(total_aciertos)/nexperimentos)
cat(sum(cantidad_tiros, na.rm = T))
#View(resultados %>% arrange(desc(total)))