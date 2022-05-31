require("data.table")
library(purrr)
library(dplyr)
library(stringr)

rm( list=ls() )
gc()

#set.seed(10237)
#set.seed(777781)
#set.seed(777787)

nexperimentos = 100
primer_inter = 6 # atención las rondas comienzan en 2: o sea que los 100 tiran 1 tiro cada uno 5 veces: 100*1*5

# guarda resultados
total_aciertos = vector()
cantidad_tiros = vector()


for (h in 1:nexperimentos){
  
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
  
  for(i in 2:primer_inter){ # se van 500 tiros
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], 1)
    
  }
  
  cantidad_tiros[[1]] = length(ids_juegan) * 5
  
  ids_resumen = resultados$id
  encestes = resultados %>% select(-id) %>% rowSums()
  resumen = tibble(ids_resumen, encestes)  
  
  peor_score = resumen$encestes[which.min(resumen$encestes)]
  ids_juegan = resumen$ids_resumen[resumen$encestes > peor_score]
  resumen = resumen[resumen$encestes > peor_score,]
  
  if(!(100 %in% resumen$ids_resumen)){a = 'salio 100 en ronda 1'}else{a = 'sigue 100 a ronda 2'}
  
  cat(a, '\n')
  
  iter = nrow(resumen)
  resultados  <- data.table("id" = ids_juegan)  
 
  for(j in 1:10){
    
    result  <- data.table("id" = ids_juegan)
    
    for(i in 2:6){ # cada iteracion son 500
      result[[i]] = mapply( ftirar, jugadores[ids_juegan], 1)
    }
    
    cantidad_tiros[[j+1]] = length(ids_juegan) * 5
    
    ids_resumen = result$id
    encestes = result %>% select(-id) %>% rowSums()
    resumen_temp = tibble(ids_resumen, encestes)  
    
    resumen = resumen %>% left_join(resumen_temp, by = 'ids_resumen')
    
    #cumulative_scores = resumen %>% select(-ids_resumen) %>% rowSums(., na.rm = T)
    # salen_en_ronda2 = resumen_temp %>% filter(encestes > 0) %>% .$ids_resumen
    # ids_juegan =  resumen$ids_resumen[resumen$ids_resumen != resumen_temp$ids_resumen]
    ids_juegan = resumen_temp$ids_resumen[resumen_temp$encestes > 0]
    
    if (length(ids_juegan) == 1) break
    
  }
  
  indice_enceste = resumen %>% 
    tidyr::pivot_longer(!ids_resumen, names_to = "rondas", values_to = "encestes") %>% 
    group_by(ids_resumen) %>% 
    summarise(suma_enceste = sum(encestes, na.rm = T)) 
  
  total_aciertos[[h]] = indice_enceste$ids_resumen[which.max(indice_enceste$suma_enceste)] == 100
  
}

cat(sum(total_aciertos)/nexperimentos)
cat(sum(cantidad_tiros, na.rm = T))