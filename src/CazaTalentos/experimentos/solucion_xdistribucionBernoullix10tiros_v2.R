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
primer_inter = 11 # atención las rondas comienzan en 2: o sea que los 100 tiran 1 tiro cada uno 5 veces: 100*1*5

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
  tablero  <- data.table("id" = 1:100, tiros_totales = 0)
  cantidad_tiros = vector()
  
  for(i in 2:primer_inter){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], 1)
    
  }
  
  ids_resumen = resultados$id
  encestes = resultados %>% select(-id) %>% rowSums()
  resumen = tibble(ids_resumen, encestes)  
 
  for(i in 12:129){
    
    resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], 1)
    
    resumen[[(i-9)]] = resultados %>% select(((i-9):i)) %>% rowSums() 
   
  }
  
  indice_enceste = resumen %>% 
    tidyr::pivot_longer(!ids_resumen, names_to = "rondas", values_to = "score_en_10") %>% 
    mutate(score_en_10 = as.factor(score_en_10)) %>% 
    group_by(ids_resumen, score_en_10, .drop=FALSE) %>% 
    summarise(cantidad = n()) %>% 
    group_by(ids_resumen) %>% 
    mutate(total_tiros = sum(cantidad)) %>% rowwise() %>% 
    mutate(prob = round(cantidad/total_tiros, digits = 5))
  
  indice_enceste_wort_scores = indice_enceste %>% 
    filter(score_en_10 %in% c(0,1,2)) %>%
    group_by(ids_resumen) %>% 
    summarise(prob_worst = sum(prob, na.rm = T), 
              cantidad = sum(cantidad, na.rm = T))
  
  
  total_aciertos[[j]] = indice_enceste_wort_scores$ids_resumen[which.min(indice_enceste_wort_scores$prob_worst)] == 100
  
}

cat(sum(total_aciertos)/nexperimentos)
cat((length(resultados)-1)*100)