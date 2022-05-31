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
tiros_finales = 1
iter = 400 # 139 tiros por cada jugador

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
  cantidad_tiros = vector()
  
  for(i in 2:iter){
    resultados[[i]] = mapply( ftirar, jugadores, tiros_finales)
  }
  
  resultados$total_acietos = resultados %>% select(-id) %>% rowSums()
  
  total_aciertos[[j]] = resultados$id[which.max(resultados$total_acietos)] == 100
  
  cat( resultados$id[which.max(resultados$total_acietos)], '\n')
 
}
cat(sum(total_aciertos)/nexperimentos)
cat(iter * 100)

