require("data.table")
library(purrr)
library(dplyr)
library(stringr)

aciertos_totales = vector()
nexperimentos = 10000

for (j in 1:nexperimentos){
 
  mejor      <-  0.7
  peloton    <-  ( 501:599 ) / 1000
  jugadores  <-  c( peloton, mejor ) 
  ids_juegan  <- 1:100   #los jugadores que participan en la ronda,
  
  resultados  <- data.table("id" = 1:100)
  cantidad_tiros = vector()
  
  tiros = 120
  
  ftirar  <- function( prob, qty )
  {
    return( sum( runif(qty) < prob ) )
  }
  
  for(i in 2:5){
  
  resultados[[i]] = mapply( ftirar, jugadores[ids_juegan], tiros)
  
  cantidad_tiros[[i]] = nrow(resultados) * tiros
  
  mediana = resultados[[i]] > median(resultados[[i]], na.rm = T) # debería trabajar con la mediana acumulada
  
  ids_juegan = ids_juegan[mediana]
  
  resultados = resultados[mediana, ]
  
  if (nrow(resultados) <= 10) break 
}

total = resultados %>% 
  mutate(id = as.factor(id)) %>% 
  janitor::adorn_totals('col', name = 'total_aciertos')

aciertos_totales[[j]] = total$id[which.max(total$total_aciertos)] == 100

cantidad_tiros = sum(cantidad_tiros, na.rm = T)

}

print(sum(aciertos_totales, na.rm = T)/nexperimentos)
print(cantidad_tiros)


