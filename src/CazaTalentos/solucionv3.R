require("data.table")
library(purrr)
library(dplyr)


ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
  #return( sum( runif(qty) < prob ) )
}


mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) 


#experimiento
set.seed( 102191 )  

ronda1_tiros_xjugador = 40 # jugadores 100
rondas1 = 2 # 100 * 40 * 2

ronda2_tiros_xjugador = 50 # jugadores 50
rondas2 = 2 # 50 * 50 * 2

ronda3_tiros_xjugador = 195 # jugadores 5
rondas3 = 6 # 195  * 5 * 6


resultados = vector()

for( experimento  in  1:1000 ){
  
  planilla_cazatalentos  <- data.table(1:100 )
  
  # Ronda1_____________________________________
  for(i in 1:rondas1){
    planilla_cazatalentos[[i]] <- mapply( ftirar, jugadores, ronda1_tiros_xjugador)
  }
  
  planilla_cazatalentos <- planilla_cazatalentos %>% 
    janitor::adorn_totals('col', name = 'aciertos_totales') %>% 
    mutate(id = 1:100) 
  
  mediana <- median(planilla_cazatalentos$aciertos_totales)
  
  planilla_cazatalentos <- planilla_cazatalentos %>% filter(aciertos_totales > mediana)
  
  id_ronda = planilla_cazatalentos$id
  
  # Ronda2_____________________________________
  
  planilla_cazatalentos  <- data.table(1:nrow(planilla_cazatalentos))
  
  for(i in 1:rondas2){
    planilla_cazatalentos[[i]] <- mapply( ftirar, id_ronda, ronda2_tiros_xjugador)
  }
  
  planilla_cazatalentos <- planilla_cazatalentos %>% 
    janitor::adorn_totals('col', name = 'aciertos_totales') %>% 
    mutate(id = id_ronda) 
  
  planilla_cazatalentos <- planilla_cazatalentos %>% 
    arrange(desc(aciertos_totales)) %>% 
    slice(1:5)
  
  id_ronda = planilla_cazatalentos$id
  
  # Ronda3__________________________________________
  planilla_cazatalentos  <- data.table(1:nrow(planilla_cazatalentos))
  
  for(i in 1:rondas3){
    planilla_cazatalentos[[i]] <- mapply( ftirar, id_ronda, ronda3_tiros_xjugador)
  }
  
  planilla_cazatalentos <- planilla_cazatalentos %>% 
    janitor::adorn_totals('col', name = 'aciertos_totales') %>% 
    mutate(id = id_ronda) 
  
  mejor <- planilla_cazatalentos$id[which.max(planilla_cazatalentos$aciertos_totales)]
  
  cat(experimento, ":", mejor, "\n" )
  
  resultados[experimento] <- mejor == 100
  
}

sum(resultados)/10000


 