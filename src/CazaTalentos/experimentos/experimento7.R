require("data.table")
library(purrr)
library(dplyr)



#limpio la memoria
rm( list=ls() )
gc()


ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
  #return( sum( runif(qty) < prob ) )
}

mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) 


#experimiento
#set.seed( 102191 )  

resultados = vector()

tiros_totales = vector()


tirosr1 = 140
tirosr2 = 100
tirosr3 = 80

ronda1_iter =2# no afecta sumar más rondas
ronda2_iter =2
ronda3_iter =2

iter_experimento = 100

for(experimento in 1:iter_experimento){
  
  tiros_totales = vector()
  
  exluido_Jordan_enRonda  = vector()
  
  # Ronda1_____________________________________
  planilla_cazatalentos  <- data.table(1:100 )
  for(i in 1:ronda1_iter){
      planilla_cazatalentos[[i]] <- mapply( ftirar, jugadores, tirosr1)
  }
  
  planilla_cazatalentos <- planilla_cazatalentos %>% 
    janitor::adorn_totals('col', name = 'aciertos_totales1') %>% 
    mutate(id = 1:100) 
    
  tiros_totales[1] = tirosr1 * nrow(planilla_cazatalentos) *ronda1_iter
  
  mediana <- median(planilla_cazatalentos$aciertos_totales)
  planilla_cazatalentos <- planilla_cazatalentos %>% filter(aciertos_totales1 > mediana)
  
  id_ronda = planilla_cazatalentos$id
  preseleccionados = jugadores[id_ronda]
  
  exluido_Jordan_enRonda[1] = ifelse(100 %in% id_ronda, 1, 0)
    
  # Ronda2_____________________________________
  temp  <- data.table(1:nrow(planilla_cazatalentos))
  for(i in 1:ronda2_iter){
    temp[[i]] <- mapply(ftirar, preseleccionados, tirosr2)
  }
    
  temp <- temp %>% 
    janitor::adorn_totals('col', name = 'aciertos_totales2') %>% 
    mutate(id = id_ronda) 
  
  planilla_cazatalentos <- planilla_cazatalentos %>% 
    left_join(temp %>% select(id, aciertos_totales2), by = "id")
  
  tiros_totales[2] = tirosr2 * nrow(planilla_cazatalentos)  *ronda1_iter
    
  temp <- temp %>% 
    arrange(desc(aciertos_totales2)) %>% 
    slice(1:5)
  
  id_ronda = temp$id
  preseleccionados = jugadores[id_ronda]
  
  exluido_Jordan_enRonda[2] = ifelse(100 %in% id_ronda, 1, 0)
  
  # Ronda3__________________________________________
  temp  <- data.table(1:nrow(temp))
    
  for(i in 1:ronda3_iter){
    temp[[i]] <- mapply( ftirar, preseleccionados, tirosr3)
  }
    
  temp <- temp %>% 
    janitor::adorn_totals('col', name = 'aciertos_totales3') %>% 
    mutate(id = id_ronda) 
  
  planilla_cazatalentos <- planilla_cazatalentos %>% 
    left_join(temp %>% select(id, aciertos_totales3), by = "id")
  
  tiros_totales[3] = tirosr3 * nrow(planilla_cazatalentos)  *ronda1_iter
  
  tiros_totales = sum(tiros_totales)
  
  planilla_cazatalentos <- planilla_cazatalentos %>%
    janitor::adorn_totals('col', name = 'aciertos_final')
  
  # planilla_cazatalentos <- planilla_cazatalentos %>% rowwise() %>% 
  #   mutate(aciertos_final = sd(c(aciertos_totales1, aciertos_totales2, aciertos_totales3), na.rm = T)) 
  
  mejor <- planilla_cazatalentos$id[which.max(planilla_cazatalentos$aciertos_final)]
    
  cat(experimento, ":", mejor,tiros_totales, "exluido_ronda:", exluido_Jordan_enRonda, "\n" )
    
  resultados[experimento] <- mejor == 100
  
}

sum(resultados)/iter_experimento


 