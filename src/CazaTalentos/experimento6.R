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
#set.seed( 102191 )  

resultados = vector()

tirosr1 = 415

iter_experimento = 10000


for(experimento in 1:iter_experimento){
  
  planilla_cazatalentos  <- data.table(id = 1:100 )
  
  planilla_cazatalentos$aciertos_totales <- mapply( ftirar, jugadores, tirosr1)
  
  mejor <- planilla_cazatalentos$id[which.max(planilla_cazatalentos$aciertos_totales)]
    
  cat(experimento, ":", mejor,  "\n" )
    
  resultados[experimento] <- mejor == 100
  
}

sum(resultados)/iter_experimento


 