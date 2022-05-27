#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar
#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
library(dplyr)

ftirar  <- function( prob, qty )
{
  #return(  sum( runif(qty, min = 0.5, max = 1) < prob ) )
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<- sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

Estrategia_A  <- function()
{
  
  gimnasio_init()
  
 
  planilla_cazatalentos  <- data.table( "id"= 1:100 )
  
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := tirosr1 ]  #registro en la planilla que tiran 90 tiros
  
  for(i in 1:14){
    
    resultado1  <- gimnasio_tirar( ids_juegan1, tirosr1)
    
    planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
    
    
  }
  
 
  
  planilla_cazatalentos$aciertos_totales = planilla_cazatalentos$aciertos1 +  planilla_cazatalentos$aciertos2
  
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos_totales) ]
  #pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos2) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

tirosr1 = 30
tirosr2 = 200

for( experimento  in  1:10000 ){
  
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy
  
  veredicto  <- Estrategia_A()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
  
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total
tasa_eleccion_correcta



