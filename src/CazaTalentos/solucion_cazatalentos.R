require("data.table")
library(dplyr)

# primer filtro los malos bien malos

# n5:  0.9901 => 195
# n10:  0.9938 => 254
# n50: 0.991 =>   371
# n100: 0.991 =>  415

# n100 0.5<p<=1 : 96



ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty, min = 0.5, max = 1) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
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
  return( list("tiros_total"=  sum(planilla_cazatalentos$tiros1) + sum(planilla_cazatalentos$tiros2, na.rm = T),
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}

# resultado <- tibble::tribble(
#   ~"inter", ~"tiros_toal", ~"acierto",
#   "0", 0, 0
# )

aciertos <- vector()

semilla <- sample(123:10123, 10000)

tiros1 = 70
tiros2 = 200

for (j in 1:10000){
  
  set.seed(semilla[j])
  
  #inicializo el juego
  gimnasio_init()
  
  #Esta el la planilla del cazatalentos
  planilla_cazatalentos  <- data.table( "id" = 1:100 )
  
  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100  n tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  grupos <- rep(1:10, 10)
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros1 ]  #registro en la planilla que tiran 90 tiros
  
  # Hago que tiren
  for(i in 1:10){
    
    resultado <- gimnasio_tirar( ids_juegan1[grupos==i], tiros1)
    planilla_cazatalentos[ids_juegan1[grupos==i][which.max(resultado)],  
                          aciertos1 := resultado[which.max(resultado)]]  #registro en la planilla
    
  }
  
  #Ronda 2 -------------------------------------------------------
  #ganadores ronda1
  ids_juegan2  <- !is.na(planilla_cazatalentos$aciertos1)
  planilla_cazatalentos[ ids_juegan2,  tiros2 := tiros2 ]  #registro en la planilla que tiran 400 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, tiros2)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos2) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  tiros <<- veredicto$tiros_total[1]
  aciertos[j] <- veredicto$acierto[1]
}

tiros
sum(aciertos)/10000

