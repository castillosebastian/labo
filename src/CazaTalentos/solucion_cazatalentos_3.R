require("data.table")
library(dplyr)

# Con pocos tiros dicrimino a mi target de los jugados con bajo score, pero necesito un creciente números de tiros para
# discriminarlo de los que tienen mejor score.

# primer filtro los malos bien malos
# n244: 0.991 => metrica importante: es la cantidad de tiros que necesito para 
#                                    discriminar mi target del mejor jugador del peloton
# n60: 0.992 => metrica importante: es la cantidad de tiros que necesito para 
#                                    discriminar mi target del peor jugador del peloton
# Conclusion: por la vía de solo tiros no voy a poder encontrar mi target por menos de 24 mil tiros.
# n5:  0.9901 => 195
# n10:  0.9938 => 254
# n50: 0.991 =>   371
# n100: 0.991 =>  415
# n100 0.5<p<=1 : 96

ftirar  <- function( prob, qty )
{
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



tiros1 = 14

  set.seed(10123)
  
  #inicializo el juego
  gimnasio_init()
  
  #Esta el la planilla del cazatalentos
  planilla_cazatalentos  <- data.table( "id" = 1:100 )
  #tiran los 100 jugadores es decir 1:100  
  ids_juegan1  <- 1:100   
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros1 ] 
  
  # Mi magia: Rondas
  
  l = list()
  
  for(  i  in 1:10){
    vaciertos  <- mapply( ftirar, ids_juegan1, tiros1 )
    l[[i]] <- vaciertos
  }
  
  df = purrr::map_dfr(l, ~as.data.frame(t(.x)))
  
  df = df %>% as_tibble() %>% 
    pivot_longer(cols = tidyselect::everything(),names_to = "jugadores", values_to = "aciertos") %>% 
    group_by(jugadores) %>% 
    summarise(suma = sum(aciertos)) %>% 
    mutate(id = str_remove_all(jugadores, "V")) %>% 
    mutate(id = as.integer(id)) %>% 
    rename(aciertos = suma) %>% 
    select(id, aciertos)
  
  planilla_cazatalentos = planilla_cazatalentos %>% 
    left_join(df)
  
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  


