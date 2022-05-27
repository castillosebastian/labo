
require("data.table")

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
  #return( sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) #intencionalmente el mejor esta al final


planilla_cazatalentos  <- data.table( "id" = 1:100 )


l = list()

for(  i  in 1:10){
    vaciertos  <- mapply( ftirar, jugadores, 14 )
    l[[i]] <- vaciertos
}

library(purrr)
df= map_dfr(l, ~as.data.frame(t(.x)))

df = df %>% as_tibble() %>% 
  pivot_longer(cols = tidyselect::everything(),names_to = "jugadores", values_to = "aciertos")
  
df = df %>% group_by(jugadores) %>% summarise(
  suma = sum(aciertos)) 

df = df %>% 
  mutate(id = str_remove_all(jugadores, "V")) %>% 
  mutate(id = as.integer(id)) %>% 
  rename(aciertos = suma) %>% 
  select(id, aciertos)

planilla_cazatalentos = planilla_cazatalentos %>% 
  left_join(df)

pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos) ]
id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
id_mejor
 