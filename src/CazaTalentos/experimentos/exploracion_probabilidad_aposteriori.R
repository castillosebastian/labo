require("data.table")
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)

rm( list=ls() )
gc()

#set.seed( 1023768)

nexperimentos = 10
total_aciertos = vector()
iteraciones = 500

jugador_id = 1

# 
# for (j in 1:nexperimentos){


ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

mejor      <-  0.7
peor      <- 0.501
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) 
ids_juegan  <- 1:100  
resultados  <- data.table("id" = 1:100)
resultado_tiro = vector()
tiro_numero = vector()

for(i in 1:iteraciones){
  tiro_numero[[i]] = i
  resultado_tiro[[i]] = mapply(ftirar, jugadores[jugador_id], 1)
}

df = tibble(tiro_numero, resultado_tiro) %>% 
 mutate(cumsum = cumsum(resultado_tiro)) %>% 
  rowwise() %>% mutate(probabilidad_enceste = cumsum/tiro_numero)


df %>% filter(tiro_numero > 10) %>% 
  ggplot(aes(x=tiro_numero, y=probabilidad_enceste)) +
  geom_line() +
  labs(title = str_c('Evolución del indice de enceste a medida que el jugador', jugador_id, ' tira'))

# df$probabilidad_enceste[iteraciones]
# 
# cat(df$probabilidad_enceste[iteraciones], jugadores[jugador_id], '\n')
# 
# }



