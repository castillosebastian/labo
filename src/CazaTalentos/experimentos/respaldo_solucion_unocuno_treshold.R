require("data.table")
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)


rm( list=ls() )
gc()


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

iteraciones = 800

for(i in 1:iteraciones){
  tiro_numero[[i]] = i
  resultado_tiro[[i]] = mapply(ftirar, jugadores[100], 1)
}

df = tibble(tiro_numero, resultado_tiro) %>% 
 mutate(cumsum = cumsum(resultado_tiro)) %>% 
  rowwise() %>% mutate(crecimiento = cumsum/tiro_numero)


df %>% filter(tiro_numero > 1) %>% 
  ggplot(aes(x=tiro_numero, y=crecimiento)) +
  geom_line() +
  geom_hline(yintercept=0.7, color = "red", linetype="dashed") +
  labs(title = "Reconociendo probabilidad teórica")
  

df$crecimiento[iteraciones]





