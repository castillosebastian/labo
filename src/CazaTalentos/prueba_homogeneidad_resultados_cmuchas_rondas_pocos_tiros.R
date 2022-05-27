require("data.table")
gimnasio_init()


ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )
#tiran los 100 jugadores es decir 1:100  
ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

#planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros1 ]  #registro en la planilla que tiran 90 tiros

#tiros1 = 10
# tiros2 = 40
# tiros3 = 50
# 
# #Eliminatoria
# planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros1 ]  #registro en la planilla que tiran 90 tiros
# resultado1  <- gimnasio_tirar( ids_juegan1, tiros1)
# planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
# 
# planilla_cazatalentos[ ids_juegan1,  tiros2 :=tiros2 ]  #registro en la planilla que tiran 90 tiros
# resultado2  <- gimnasio_tirar( ids_juegan1, tiros2)
# planilla_cazatalentos[ ids_juegan1,  aciertos2 := resultado2 ]  #registro en la planilla
# 
# planilla_cazatalentos[ ids_juegan1,  tiros3 := tiros3 ]  #registro en la planilla que tiran 90 tiros
# resultado3  <- gimnasio_tirar( ids_juegan1, tiros3)
# planilla_cazatalentos[ ids_juegan1,  aciertos3 := resultado3 ]  #registro en la planilla

#View(arrange(planilla_cazatalentos, desc(aciertos1)))


library(dplyr)
planilla_cazatalentos = as_tibble(planilla_cazatalentos)

planilla_cazatalentos = planilla_cazatalentos %>%  
  rowwise() %>% 
  mutate(strdv = round(sd(c(aciertos1, aciertos2, aciertos3)), digits = 1)) %>% View()
  


  


 