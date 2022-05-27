# Con 10 tiros y 6 iteraciones siempre voy a elegir el mejor
# cuando tengo error en la seleccion del mejor necesito 3 rondas de tiro para corregir el error
# en cualquier caso el umbral de decisión de la eleccion podrìa ser <= 0.6 antes de las 30 iteraciones. Superado
# el umbral de las 30 iteracciones eliminar los dos jugadores y seguir con los próximos 2

# Criterios >= 0.6 & 10 iteraciones

# La pista de cuàl es el mejor para definir la batalla està vinculada a la evaluaciòn que estoy haciedo
# si evaluo a > b y es TRUE las probabilidades son 1 o bien vector ascedente de probabilidaes iteracion1 < iteracionN:
# 1 : 0 
# 2 : 0 
# 3 : 0.3333333 
# 4 : 0.5 
# 5 : 0.6 
# 6 : 0.6666667 
# 7 : 0.7142857 
# 8 : 0.75 

# si evaluo a > b y es FALSE las probabilidades son 0 o bien venctor de probabilidades decrecientes.

# el juego de 1 contra 1 procederìa así
# el mejor contra el menos peor se resuelve:
# por obtener probabilidad crecientes probabilidad <= 6

require("data.table")

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

# # Mejor contra menos peor
menos_peor <- 0.599
mejor      <-  0.7
mejor_menos_peor  <-  c( menos_peor, mejor ) #intencionalmente el mejor esta al final

tiros = 20
peor = vector()
mejor = vector()
resultado = vector()
iter = 15
prob = vector()


for( i in 1:iter)  #diez mil experimentos
{
  vaciertos  <- mapply(  ftirar, mejor_menos_peor, iter )
  peor[[i]] = vaciertos[1]
  mejor[[i]] = vaciertos[2]
  resultado[[i]] = sum(mejor) > sum(peor)
  cat(i, ":", sum(resultado)/i, "\n" )

  prob[[i]] = sum(resultado)/i

}

prob[1] <= prob[iter]


