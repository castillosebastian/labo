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
# n25: 0.99 = 410     * Ojo: estos 26 jugadores eran los más próximos a 0.7
#                             si son los 25 peores la comparación necesita menos tiros.
# n50: 0.991 =>   371
# n100: 0.991 =>  415
# n100 0.5<p<=1 : 96

# reutilizar los tiros realizados en rondas anteriores: MUY IMPORTANTE

#comparar uno contra uno hasta tener certeza del 0.99 de tener al major, #seleccion mediante sampleo
#mientras no tener 0.99 de certeza seguir probando: tirar y comparar resultados


