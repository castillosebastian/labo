tiros = 1:139
options(scipen = 999) # inhabilito notacion científica
jugador_index = 0.3

prob = vector()

for (i in 1:139){
  
  prob[[i]] = jugador_index ^ tiros[i]
  
}
prob
