
library(matrixStats)

stdv = rowSds(as.matrix(resultados %>% select(-id)))

suma_aciertos= rowSums(resultados %>% select(-id))

tablero = tibble(ids_juegan, suma_aciertos, stdv)