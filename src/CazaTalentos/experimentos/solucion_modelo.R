require("data.table")
library(purrr)
library(dplyr)
library(stringr)

rm( list=ls() )
gc()

#set.seed(10237)
#set.seed(777781)
#set.seed(777787)

nexperimentos = 10
tiros_finales = 10
iter = 139
# guarda resultados
total_aciertos = vector()


  
  ftirar  <- function( prob, qty )
  {
    return( sum( runif(qty) < prob ) )
  }

  mejor      <-  0.7
  peloton    <-  ( 501:599 ) / 1000
  jugadores  <-  c( peloton, mejor ) 
  ids_juegan  <- 1:100   
  
  resultados  <- data.table("id" = ids_juegan)
  cantidad_tiros = vector()
  
  df =  tibble(id=0,V2=0,V3=0,V4=0,V5=0,V6=0,V7=0,V8=0,V9=0,V10=0,V11=0,
               V12=0,V13=0,promedio_aciertos=0,mediana_aciertos=0,min=0,max=0,
               sd=0,var=0) 
  
  
for( i in 1:10000){
  
  resultados  <- data.table("id" = ids_juegan)
  
  for(i in 2:13){
    resultados[[i]] = mapply( ftirar, jugadores, tiros_finales)
  }
  
  resultados$promedio_aciertos = rowMeans(resultados)
  resultados$mediana_aciertos = robustbase::rowMedians(as.matrix(resultados %>% select(-id)))
  resultados = resultados %>% rowwise() %>% mutate(min = min(V2 ,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13))
  resultados = resultados %>% rowwise() %>% mutate(max = max(V2 ,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13))
  resultados = resultados %>% rowwise() %>% mutate(sd = sd(c(V2 ,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)))
  resultados = resultados %>% rowwise() %>% mutate(var = var(c(V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)))
  
  df = df %>% bind_rows(resultados)
  
}
