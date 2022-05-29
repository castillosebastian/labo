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
iteraciones = 139

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

jugador_id = 100

for (j in 1:99) {
  
  resultados  <- data.table("id" = j)
  
  for(i in 1:139){
    resultado_tiro[[i]] = mapply(ftirar, jugadores[jugador_id], 1)
    
  }
  
}







df = tibble(tiro_numero, resultado_tiro) %>% 
  mutate(cumsum = cumsum(resultado_tiro)) %>% 
  rowwise() %>% mutate(probabilidad_enceste = cumsum/tiro_numero)







rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


ksemilla  <- 102191  #poner aqui la PRIMERA de sus cinco semillas

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )


#genero el modelo con los parametros por default
#estos hiperparametros  salieron de una Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   max_bin=              31,
                                   learning_rate=         0.067,
                                   num_iterations=      128,
                                   num_leaves=          100,
                                   min_data_in_leaf=   1700,
                                   feature_fraction=      0.37,
                                   seed=               ksemilla   #aqui se utiliza SU primer semilla
                                  )
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
#Atencion ya NO corto por  1/60,  sino que busque el punto de corte optimo
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.integer(prediccion > 0.023)   ) ) #ATENCION  no es  1/60

#guardo el resultado
#creo las carpetas
dir.create( "~/R/labo/exp/",  showWarnings = FALSE ) 
dir.create( "~/R/labo/exp/KA5520/", showWarnings = FALSE )
setwd( "~/R/labo/exp/KA5520/" )

archivo_salida  <- "KA_552_001.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )


#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "552_importancia_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )


#cuento cuantos 1's tiene la prediccion
#cuantos estimulos estoy enviando para retener clientes
entrega[  , sum( Predicted ) ]
