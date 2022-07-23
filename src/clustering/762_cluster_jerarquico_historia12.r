#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")


setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset_grande  <- fread( "./exp/8101FEa/paquete_premium_ext.csv.gz", stringsAsFactors= TRUE)

#me quedo SOLO con los BAJA+2
dataset  <- copy( dataset_grande[  clase_ternaria =="BAJA+2"  & foto_mes>=202001  & foto_mes<=202011, ] )

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset_grande[  numero_de_cliente %in%  dataset[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]

#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering
setwd( "~/buckets/b1/exp/8101FEa/" )  
filenames <- list.files(pattern="impo_*", recursive = T)

tbl <- do.call(rbind, 
               lapply(filenames, function(x) 
                 cbind(fread(x, stringsAsFactors = FALSE), filename = x)))

tbl = tbl %>% 
  group_by(filename) %>% 
  mutate(pos = row_number())

# tbl = tbl %>% 
#   mutate(experimento = str_sub(filename, 1, 7),
#          top_modelos = as.integer(str_sub(filename, 14,15))) 

features_summary <- tbl %>% 
  group_by(Feature) %>% 
  summarise(ganancia_media = mean(Gain, na.rm = T),
            ganancia_total = sum(Gain, na.rm = T),
            ganancia_max = max(Gain, na.rm = T),
            ganancia_min = min(Gain, na.rm = T),
            ganancia_sd = round(sd(Gain, na.rm = T), digits = 2),
            posicion_promedio = round(median(pos, na.rm=T)),
            ocurrencias = n()) %>% 
  arrange(desc(ganancia_media))

#por supuesto, se pueden cambiar
# campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_trx",
#                      "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
#                      "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_trx", "Visa_msaldopesos",
#                      "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
#                      "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
#                      "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
#                      "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
#                      "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
#                      "mpagomiscuentas")


# #paso los infinitos a NULOS
# infinitos <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
# infinitos_qty <- sum( unlist( infinitos) )
# if( infinitos_qty > 0 )
# {
#   cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
#   dataset[mapply(is.infinite, dataset)] <<- NA
# }
# 
# #valvula de seguridad para evitar valores NaN que es 0/0
# #paso los NaN a 0 , decision polemica si las hay
# #se invita a asignar un valor razonable segun la semantica del campo creado
# nans <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
# nans_qty <- sum( unlist( nans) )
# if( nans_qty > 0 )
# {
#   cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
#   cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
#   dataset[mapply(is.nan, dataset)] <<- 0
# }







campos_buenos <- features_summary$Feature[1:20]


#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#primero, creo la carpeta donde van los resultados
dir.create( "./exp/", showWarnings= FALSE )
dir.create( "./exp/ST7620", showWarnings= FALSE )
setwd( "~/buckets/b1/exp/ST7620" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 7 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_trx),  cluster2 ]


#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo

dataset12[ dataset,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )
