# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# el resultado queda en  el bucket en  ./exp/KA7410/ 
# son varios archivos, subirlos inteligentemente a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory


kprefijo       <- "KA741"
ksemilla_azar  <- 102191  #Aqui poner la propia semilla
kdataset       <- "./datasets/paquete_premium_ext_721.csv.gz"

#donde entrenar
kfinal_mes_desde    <- 201912        #mes desde donde entreno
kfinal_mes_hasta    <- 202011        #mes hasta donde entreno, inclusive
kfinal_meses_malos  <- c( 202006 )   #meses a excluir del entrenamiento

#hiperparametros de LightGBM
#aqui copiar a mano lo menor de la Bayesian Optimization
# si es de IT y le gusta automatizar todo, no proteste, ya llegara con MLOps
kmax_bin           <-    31
klearning_rate     <-     0.0131094708
knum_iterations    <-   951
knum_leaves        <-  1002
kmin_data_in_leaf  <- 16918
kfeature_fraction  <-     0.5609450635



kexperimento   <- paste0( kprefijo, "0" )



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "~/buckets/b1" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(kdataset, stringsAsFactors= TRUE)



#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]

dataset[ foto_mes >= kfinal_mes_desde &
         foto_mes <= kfinal_mes_hasta &
         !( foto_mes %in% kfinal_meses_malos), 
         train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", kexperimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", kexperimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            kmax_bin,
                                   learning_rate=      klearning_rate,
                                   num_iterations=     knum_iterations,
                                   num_leaves=         knum_leaves,
                                   min_data_in_leaf=   kmin_data_in_leaf,
                                   feature_fraction=   kfeature_fraction,
                                   seed=               ksemilla_azar
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== 202101 ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
for( envios  in  c( 10000, 10500, 11000, 11500, 12000, 12500, 13000, 13500 ) )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  kexperimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )
