rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de SU computadora local
setwd("D:\\gdrive\\Austral2022R\\")  #Establezco el Working Directory

#cargo la salida del Grid Seach, verifique que corresponda a la carpeta donde dejó el resultado
dtrain  <- fread("./labo/exp/HT2020/gridsearch.txt")

#genero el modelo,  aqui se construye el arbol
#este sera un arbol de REGRESION ya que la variable objetivo, ganancia_promedio,  es una variable continua
modelo  <- rpart("ganancia_promedio ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 cp=         0,
                 minsplit=  50,     #minima cantidad de registros para que se haga el split
                 minbucket= 10,     #tamaño minimo de una hoja
                 maxdepth=   4 )    #profundidad maxima del arbol


#grafico el arbol

#primero creo la carpeta a donde voy a guardar el dibujo del arbol
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/ST2030/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/ST2030/arbol_analisis_gridsearch.pdf"

#finalmente, genero el grafico guardandolo en un archivo pdf
pdf( archivo_salida, paper="a4r" )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
