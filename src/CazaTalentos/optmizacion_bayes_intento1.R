
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#Defino la  Optimizacion Bayesiana

kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
  makeNumericParam("tirosr1", lower= 1L, upper=  20L),
  makeIntegerParam("tirosr2", lower= 1L, upper= 400L),  #la letra L al final significa ENTERO
  makeIntegerParam("tirosr3", lower= 1L, upper= 3000L) #,
  #forbidden = quote( minbucket > 0.5*minsplit )  # minbuket NO PUEDE ser mayor que la mitad de minsplit
  )            

ksemilla_azar  <- 777781   #cambiar por la primer semilla



# Sample de 10 sin sustituciona: "la eliminatoria"
# Ronda final: todos contra todos gane el mejor

# luego optimizacion de parametros
# tal vez nuevo modelo de datos