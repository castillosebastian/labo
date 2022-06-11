
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("data.table")
require("dplyr")
require("ggplot2")
require("stringr")


#Aqui se debe poner la carpeta de la computadora local
setwd("~/R/labo")  #Establezco el Working Directory


#cargo el dataset donde voy a entrenar
#dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)

#cargo el dataset nube
dataset  <- fread("./DT0001/paquete_premium.csv.gz")   

dataset = as_tibble(dataset)


#creo la carpeta donde va el experimento
# DA  representa  Exploratory Data Analysis
dir.create( "~/R/labo/exp/DA7050/", showWarnings = FALSE )
setwd("~/R/labo/exp/DA7050/")   

var_target = dataset %>% select(clase_ternaria) 

dataset = dataset %>% select_if(is.numeric)

variables = colnames(dataset)

pdf("boxplotvars.pdf")

for( var in  variables ){
  
  name_Var = colnames(dataset[var])
  
  dataset %>% select(variable = var) %>% 
    scale(center = TRUE, scale = TRUE) %>%   
    bind_cols(var_target) %>% 
    ggplot(aes(y=variable, color=as.factor(clase_ternaria))) + 
    geom_boxplot() +
    labs(title = str_c(name_Var, " (variables escaladas)"), x = '', y = '') 
}

dev.off()