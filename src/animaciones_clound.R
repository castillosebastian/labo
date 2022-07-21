library(ggplot2)
library(gapminder)
library(gganimate)
library(dplyr)
library(data.table)
options(scipen = 999)
library(stringr)
library(directlabels)

## 'gapminder', 'gganimate', 
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset_grande  <- fread( "./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

#me quedo SOLO con los BAJA+2
dataset  <- copy( dataset_grande[  clase_ternaria =="BAJA+2"  & 
                                     foto_mes>=202001  & foto_mes<=202011, ] )

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset_grande[  numero_de_cliente %in%  dataset[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]
#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]


# data set continua
#me quedo SOLO con los BAJA+2
datasetC <- copy( dataset_grande[  clase_ternaria =="CONTINUA"  & 
                                     foto_mes>=202001  & foto_mes<=202011, ] )

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12C  <- copy( dataset_grande[  numero_de_cliente %in%  datasetC[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12C, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12C[  , pos := seq(.N) , numero_de_cliente ]
#me quedo solo con los 12 meses antes de morir
dataset12C  <- dataset12C[  pos <= 12 , ]
dataset12C <- dataset12C[ clase_ternaria == "CONTINUA", ]

dataset12$grupo = "grupo_baja"
dataset12C$grupo = "grupo_continua"

dataset = bind_rows(dataset12, dataset12C)

# Formateo fecha
df = dataset %>% 
  select(-c(clase_ternaria, numero_de_cliente)) %>% 
  mutate(date = as.Date(str_c(foto_mes, "01"), "%Y%m%d"))


# #variable nuevas
# df[ , mpayroll_sobre_edad  := mpayroll / cliente_edad ]
# df[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
# df[ , mv_status02       := Master_status +  Visa_status ]
# df[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
# df[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
# df[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
# df[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
# df[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
# df[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
# df[ , total_deuda_acumulada := rowSums( cbind( mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm=TRUE ) ] #top 11
# df[ , ratio_deuda_acumulada_antiguedad  := total_deuda_acumulada / cliente_antiguedad ]
# df[ , ratio_deuda_acumulada_cproductos_sobre_ctrx_q  := total_deuda_acumulada / ctrx_quarter ]
# df[ , mrentabilidad_sobre_antiguedad  := mrentabilidad / cliente_antiguedad ]
# df[ , mrentabilidad_annual_sobre_antiguedad  :=  mrentabilidad_annual/ cliente_antiguedad ]


# graficos temporales
# Señal temprana de baja
p = df %>% 
  filter(date > as.Date("2020-01-01")) %>% 
  group_by(date, grupo) %>% summarise_all(sum, na.rm = T) %>% 
  group_by(grupo) %>% mutate(ctrx_quarter_scaled = scale(ctrx_quarter)) %>% 
  ggplot(aes(y = ctrx_quarter_scaled, x=date, colour = grupo)) +
  geom_line() +
  theme(legend.position = "top", legend.title = element_blank()) +
  labs(title = "Señales de Baja: cantidad de transacciones",
       y = "Cantidad transacciones trimestrales", x = "") 
p + 
  geom_point() +
  transition_reveal(date)

# Propaga la señal a otras variables?
campos_buenos  <- c( "date", "grupo", "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_trx",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_trx", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")
# promedio
df_allvars = df %>% 
  filter(date > as.Date("2020-01-01")) %>% 
  group_by(date, grupo) %>%
  #summarise_all(sum, na.rm = T) %>% 
  summarise_all(median, na.rm = T) %>% 
  select(campos_buenos) %>% 
  filter(date != as.Date("2020-06-01"))

temp = df_allvars %>% ungroup() %>% select(-c(date, grupo)) %>% mutate_all(scale) 

df_allvars = temp %>% 
  mutate(grupo = df_allvars$grupo, 
         date = df_allvars$date)

df_allvars = df_allvars %>% 
  tidyr::pivot_longer(names_to = "variables", values_to = "valores", -c(grupo, date)) 

df_allvars %>% 
  ggplot(aes(y = valores, x=date, colour = variables)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Señales de Baja: cantidad de transacciones",
       y = "Cantidad transacciones trimestrales", x = "")  +
  facet_wrap(~grupo, scales = "free")


# Suma sin escalamiento
df_allvars = df %>% 
  filter(date > as.Date("2020-01-01")) %>% 
  select(campos_buenos) %>% 
  group_by(date, grupo) %>%
  summarise_all(sum, na.rm = T) %>% 
  filter(date != as.Date("2020-06-01"))

# temp = df_allvars %>% ungroup() %>% select(-c(date, grupo)) %>% mutate_all(scale)
# df_allvars = temp %>%
#   mutate(grupo = df_allvars$grupo,
#          date = df_allvars$date)

df_allvars = df_allvars %>% 
  tidyr::pivot_longer(names_to = "variables", values_to = "valores", -c(grupo, date)) 

df_allvars %>% 
  #filter(date > as.Date("2020-07-01")) %>% 
  ggplot(aes(y = valores, x=date, colour = variables)) +
  geom_line() +
  geom_dl(aes(label = variables), method = list(dl.trans(x = x + 4), "first.points", cex = 0.8)) +
  scale_y_log10() +
  scale_x_date( date_labels = "%b-%Y") +
  theme(legend.position = "none") +
  labs(title = "Señales de Baja: otras variables",
       y = "Otros datos bancarios", x = "")  +
  facet_wrap(~grupo, scales = "free")


# Límite de financiacion de la tarjeta de crédito, expresado en pesos.
# 
df_allvars = df %>% 
  filter(date > as.Date("2020-01-01")) %>% 
  select(campos_buenos) %>% 
  group_by(date, grupo) %>%
  summarise_all(sum, na.rm = T) %>% 
  filter(date != as.Date("2020-06-01"))

temp = df_allvars %>% ungroup() %>% select(-c(date, grupo)) %>% mutate_all(scale)

df_allvars = temp %>%
  mutate(grupo = df_allvars$grupo,
         date = df_allvars$date)

df_allvars = df_allvars %>% 
  tidyr::pivot_longer(names_to = "variables", values_to = "valores", -c(grupo, date)) 

df_allvars %>% 
  filter(str_detect(variables, "limite|consumo")) %>% 
  ggplot(aes(y = valores, x=date, colour = variables)) +
  geom_line() +
  geom_dl(aes(label = variables), method = list(dl.trans(x = x + 4), "first.points", cex = 0.8)) +
  scale_x_date( date_labels = "%b-%Y") +
  theme(legend.position = "none") +
  labs(title = "Señales de Baja: Consumo Visa-Master",
       y = "Otros datos bancarios", x = "")  +
  facet_wrap(~grupo, scales = "free")

# # Comparacion en otras variables
library(lattice)
temp = df %>% 
  select(grupo, everything(), -date)
parallelplot(~temp[2:159] | grupo, data=temp)


# utiles

# vuelvo a filtrar el período estudiado
# dataset12 <- dataset12[ foto_mes<=202011, ] 
# # Incio animacion
# df = dataset12
# df = df[ , c("ctrx_quarter","mcaja_ahorro", "clase_ternaria", "cliente_edad", "foto_mes", "cproductos"), with=FALSE]
# 
# p = df %>% 
#   filter(foto_mes >= 202001) %>% 
#   ggplot(aes(y = ctrx_quarter, x=mcaja_ahorro, size = cproductos, colour = as.factor(clase_ternaria))) +
#   geom_point(show.legend = F, alpha = 0.3) +
#   scale_color_viridis_d() +
#   # scale_size(range = c(2, 12)) +
#   scale_x_log10() +
#   labs(y = "Cantidad transacciones trimestrales", x = "Monto total caja ahorro") +
#   facet_wrap(~clase_ternaria)
# 
# panimet = p + transition_time(foto_mes) +
#   labs(title = "foto_mes: {frame_time}") 
# 
# animate(panimet, duration = 80)

#-----
# Trabajar con el subset de baja+2, luego agrupar por mes y summarizar_all con mediana
# luego graficar las lineas de evolucion de cada variable del grupo y buscar tendencia.

dataset12 <- dataset12[ foto_mes<=202011, ]

df_summary = dataset12 %>% 
  filter(foto_mes >= 201903 & foto_mes < 202012) %>% 
  select(-clase_ternaria, -numero_de_cliente) %>% 
  group_by(foto_mes) %>% 
  summarise_all(median, na.rm = T) 

df_scale = as.data.frame(scale(df_summary)) %>% 
  mutate(foto_mes = df_summary$foto_mes) %>% 
  select_if(~ !any(is.na(.)))