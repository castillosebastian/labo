library(purrr)
library(data.table)
library(dplyr)

setwd("~/R/labo/datasets")

tbl_fread <- 
  list.files(pattern = "*.txt") %>% 
  map_df(~fread(.))

features_summary <- tbl_fread %>% 
  group_by(Feature) %>% 
  summarise(sum_gain = sum(Gain, na.rm = T),
            ocurrencias = n()) %>% 
  arrange(desc(ocurrencias), desc(sum_gain))

cliente_baja2 = paquete_premium_202011$numero_de_cliente[paquete_premium_202011$clase_ternaria == "BAJA+2"]
cliente_continua = paquete_premium_202011$numero_de_cliente[paquete_premium_202011$clase_ternaria == "CONTINUA"]

fotomescliente = paquete_premium_202011 %>% filter(numero_de_cliente == 60888266)
fotomeses <- t(fotomescliente) %>% as.data.frame() %>% setNames(fotomescliente[,1])