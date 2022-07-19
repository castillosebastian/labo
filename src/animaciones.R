library(gifski)
library(ggplot2)
library(gapminder)
library(gganimate)
library(dplyr)
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/

df = fread( "~/buckets/b1/exp/8122TSc/TS_train_final.csv.gz" )

df = paquete_premium_202011

df = df[ , c("ctrx_quarter","mcaja_ahorro", "clase_ternaria", "foto_mes", "cproductos"), with=FALSE]

df %>% 
  ggplot(aes(x = ctrx_quarter, y=mcaja_ahorro, size = cproductos, colour = as.factor(clase_ternaria))) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Cantidad transacciones trimestrales", y = "Monto total caja ahorro")+
  facet_wrap(~clase_ternaria)

p + transition_time(year) +
  labs(title = "Year: {frame_time}") 