library(data.table)
library(dplyr) 
library(stringr)
library(ggplot2)
library(forcats)

# data_dir = setwd("~/R/labo/exp/archivo")
# setwd("~/buckets/b1/exp/8101HTa")
setwd("~/R/labo/src/clustering/clustering1")

df = fread("cluster_de_bajas.txt", stringsAsFactors=FALSE)

df = df %>% mutate(cluster2 = as.factor(cluster2))

df %>% ggplot() +
  geom_point(mapping = aes(x = cliente_antiguedad, 
                           y = ctrx_quarter, 
                           colour = as.factor(cluster2)))

mean_vectors = df %>%
  group_by(cluster2) %>%
  summarise_all("mean") %>% 
  t() %>% as.data.frame() %>%
  mutate(features = row.names(.))

g1 = df %>% 
  ggplot(aes(x = cluster2, y = ctrx_quarter)) +
  geom_boxplot()+
  theme_bw()+
  xlab("Number of groups (K)")

my_pal <- RColorBrewer::brewer.pal(n=7, name = "Dark2")

g2 = df %>% 
  ggplot(aes(x = cliente_antiguedad, y = ctrx_quarter, color = cluster2, fill = cluster2)) +
  geom_point(size = 4, shape = 21) +
  theme_bw() +
  scale_color_manual(values=c(my_pal)) +
  scale_fill_manual(values=c(paste(my_pal, "66", sep = "")))

library(GGally)
library(plotly)

df_small <- df %>% 
  select(ctrx_quarter, Visa_Finiciomora, Visa_mconsumototal, Master_mconsumospesos, Visa_mconsumospesos, cluster2) 

p = ggparcoord(data = df_small, 
               groupColumn = "cluster2", 
               splineFactor = TRUE, 
               scale = "robust",
               # scale = "uniminmax",
               # scale = "centerObs",
               # scale = "globalminmax",
               ) +
  scale_color_brewer(palette = "Set2") + 
  labs(x = "Baja+2 Clientes", y = "Valores", title = "Clustering")

ggplotly(p)


