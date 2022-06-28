library(data.table)
library(dplyr) 
library(stringr)
library(ggplot2)

data_dir = setwd("~/R/labo/exp/archivo")

filenames <- list.files(pattern="impo_*", recursive = T)

tbl <- do.call(rbind, 
               lapply(filenames, function(x) 
                 cbind(fread(x, stringsAsFactors = FALSE), filename = x)))
tbl = tbl %>% 
  mutate(experimento = str_sub(filename, 1, 7),
         top_modelos = as.integer(str_sub(filename, 14,15))) 
        
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


features_summary %>% 
  filter(posicion_promedio < 20 & !str_detect(Feature, "ctrx_quart")) %>% 
  ggplot2::ggplot(aes(y=ganancia_media, x=posicion_promedio, label=Feature)) +
  geom_point() + 
  ggrepel::geom_label_repel()
  