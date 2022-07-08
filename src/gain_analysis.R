library(data.table)
library(dplyr) 
library(stringr)
library(ggplot2)
library(forcats)

# data_dir = setwd("~/R/labo/exp/archivo")
# setwd("~/buckets/b1/exp/8101HTa")
setwd("~/R/labo/datasets")

filenames <- list.files(pattern="bo_log_20*")

tbl <- do.call(rbind, 
               lapply(filenames, function(x) 
                 cbind(fread(x, stringsAsFactors = FALSE), filename = x)))

tbl %>%
  filter(!str_detect(filename, "10|11")) %>% 
  group_by(filename) %>% 
  arrange(desc(ganancia)) %>% 
  slice(1:15) %>% 
  ggplot(aes(fill=filename, x=filename, y=ganancia/1000000)) +
  geom_boxplot( ) +
  labs(title = "Entrenando modelos en datos mensuales y ganancia obtenidas en los 15 mejores")

