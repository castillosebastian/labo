library(purrr)
library(data.table)

setwd("~/R/labo/exp/HT7310")

tbl_fread <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))

features_summary <- tbl_fread %>% 
  group_by(Feature) %>% 
  summarise(sum_gain = sum(Gain, na.rm = T),
            ocurrencias = n()) %>% 
  arrange(desc(sum_gain))