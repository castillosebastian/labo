library(purrr)
library(data.table)
library(readr)
library(stringr)
#setwd("~/R/labo/exp/HT7310")
setwd("~/R/labo/datasets/precios_merval")


read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

tbl_with_sources %>% 
  mutate(tiker = filename) %>% 
  mutate(tiker = str_remove_all(filename, '.BA.csv')) %>% 
  mutate(tiker = str_remove_all(filename, '.BA.csv'))
