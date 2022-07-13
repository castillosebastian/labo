library(data.table)
library(dplyr) 
library(stringr)
library(ggplot2)
library(forcats)

# data_dir = setwd("~/R/labo/exp/archivo")
# setwd("~/buckets/b1/exp/8101HTa")
setwd("~/R/labo/exp/impofiles")

filenames <- list.files(pattern="impo_*")

tbl <- do.call(rbind, 
               lapply(filenames, function(x) 
                 cbind(fread(x, stringsAsFactors = FALSE), filename = x)))

# tbl %>% 
#   filter(!str_detect(Feature, "foto")) %>% 
#   filter(pos < 15) %>% 
#   mutate(Gain = round(Gain, digits = 4)) %>% 
#   ggplot(aes(fill=Feature, y=Gain, x=filename)) + 
#   geom_point( stat="identity") +
#   ggrepel::geom_label_repel(aes(label = Feature)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position = "none") 

tbl = tbl %>% 
  group_by(filename) %>% 
  mutate(pos = row_number())

nfeatures = 35

tbl %>% 
  filter(!str_detect(Feature, "foto")) %>% 
  filter(pos < nfeatures) %>%
  ggplot(aes(Gain, fct_reorder(Feature, Gain))) +
  geom_col() +
  geom_col(aes(fill = filename)) +
  facet_wrap(~ filename, nrow = 1) +
  theme_minimal() +
  theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#3e3d40", color = "#3e3d40"),
      strip.text.x = element_text(hjust = 0, color = "#dcbf35"),
      axis.title.x.top = element_text(color = "#dcbf35", hjust = 0),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dcbf35"),
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(colour = "white", size = 6),
      axis.title.y = element_blank(),
      axis.title.x = element_text(hjust = 0.5, colour = "grey"),
      axis.text.x = element_text(color = "#dcbf35"),
      plot.title = element_text(colour = "grey")) +
  labs(title = str_c("Top ", nfeatures," Features Gain by Experiment"))



