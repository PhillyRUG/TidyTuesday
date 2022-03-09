
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

## libraries

library(tidyverse)
library(ggraph)
library(tidygraph)
library(countrycode)
library(ggflags)

## test the countrycode;

countrycode(c('AL'), origin = 'iso3c', destination = 'cown')

flag_list <- c('ac','ad','ae','af','ag','az','ba','bb','bd','be','bs','bt','bv','bw','by','cn','co','cp','cr','cu','dz','ea','ec','ee','eg','ga','gb','gd','ge','gf','gu','gw','gy','hk','hm','iq','ir','is','it','je','kw','ky','kz','la','lb','md','me','mg','mh','mk','mw','mx','my','mz','na','om','pa','pe','pf','pg','re','ro','rs','ru','rw','sm','sn','so','sr','ss','tj','tk','tl','tm','tn','uz','va','vc','ve','vg')

## processing 

x <- erasmus %>% 
  rename(sending_country = sending_country_code,
         receiving_country = receiving_country_code) %>%
  mutate(receiving_country = stringr::str_to_lower(receiving_country),
         sending_country = stringr::str_to_lower(sending_country)) %>%
  count(sending_country, receiving_country) %>%
  filter(!is.na(sending_country), !is.na(receiving_country)) %>%
  filter(sending_country %in% flag_list, receiving_country %in% flag_list) %>%
  arrange(desc(n)) %>%
  top_n(200) %>%
  mutate(sending_country = ifelse(sending_country == "uk", "gb", sending_country),
         receiving_country = ifelse(receiving_country == "uk", "gb", receiving_country)) %>%
  as_tbl_graph(.) %>%
  mutate(Popularity = centrality_degree(mode = 'in'))

## graph 

ggraph(x, layout = 'linear', circular = TRUE) + 
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(size = Popularity)) + 
  #facet_edges(~year) + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
  geom_node_point(size = 6, alpha = 0.4) +
  #geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  theme_void() +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
  geom_flag(aes(country = name, x = x, y = y), size = 10)  +
  theme(legend.position='none')
