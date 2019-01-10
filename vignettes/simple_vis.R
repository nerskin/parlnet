## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE,eval=TRUE,cache=TRUE,autodep=TRUE)

## ------------------------------------------------------------------------
library(parlnet)
library(ggraph)
library(igraph)
library(lubridate)
library(Matrix)
library(dplyr)
library(stringr)
library(purrr)
library(fuzzyjoin)

start_date <- ymd('2016-07-03')
adj_m <- adjacency_matrix_from_divisions(divisions,chamber_id = 'Senate',start_date=start_date)
threshold <- quantile(adj_m,0.5)

## ------------------------------------------------------------------------
party_membership <- function(start_date = '1901-01-01',end_date='2100-01-01'){
	start_date <- lubridate::ymd(start_date)
	end_date <- lubridate::ymd(end_date)
	parlnet::speeches %>%
		filter(date >= start_date) %>%
		filter(date <= end_date) %>%
		count(name,party) %>%
		group_by(name) %>%
		summarise(party = party[which.max(n)])
}
party_members <- party_membership(start_date = start_date) %>%
  mutate(name = str_split(name,', ')) %>%
  mutate(name = map(name,function(x){
    x[length(x)] <- substring(x[length(x)],1,1)
    x
  }))
party_members <- party_members %>%
  mutate(name = map_chr(name,~paste(.x,collapse=', '))) %>%
  filter(!is.na(party))

## ------------------------------------------------------------------------
library(tidygraph)
adj_m <- adj_m > threshold
adj_m <- adj_m[colSums(adj_m) > 0,colSums(adj_m) > 0]
graph_representation <- adj_m %>% 
	graph_from_adjacency_matrix(mode='undirected') %>%
  as_tbl_graph()

nodes_tbl <- graph_representation %>%
  activate(nodes) %>%
  as_tibble %>%
  stringdist_left_join(party_members,max_dist = 2) %>%
  mutate(string_distance = stringdist::stringdist(name.x,name.y)) %>%
  mutate(string_distance = if_else(is.na(string_distance),Inf,string_distance)) %>%
  group_by(name.x) %>%
  summarise(party = party[which.min(string_distance)])

print(nodes_tbl)

graph_representation <- graph_representation %>%
  activate(nodes) %>%
  mutate(party = nodes_tbl$party) %>%
  mutate(party = str_to_lower(party)) #to avoid e.g. NATS vs Nats problem

print(graph_representation) 

network_plot <- graph_representation %>% 
  activate(nodes) %>%
  create_layout('fr') %>%
  ggraph() +
  theme_graph() + 
  geom_edge_link(alpha=0.01) + 
  geom_node_point(aes(colour = party))
print(network_plot)

## ------------------------------------------------------------------------
graph_representation %>%
  activate(nodes) %>%
  filter(is.na(party))

## ------------------------------------------------------------------------
senate_divisions_split <- map(1:nrow(senate_dates),~filter(divisions,date >= senate_dates[.x,]$start_date,date < senate_dates[.x,]$end_date)) %>%
  keep(~nrow(.x) > 0) 

## ------------------------------------------------------------------------
senate_networks <- senate_divisions_split %>%
  map(~adjacency_matrix_from_divisions(.x,start_date = min(.x$date),end_date = max(.x$date))) %>% 
  map(~.x > quantile(.x,0.5)) %>%
  map(~graph_from_adjacency_matrix(.x,mode='undirected',weighted=NULL)) %>%
  map(as_tbl_graph) %>%
  map(~activate(.x,nodes))

party_membership <- senate_divisions_split %>%
  map(~group_by(.x,name) %>%
        summarise(party = party[1]))

senate_networks <- map2(senate_networks,party_membership,~left_join(.x,.y)) %>%
  map(~ggraph(.x) +
        geom_node_point() +
        geom_edge_link()
  )

