## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE)

## ------------------------------------------------------------------------
library(tidyverse)
library(quanteda)
library(tidytext)
library(quanteda)
library(parlnet)
library(igraph)
library(ggraph)
library(tidygraph)

## ----cache=TRUE----------------------------------------------------------
common_terms <- speeches_lemmatised %>%
  select(text_lemmatised) %>%
  unnest_tokens(word,text_lemmatised) %>%
  anti_join(stop_words) %>%
  filter(grepl('^[a-z]+$',word)) %>%
  count(word) %>%
  top_n(5000,n)

## ----cache=TRUE,autodep=TRUE---------------------------------------------
parliament_dfm <- speeches_lemmatised %>%
  filter(date >= lubridate::ymd('2016-07-03')) %>%
  unnest_tokens(word,text_lemmatised) %>%
  semi_join(common_terms) %>%
  count(name,word) %>%
  filter(grepl('^[a-z]+$',word)) %>%
  filter(complete.cases(.)) %>%
  cast_dfm(name,word,n)

## ------------------------------------------------------------------------
similarity_matrix <- textstat_simil(parliament_dfm,method = 'ejaccard') %>%
  as.matrix

dissimilarity_matrix <- textstat_dist(parliament_dfm)

k <- 10

clusters <- kmeans(dissimilarity_matrix,centers=10)$cluster %>%
  as.data.frame %>%
  rownames_to_column() %>% 
  set_names('member','cluster')

clusters %>%
	arrange(cluster)

## ------------------------------------------------------------------------
adj_m <- apply(similarity_matrix,function(x)x > quantile(x,0.95),MARGIN=2)

adj_m %>%
  graph_from_adjacency_matrix %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(name_sparse = ifelse(runif(length(name)) < 0.05,name,NA_character_)) %>%
  ggraph() + 
  geom_edge_link(alpha = 0) + 
  theme_graph() + 
  geom_node_text(aes(label=name_sparse))

## ------------------------------------------------------------------------
similarity_matrix %>%
  as.data.frame %>% 
  rownames_to_column('member1') %>%
  gather(key='member2',value='similarity',-member1) %>%
  left_join(clusters,by=c(members='members') %>%
  ggplot(aes(member1,member2,fill=similarity)) + 
  geom_tile() +
  scale_fill_viridis() 

## ------------------------------------------------------------------------
library(quanteda)
data_corpus <- speeches_lemmatised %>%
  filter(date >= lubridate::ymd('2016-07-03')) %>%
  pull(text_lemmatised) %>%
  corpus

fcm <- data_corpus %>%
  tokens(remove_punct=TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords('english'),padding=FALSE) %>%
  fcm(context='window',window=5,tri=FALSE)

topfeats <- names(topfeatures(fcm,30))

textplot_network(fcm_select(fcm,topfeats),min_freq=0.95)

