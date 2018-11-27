## ------------------------------------------------------------------------
library(parlnet)
library(ggraph)
library(igraph)
library(lubridate)
library(Matrix)

adj_m <- adjacency_matrix_from_divisions(divisions,chamber_id = 'Senate',start_date = ymd('2018-01-01'))

## ------------------------------------------------------------------------
party_membership <- function(divisions,start_date = '1901-01-01',end_date='2100-01-01'){
	start_date <- ymd(start_date)
	end_date <- ymd(end_date)
	divisions %>%
		filter(date >= start_date,date <= end_date) %>%
		count(name,party) %>%
		group_by(name,party) %>%
		summarise(party = party[which.max(n)])
}
party_membership(divisions)

## ------------------------------------------------------------------------
adj_m <- adj_m > 100
adj_m <- adj_m[colSums(adj_m) > 0,colSums(adj_m) > 0]
adj_m %>% 
	graph_from_adjacency_matrix(mode='undirected') %>%
	ggraph() +
	geom_edge_link(alpha=0.25) + 
	geom_node_point(alpha=0.25) + 
	theme_graph() + 
	geom_node_text(aes(label=name))

