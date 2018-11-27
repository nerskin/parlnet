#' Generate an adjacency matrix based on how often MPs vote together
#'
#' @param divisions the data frame containing the votes
#' @param chamber_id either 'HoR' for the House of Representatives or 'Senate' for the Senate
#' @param start_date the start of the time period to consider
#' @param end_date the end of the time period to consider
#' @return a matrix where the $(i,j)$th entry is the number of times the ith legislator votes with the jth legislator in the period considered
#' @export
#' @import dplyr Matrix purrr
adjacency_matrix_from_divisions <- function(divisions,chamber_id ='HoR',start_date = '2000-01-01',end_date = '2020-01-01'){



	co_occurrence <- divisions %>% 
		filter(date >= lubridate::ymd(start_date)) %>%
		filter(date <= lubridate::ymd(end_date)) %>%
		filter(chamber == chamber_id) %>%
		select(name,vote,division_pk) %>% 
		group_by(vote,division_pk) %>% 
		summarise(members = list(name)) %>% 
		filter(map_int(members,length) > 1  )  %>%
	      	mutate(members = map(members,~as_tibble(t(combn(.x,2))))) %>% 
		pull(members) %>% 
		bind_rows %>% 
		count(V1,V2) 

	names <- unique(c(co_occurrence$V1,co_occurrence$V2))
	
	i <- match(co_occurrence$V1,names)
	j <- match(co_occurrence$V2,names)
	
	adjacency_matrix <- sparseMatrix(i,j,x=co_occurrence$n,
	                                 dims = rep(length(names),2),
	                                 dimnames = list(names,names))
	
	adjacency_matrix + t(adjacency_matrix)

}
