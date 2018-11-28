library(dplyr)
library(purrr)
library(readr)
library(Matrix)
library(stringr)


divisions <- dir('tidied_parliamentary_data/divisions',full.names=TRUE) %>% 
	map(~read_csv(.x,col_types=cols())) %>%
	keep(~nrow(.x) > 0) %>%
	bind_rows 

divisions <- divisions %>%
	mutate(chamber = str_replace(chamber,'SENATE','Senate')) %>%
	mutate(chamber = str_replace(chamber,'REPS','HoR'))

#tidy up the names field
divisions <- divisions %>%
	mutate(name = str_replace(name,'\\(teller\\)','')) %>%
	mutate(name = str_replace_all(name,'\\. ','')) %>%
	mutate(name = str_replace(name,'Teller','')) %>%
	mutate(name = str_replace_all(name,'\\.','')) %>%
        mutate(name = str_replace_all(name,'\\*','')) %>%
	mutate(name = str_trim(name)) %>%
	mutate(name = str_replace(name,'\\(proxy\\)',''))

#add a single variable to the divisions data frame that acts as a primary key for the division
divisions_pk <- divisions %>%
	select(parent_title,grandparent_title,date,chamber,debate_type,debate_id,subdebate_id,division_id) %>%
	distinct %>%
	mutate(division_pk = row_number())

divisions <- left_join(divisions,divisions_pk)

usethis::use_data(divisions,overwrite=TRUE)

