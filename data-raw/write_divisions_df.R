library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(Matrix)
library(stringr)

if (!('parlnet' %in% installed.packages()[,1])){
	devtools::install()
}

library(parlnet)

###horrible
if (last(unlist(str_split(getwd(),'/'))) == 'parlnet'){
  setwd('data-raw')
}

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

# get party membership - challenge here is that party membership is given in the 'speeches' section
# of the XML, but names are given differently in the speeches section than in the divisions section

load('../data/speeches.rda')

library(fuzzyjoin)#for joining by _approximate_ string matches



divisions_HoR <- filter(divisions,chamber == 'HoR')
divisions_senate <- filter(divisions,chamber == 'Senate')

HoR_election_dates <- c(read_csv('HoR_election_dates.csv')$date,lubridate::ymd('2100-01-01'))
senate_start_dates <- c(read_csv('senate_dates.csv')$start_date,lubridate::ymd('2100-01-01'))

divisions_HoR_split <- map(1:length(HoR_election_dates),~filter(divisions_HoR,date >= HoR_election_dates[.x],date <= HoR_election_dates[.x + 1])) %>%
	keep(~nrow(.x) > 0)
divisions_senate_split <- map(1:length(senate_start_dates),~filter(divisions_senate,date  >= senate_start_dates[.x],date <= senate_start_dates[.x + 1])) %>%
  keep(~nrow(.x) > 0)

add_parties <- function(x){
	start_date <- min(x$date)
	end_date <- max(x$date)
	party_members <- party_membership(start_date=start_date,end_date=end_date)

	party_members <- mutate(party_members,name=str_to_lower(name))  #avoids a small number of problems e.g. van Manen vs. Van Manen
	
	x  <- x %>%
	  filter(!is.na(name))
	
	x_name_na <- x %>%
	  filter(is.na(name))
	
	party_members <- party_members %>%
		mutate(name = str_split(name,', ')) %>%
		mutate(name = map(name,function(x){
					  x[length(x)] <- substring(x[length(x)],1,1)
					  x
				})) %>%
		mutate(name = map_chr(name,~paste(.x,collapse=', '))) %>%
		filter(complete.cases(.))
	party_members
	x <- mutate(x,name_lower = str_to_lower(name))
	
	names_key <- x %>%
	  select(name_lower) %>%
	  stringdist_left_join(party_members,by=c(name_lower = 'name'),max_dist=3) %>%
	  mutate(string_distance = stringdist::stringdist(name_lower,name)) 
	
	write_csv(names_key,'potential_party_misallocations.csv',append=TRUE)
	
	
	names_key <- names_key %>%
	  group_by(name_lower) %>%
	  summarise(party = (function(party,string_distance){
	    if (all(is.na(party))){
	      return(NA_character_)
	    }
	    else {
	      party <- party[!is.na(party)]
	      string_distance <- string_distance[!is.na(party)]
	      party <- party[which.min(string_distance)]
	    }
	  })(party,string_distance))

	left_join(x,names_key) %>%
	  bind_rows(x_name_na)
	  
	
	#TODO: manually handle clashes and ambiguous cases
	
}


divisions_HoR <- map_dfr(divisions_HoR_split,add_parties)
divisions_senate <- map_dfr(divisions_senate_split,add_parties)

divisions <- bind_rows(divisions_HoR,divisions_senate)

usethis::use_data(divisions,overwrite=TRUE)
