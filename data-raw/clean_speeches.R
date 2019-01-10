library(readr)
library(dplyr)
library(purrr)
library(furrr)
library(stringr)
library(tidytext)
library(lubridate)

speeches <- dir('tidied_parliamentary_data/speeches/',full.names=TRUE) %>%
	map_dfr(~read_csv(.x,col_types = cols(name_id = col_character()))) 

#The file format changes in 2011, and speeches before and after the change need 
#to be handled differently

format_change_date <- lubridate::ymd('2011-05-10')

speeches_pre_change <- speeches %>%
	filter(date < format_change_date)

speeches_post_change <- speeches %>%
	filter(date >= format_change_date )

## Tidy the new speeches, which requires a small amount of extra work

speeches_post_change <- speeches_post_change %>%  
	mutate(text = str_split(text,':')) %>% 
	mutate(text = map(text,function(x) x[-c(1,2)])) %>%
	mutate(text = map_chr(text,~paste(.x,collapse = ' '))) %>%
        mutate(text = str_trim(text))

speeches <- bind_rows(speeches_pre_change,speeches_post_change)

## Tidy up chamber identifiers
## REPS --> HoR
## SENATE --> Senate

speeches <- speeches %>%
	mutate(chamber = if_else(chamber == 'REPS','HoR',chamber)) %>%
	mutate(chamber = if_else(chamber == 'SENATE','Senate',chamber))

## tidy up names

speeches <- speeches %>%
	mutate(role = str_extract(name,'\\(.+\\)')) %>%
	mutate(name = str_remove(name,'\\(.+\\)')) %>%
	mutate(name = str_trim(name)) %>%
        mutate(debate_title = str_remove(debate_title,'<title>'),subdebate_title = str_remove(subdebate_title,'<title>')) %>%
        mutate(debate_title = str_remove(debate_title,'</title>'),subdebate_title = str_remove(subdebate_title,'</title>')) %>%
        mutate(name = str_remove(name,'Senator')) %>% #sometimes given as "Senator" rather than "Sen"
        mutate(name = str_remove(name,'Sen')) %>% #redundant information because the chamber is given elsewhere
	mutate(name_abbreviated = str_split(name,' ')) %>% #is the correct? - what about e.g. Albertus Johannes van Manen 
	mutate(name = str_remove(name,'MP$')) %>%
	mutate(name = str_remove(name,',$')) %>%
	mutate(name = str_remove(name,'^Mr ')) %>%
	mutate(name = str_trim(name)) %>%
	mutate(name = str_squish(name))

procedural_roles_regex <- '(President)|(Speaker)|(Chair)|(Clerk)'#make sure to ignore case
speeches <- speeches %>%
  mutate(role = if_else(grepl(procedural_roles_regex,name),name
                        ,role)) %>%
  mutate(name = if_else(grepl(procedural_roles_regex,name,ignore.case=TRUE),NA_character_,name))
 

## Write data

usethis::use_data(speeches,overwrite=TRUE)


## Stop words

parliamentary_stop_words <- read_csv('parliamentary_stop_words.csv',col_names=c('word'))

usethis::use_data(parliamentary_stop_words,overwrite=TRUE)
