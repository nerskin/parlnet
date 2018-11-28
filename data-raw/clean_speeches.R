library(readr)
library(dplyr)
library(purrr)
library(furrr)
library(stringr)
library(tidytext)


speeches <- dir('tidied_parliamentary_data/speeches/',full.names=TRUE) %>%
	map_dfr(~read_csv(.x,col_types = cols())) 

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


## tidy up names

speeches <- speeches %>%
	mutate(role = str_extract(name,'\\(.+\\)')) %>%
	mutate(name = str_remove(name,'\\(.+\\)')) %>%
	mutate(name = str_trim(name)) %>%
        mutate(debate_title = str_remove(debate_title,'<title>'),subdebate_title = str_remove(subdebate_title,'<title>')) %>%
        mutate(debate_title = str_remove(debate_title,'</title>'),subdebate_title = str_remove(subdebate_title,'</title>')) %>%
        mutate(name = str_remove(name,' Sen')) %>% #redundant information because the chamber is given elsewhere
	mutate(name = str_remove(name,' MP')) %>%
	mutate(name_abbreviated = str_split(name,' ')) #is the correct? - what about e.g. Albertus Johannes van Manen

speeches <- speeches %>%
  mutate(role = if_else(grepl('(President)|(Speaker)|(Chair)',name),name
                        ,role)) %>%
  mutate(name = if_else(grepl('(President)|(Speaker)|(Chair)',name,ignore.case=TRUE),NA_character_,name))



## Lemmatise

library(textstem)

progressively <- function(.f, .n, ...) {
  pb <- progress::progress_bar$new(total = .n, ...)
  function(...) {
    pb$tick()
    .f(...)
  }
}

progress_fn <- progressively(function(text){lemmatize_strings(text)},nrow(speeches))

#speeches_lemmatised <- mutate(speeches,text_lemmatised = map_chr(text,progress_fn)) %>%
	#select(-text)


## Write data

usethis::use_data(speeches,overwrite=TRUE)
#usethis::use_data(speeches_lemmatised,overwrite=TRUE)

