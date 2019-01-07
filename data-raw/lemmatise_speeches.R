## Lemmatise

devtools::install()


temp <- options()$mc.cores
options(mc.cores = 4)

library(tidyverse)
library(parlnet)
library(textstem)
library(furrr)
library(lubridate)

speeches_split <- unique(year(speeches$date)) %>%
  map(~filter(speeches,year(date) == .x)) 

text_split <- map(speeches_split,~pull(.x,text))

plan(multiprocess)

text_lemmatised <- future_map(text_split,~map_chr(.x,lemmatize_strings))

speeches_lemmatised <- map2(speeches_split,text_lemmatised,function(x,y){ 
				    x$text_lemmatised <- y
				    x}) %>%
	bind_rows

usethis::use_data(speeches_lemmatised,overwrite=TRUE)

options(mc.cores = temp)
