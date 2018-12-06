## Lemmatise

devtools::install()

library(tidyverse)
library(parlnet)
library(textstem)
library(furrr)
library(lubridate)

speeches_split <- unique(year(speeches$date)) %>%
  map(~filter(speeches,year(date) == .x)) 

plan(multiprocess)

speeches_lemmatised <- future_map(speeches_split,function(x){
  res <- mutate(x,text_lemmatised = map_chr(text,lemmatize_strings))
  #print(paste0('Done ',year(x$date)[1]))
  res
})

speeches_lemmatised <- speeches_lemmatised %>%
  bind_rows() %>%
  select(-text)


usethis::use_data(speeches_lemmatised,overwrite=TRUE)
