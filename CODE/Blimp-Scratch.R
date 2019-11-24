suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(recipes))

id_freq <- input_orig %>% count(id) %>% 
  mutate(perc = round(100*(n/sum(n)), 1), cum_per = round(100*(cumsum(n)/sum(n)), 1))

id_dup <- id_freq %>% filter(n == 2)
