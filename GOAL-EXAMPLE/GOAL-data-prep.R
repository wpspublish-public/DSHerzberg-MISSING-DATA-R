# required packages
suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(scales))

# input parameters. Input file is .csv, with person ID column on far-left, and
# remaining columns hold items, numbered in i001, i002, i003 etc. format.
id <- c('id')
first_item <- c('i001')
last_item <- c('i132')
file_name <- c('GOAL-data-i-num')

# read input
input_orig <- suppressMessages(read_csv(here(
  paste0('INPUT-FILES/', file_name, '.csv')
))) %>% 
  select(id, first_item:last_item)
names_input_orig <- names(input_orig)
# Count NA across all columns
NA_count <- sum(is.na(input_orig))
NA_pct <- percent(NA_count/(nrow(input_orig)*(ncol(input_orig))))
NA_count
NA_pct

# replace all NA with 999
input_orig[is.na(input_orig)] <- 999
# gather cols into nested (multi-level) format. Tall table has id, item,
# response cols.
input_gathered <- input_orig %>%
  gather('item','response',-id) %>% 
  group_by(!!sym(id)) %>% 
  arrange(!!sym(id)) %>% 
  mutate(item = as.factor(str_sub(item, 2, 4)))

# write BLIMP input file
write_csv(input_gathered,
          here(paste0(file_name, '-BLIMP-input.csv')),
          col_names = F
)


# CODE BELOW IS RUN AFTER BLIMP IMPUTATION --------------------------------

# Reformat imputed data set for downstream analysis 
temp1 <- suppressMessages(
  read_csv(
    (here('GOAL-EXAMPLE/GOAL-impute1.csv')), col_names = F)[1:3])
names(temp1) <- c('id', 'item', 'response')
temp2 <- temp1 %>% 
  spread(item, response) 
names(temp2) <- names_input_orig

# Count NA across all columns
NA_count <- sum(temp2 == 999)
NA_count

# Write output
write_csv(temp2, here(
  paste0(
    'OUTPUT-FILES/',
    file_name,
    '-noMiss.csv'
  )
))

