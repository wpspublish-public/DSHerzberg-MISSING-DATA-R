# required packages
suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(fastDummies))

# input parameters. Input file is .csv, with person ID column on far-left, and
# remaining columns hold items, numbered in i001, i002, i003 etc. format. Input
# can have any number of subscales/factors, input via `scale_num` vector.
# Item-to-scale assignments input in the `scale_assign` vector, which is a list
# of numerical ranges containing the items assigned to each subscale/facttor.
id <- c('id')
first_item <- c('i001')
last_item <- c('i114')
file_name <- c('SPM-P-data-i-num')
scale_assign <- list(1:14, 15:30, 31:42, 43:65, 66:73, 74:87, 88:103, 104:114)
scale_num <- 1:8
names(scale_assign) <- scale_num

# read input
input_orig <- suppressMessages(read_csv(here(
  paste0('INPUT-FILES/', file_name, '.csv')
))) %>% 
  select(id, first_item:last_item)
names_input_orig <- names(input_orig)
# Count NA accross all columns
NA_count <- input_orig %>% summarise_all(list(~sum(is.na(.)))) %>%
  mutate(
    all_NA = rowSums(.[2:ncol(.)]))
NA_count$all_NA
# replace all NA with 999
input_orig[is.na(input_orig)] <- 999
# gather cols into nested (multi-level) format. Tall table has id, item,
# response cols.
input_gathered <- input_orig %>%
  gather('item','response',-id) %>% 
  group_by(id) %>% 
  arrange(id) %>% 
  mutate(item = as.integer(str_sub(item, 2, 4)))

# add two more columns: scale = each items scale assignment, scale_last = labels
# row that holds last item on each scale.
input_scale <- enframe(scale_assign) %>%
  unnest(item = value) %>%
  rename (scale = name) %>%
  right_join(input_gathered, by = "item") %>%
  select(id, item, response, scale) %>%
  mutate(scale_last = case_when(scale != lead(scale) |
                                  is.na(lead(scale)) ~ 1,
                                TRUE ~ NA_real_)) %>%
  mutate_at(vars(item), ~ as.factor(.))

# add columns holding dummy codes for items, each item is dummy coded relative
# to last item in its scale.
dum <- input_scale %>% 
  dummy_cols(select_columns = 'item') %>% 
  mutate_at(vars(starts_with("item_")), ~replace(., scale_last == 1, 0)) %>% 
  select(-scale_last)

# write BLIMP input file
write_csv(dum,
          here(paste0(file_name, '-BLIMP-input.csv')),
          col_names = F
)


# CODE BELOW IS RUN AFTER BLIMP IMPUTATION --------------------------------

# Reformat imputed data set for downstream analysis 
temp1 <- suppressMessages(
  read_csv(
    (here('model4imp1.csv')), col_names = F)[1:3])
names(temp1) <- c('id', 'item', 'response')
temp2 <- temp1 %>% 
  spread(item, response) 
names(temp2) <- names_input_orig

# Count NA across all columns
NA_count_noMiss <- temp2 %>% summarise_all(list(~sum(. == 999))) %>%
  mutate(
    all_NA = rowSums(.[2:ncol(.)]))
NA_count_noMiss$all_NA

# Write output
write_csv(temp2, here(
  paste0(
    'OUTPUT-FILES/',
    file_name,
    '-noMiss.csv'
  )
))

