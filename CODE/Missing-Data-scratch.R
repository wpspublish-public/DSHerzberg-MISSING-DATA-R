suppressMessages(library(here))
suppressMessages(library(tidyverse))

id <- c('id')
first_item <- c('hom_i001')
last_item <- c('hom_i114')
file_name <- c('SPM-P-data')
scale_assign <- list(1:14, 15:30, 31:42, 43:65, 66:73, 74:87, 88:103, 104:114)
scale_num <- 1:8

# SPM-P
# prep data file for BLIMP
input_orig <- suppressMessages(read_csv(here(
  paste0('INPUT-FILES/', file_name, '.csv')
))) %>% 
  select(id, first_item:last_item)
# Save column names for downstream
names_input <- names(input_orig)
# Count NA accross all columns
NA_count <- input_orig %>% summarise_all(list(~sum(is.na(.)))) %>%
  mutate(
    all_NA = rowSums(.[2:115]))
# replace all NA with 999
input_orig[is.na(input_orig)] <- 999
# gather cols
input_gathered <- input_orig %>%
  gather('item','response',-id) %>% 
  group_by(id) %>% 
  arrange(id) %>% 
  mutate(
    item = as.integer(str_sub(item, 6, 8)),
    scale = as.factor(
      case_when(
        between(item, 1, 14) ~ 1,
        between(item, 15, 30) ~ 2,
        between(item, 31, 42) ~ 3,
        between(item, 43, 65) ~ 4,
        between(item, 66, 73) ~ 5,
        between(item, 74, 87) ~ 6,
        between(item, 88, 103) ~ 7,
        between(item, 104, 114) ~ 8,
        TRUE ~ NA_real_
      )
    # ) %>%
    #   mutate(
    #     scale_last = case_when(
    #       as.integer(scale) != lead(as.integer(scale)) | is.na(lead(as.integer(scale))) ~ 1,
    #       TRUE ~ NA_real_
    #       )
      )
  )
# strip column names from df, and write outfile
write_csv(input_gathered, here(paste0('OUTPUT-FILES/', file_name, '-gathered-BLIMP.csv')), col_names = F)

input_gathered$item <- as.factor(input_gathered$item)

input_gathered <- input_gathered %>% mutate(
  scale_last = case_when(
    # as.integer(scale) != lead(as.integer(scale)) | is.na(lead(as.integer(scale))) ~ 1,
    scale != lead(scale) | is.na(lead(scale)) ~ 1,
    TRUE ~ NA_real_
  )
)


dum <- input_gathered %>% 
  recipe(~ .) %>% 
  step_dummy(item, one_hot = T) %>% 
  prep(training = input_gathered) %>%
  bake(new_data = input_gathered) %>% 
  mutate_at(vars(starts_with("item")), ~replace(., scale_last == 1, 0))
