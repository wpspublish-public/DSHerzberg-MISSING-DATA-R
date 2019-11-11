suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(recipes))

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
    all_NA = rowSums(.[2:ncol(.)]))
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
      )
    )

# coerce item to factor.
input_gathered$item <- as.factor(input_gathered$item)

input_gathered <- input_gathered %>% mutate(
  scale_last = case_when(
    scale != lead(scale) | is.na(lead(scale)) ~ 1,
    TRUE ~ NA_real_
  )
)


dum <- input_gathered %>% 
  recipe(~ .) %>% 
  step_dummy(item, one_hot = T, preserve = T) %>% 
  prep(training = input_gathered) %>%
  bake(new_data = input_gathered) %>% 
  mutate_at(vars(starts_with("item_")), ~replace(., scale_last == 1, 0)) %>% 
  select(-scale_last)

write_csv(dum, here('test.csv'), col_names = F)

test <- input_gathered %>% map2(
  scale_assign,
  scale_num,
  ~ mutate(scale = case_when(
    item %in% scale_assign ~ scale_num
    TRUE ~ NA_real_
  )
  )
)
    
df <- tibble(
  person = rep(101:103, each = 12),
  item = rep(1:12, 3),
  response = sample(1:4, 36, replace = T),
  scale = rep(1:4, each = 3, 3)
)

scale_assign <- list(1:3, 4:6, 7:9, 10:12)
scale_num <- 1:4

print(df, n = Inf)

# A tibble: 36 x 4
# person  item response scale
# <int> <int>    <int> <int>
#   1    101     1        4     1
# 2    101     2        2     1
# 3    101     3        4     1
# 4    101     4        4     2
# 5    101     5        4     2
# 6    101     6        4     2
# 7    101     7        3     3
# 8    101     8        2     3
# 9    101     9        4     3
# 10    101    10        1     4
# 11    101    11        1     4
# 12    101    12        4     4
# 13    102     1        1     1
# 14    102     2        3     1
# 15    102     3        1     1
# 16    102     4        1     2
# 17    102     5        3     2
# 18    102     6        3     2
# 19    102     7        4     3
# 20    102     8        1     3
# 21    102     9        3     3
# 22    102    10        4     4
# 23    102    11        3     4
# 24    102    12        3     4
# 25    103     1        4     1
# 26    103     2        1     1
# 27    103     3        2     1
# 28    103     4        2     2
# 29    103     5        4     2
# 30    103     6        1     2
# 31    103     7        4     3
# 32    103     8        4     3
# 33    103     9        1     3
# 34    103    10        4     4
# 35    103    11        1     4
# 36    103    12        2     4
