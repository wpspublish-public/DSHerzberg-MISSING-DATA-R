suppressMessages(library(tidyverse))
suppressMessages(library(recipes))

id <- c('id')
first_item <- c('i001')
last_item <- c('i114')
scale_assign <- list(1:14, 15:30, 31:42, 43:65, 66:73, 74:87, 88:103, 104:114)
scale_num <- 1:8
names(scale_assign) <- scale_num

input_orig <- suppressMessages(read_csv(
  '~/desktop/SPM-P-data-i-num.csv'
)) %>% 
  select(id, first_item:last_item)

NA_count <- input_orig %>% summarise_all(list(~sum(is.na(.)))) %>%
  mutate(
    all_NA = rowSums(.[2:ncol(.)]))

input_orig[is.na(input_orig)] <- 999

input_gathered <- input_orig %>%
  gather('item','response',-id) %>% 
  group_by(id) %>% 
  arrange(id) %>% 
  mutate(item = as.integer(str_sub(item, 2, 4)))

input_scale <- enframe(scale_assign) %>%
  unnest(item = value) %>%
  rename (scale = name) %>%
  mutate_all(as.integer) %>%
  right_join(input_gathered, by = "item") %>%
  select(id, item, response, scale) %>%
  mutate(scale_last = case_when(scale != lead(scale) |
                                  is.na(lead(scale)) ~ 1,
                                TRUE ~ NA_real_)) %>%
  mutate_at(vars(item), ~ as.factor(.))

dum <- input_scale %>% 
  recipe(~ .) %>% 
  step_dummy(item, one_hot = T, preserve = T) %>% 
  prep(training = input_scale) %>%
  bake(new_data = input_scale) %>% 
  mutate_at(vars(starts_with("item_")), ~replace(., scale_last == 1, 0)) %>% 
  select(-scale_last)

write_csv(dum, '~/desktop/SPM-P-data-BLIMP-input.csv',
          col_names = F
)


