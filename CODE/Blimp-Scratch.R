suppressMessages(library(here))
suppressMessages(library(tidyverse))

# SPM-P
# prep data file for BLIMP
spm_p <- read_csv(here('WPS-FILES/SPM-P-data.csv')) %>% 
  select(id, hom_i001:hom_i114) 
  # Count NA accross all columns
NA_count <- spm_p %>% summarise_all(list(~sum(is.na(.)))) %>%
  mutate(
    all_NA = rowSums(.[2:115]))
# replace all NA with 999
spm_p[is.na(spm_p)] <- 999
# gather cols
spm_gathered <- spm_p %>%
  gather('item','response',-id) %>% 
  group_by(id) %>% 
  arrange(id) %>% 
  mutate(
    item = as.integer(str_sub(item, 6, 8)),
    scale = as.integer(
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
spm_head <- head(spm_gathered, 100L)
# strip column names from df
write_csv(spm_p, here('WPS-FILES/SPM-P-BEST-SOLUTION/SPM-P-data-BLIMP.csv'), col_names = F)
write_csv(spm_gathered, here('WPS-FILES/SPM-P-BEST-SOLUTION/SPM-P-data-gathered-BLIMP.csv'), col_names = F)
# keep col names
write_csv(spm_head, here('WPS-FILES/SPM-P-BEST-SOLUTION/SPM-head.csv'))




# GOAL
# prep data file for BLIMP
goal <- read_csv(here('Examples/10. Multiple Imputation/GOAL-data.csv')) %>% 
  select(ID, gmet_41:gmet_54) %>% 
  head(100)
# replace all NA with 999
goal[is.na(goal)] <- 999
# strip column names from df
write_csv(goal, here('Examples/10. Multiple Imputation/GOAL-data-BLIMP.csv'), col_names = F)

spm_p_noMiss_tall <- suppressMessages(
  read_csv(
    (here('model4imp1.csv')), 
  col_names = c('ID', 'item', 'response'))[1:3])

spm_p_noMiss_tall <- suppressMessages(read_csv(here('model4imp1.csv'))[1:3]) 

test <- spm_p %>% filter(id == 28013)
test_noMiss <- spm_p_noMiss %>% filter(id == 28013)
all.equal(test, test_noMiss)


data <- matrix(
  c(rep(101, 10),rep(102, 10), rep(1:10, 2), sample(1:4, 20, replace = T)),
  nrow= 20,
  ncol= 3
  ) 

 
df <- tibble(
  person = rep(101:102, each = 10),
  item = as.factor(rep(1:10, 2)),
  response = sample(1:4, 20, replace = T),
  scale = as.factor(rep(rep(1:2, each = 5), 2))
) %>% mutate(
  scale_last = case_when(
    as.integer(scale) != lead(as.integer(scale)) | is.na(lead(as.integer(scale))) ~ 1,
    TRUE ~ NA_real_
  )
)

dum <- df %>% 
  recipe(~ .) %>% 
  step_dummy(item, one_hot = T) %>% 
  prep(training = df) %>%
  bake(new_data = df)
dum

recode <- function(x) {x = 0}
test <- dum %>% mutate_all(recode)
  

dummies <- matrix(0, nrow = 20, ncol = 10)

for(i in 1:4){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 6:9){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}

supressMessages(library(recipes))
library(tibble)

tib <- as.tibble(list(record = c(1:10), 
                      gender = as.factor(sample(c("M", "F"), 10, replace = TRUE)), 
                      like_product = as.factor(sample(1:5, 10, replace = TRUE))))
dum <- tib %>% 
  recipe(~ .) %>% 
  step_dummy(gender, like_product) %>% 
  prep(training = tib) %>% 
  bake(new_data = tib)

dum
