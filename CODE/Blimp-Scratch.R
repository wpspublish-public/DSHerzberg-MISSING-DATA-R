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


