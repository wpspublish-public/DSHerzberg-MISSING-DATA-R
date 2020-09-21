suppressMessages(library(here))
suppressMessages(library(tidyverse))

input_orig <- suppressMessages(read_csv(here(
  "ENDERS-REDUCE-PROC-TIME/SCALE-ASSIGN-MODEL/TOD.DATA.3.5.20_forBLIMP7.14.20.csv"
))) 

NA_count <- sum(is.na(input_orig))
NA_count

input_orig[is.na(input_orig)] <- 999

input_long <- input_orig %>%
  pivot_longer(cols = -ID,
               names_to = "item",
               values_to = "response") %>%
  mutate(
    scale = case_when(
      item %in% item[1:50] ~ 1, 
      item %in% item[51:84] ~ 2, 
      item %in% item[85:114] ~ 3, 
      item %in% item[115:185] ~ 4, 
      item %in% item[186:206] ~ 5, 
      item %in% item[207:251] ~ 6, 
      item %in% item[252:293] ~ 7, 
      item %in% item[294:343] ~ 8, 
      item %in% item[344:363] ~ 9, 
      item %in% item[364:388] ~ 10, 
      item %in% item[389:438] ~ 11, 
      item %in% item[439:459] ~ 12, 
      item %in% item[460:502] ~ 13, 
      item %in% item[503:544] ~ 14, 
    ), 
    across(item, ~ str_sub(., 2, 4))) %>% 
  relocate(scale, .after = "ID")

write_csv(input_long,
          here(
            "ENDERS-REDUCE-PROC-TIME/SCALE-ASSIGN-MODEL/TOD.DATA.3.5.20_forBLIMP7.14.20-BLIMP-input.csv"
          ),
          col_names = F
)
