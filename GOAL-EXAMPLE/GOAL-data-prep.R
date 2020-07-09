suppressMessages(library(here))
suppressMessages(library(tidyverse))

file_name <- c("GOAL-data-i-num")

input_orig <- suppressMessages(read_csv(here(
  str_c("INPUT-FILES/", file_name, ".csv")
))) 

NA_count <- sum(is.na(input_orig))
NA_count

input_orig[is.na(input_orig)] <- 999

input_tall <- input_orig %>%
  pivot_longer(cols = -id,
               names_to = "item",
               values_to = "response") %>%
  mutate(across(c(item), ~ str_sub(item, 2, 4)))

write_csv(input_tall,
          here(
            str_c(file_name, '-BLIMP-input.csv')
            ),
          col_names = F
)

impute_name <- "GOAL-impute1"

temp1 <- suppressMessages(
  read_csv(
    here(
    str_c("GOAL-EXAMPLE/", impute_name, ".csv")
  ),
    col_names = F))
names(temp1) <- c("id", "item", "response")
temp2 <- temp1 %>% 
  pivot_wider(
    id_cols = id,
    names_from = c(item),
    values_from = response
  )
names(temp2) <- names(input_orig)

NA_count <- sum(temp2 == 999)
NA_count

write_csv(temp2, here(
  str_c(
    'OUTPUT-FILES/',
    file_name,
    '-noMiss.csv'
  )
))

