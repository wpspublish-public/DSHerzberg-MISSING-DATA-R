suppressMessages(library(here))
suppressMessages(library(tidyverse))

input_orig <- suppressMessages(read_csv(here(
  "ENDERS-REDUCE-PROC-TIME/TOD.DATA.3.5.20_forBLIMP7.14.20.csv"
))) 

NA_count <- sum(is.na(input_orig))
NA_count


impute_name <- "TOD-impute1"
impute_path <- "ENDERS-REDUCE-PROC-TIME/"

temp1 <- suppressMessages(
  read_csv(
    here(
      str_c(impute_path, impute_name, ".csv")
    ),
    col_names = F))
names(temp1) <- c("id", "item", "response")
temp2 <- temp1 %>% 
  pivot_wider(
    id_cols = id,
    names_from = item,
    values_from = response
  )
names(temp2) <- names(input_orig)

NA_count <- sum(temp2 == 999)
NA_count
