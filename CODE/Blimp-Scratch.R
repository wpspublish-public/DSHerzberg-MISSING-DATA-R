suppressMessages(library(here))
suppressMessages(library(tidyverse))

outboard_1 <- read_csv(here(
  "OUTPUT-DEDICATED-BLIMP-PROCESSOR/GOAL2-impute1.csv"
))

MBP_1 <- read_csv(here(
  "GOAL-EXAMPLE/GOAL-impute1.csv"
))
identical(outboard_1, MBP_1)
all.equal(outboard_1, MBP_1)

sum(outboard_1 == 999)
sum(MBP_1 == 999)
