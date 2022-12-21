library(tidyverse)

input <- as_tibble(read.delim("day4/input.txt", header = FALSE, sep = ","))
names(input) <- c("e1", "e2")

d <- input %>%
  # process input into start and end for both ranges
  mutate(
    e1_st = str_remove(e1, "-.*$"),
    e1_en = str_remove(e1, "^.*-"),
    e2_st = str_remove(e2, "-.*$"),
    e2_en = str_remove(e2, "^.*-"),
  ) %>%
  select(!e1 & !e2)

# function to check whether either range is contained in the other
test <- function(x) {
  seq1 <- seq(x[1], x[2])
  seq2 <- seq(x[3], x[4])
  int <- intersect(seq1, seq2)
  if (!anyNA(match(seq1, int)) || !anyNA(match(seq2, int))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

d$full_overlap <- apply(X = d, MARGIN = 1, FUN = test)

sum(d$full_overlap) # 498
