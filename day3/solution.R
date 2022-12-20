library(tidyverse)

input <- as_tibble(read_lines("day3/input.txt"))

let <- c(letters, LETTERS)

d <- input %>%
  mutate(
    # split each line in half
    len = nchar(value),
    h1 = substr(value, 1, len / 2),
    h2 = substr(value, len / 2 + 1, len),
  ) %>%
  # find common character
  rowwise() %>%
  mutate(
    common = intersect(str_split(h1,"")[[1]], str_split(h2,"")[[1]])
  ) %>%
  # priority of common character
  mutate(
    priority = which(let == common)
  )

# answer
sum(d$priority) #8394
