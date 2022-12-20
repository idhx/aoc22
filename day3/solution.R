library(tidyverse)
input <- as_tibble(read_lines("day3/input.txt"))

## part 1

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

## part two
d$value[1:3]

badges <- c()
for (i in seq(1, 298, by = 3)) {
  elves <- d$value[i:(i+2)]
  split <- str_split(elves, "")
  badges <- c(badges, Reduce(intersect, split) %>% print())
}

d2 <- tibble(badges) %>%
  rowwise() %>%
  mutate(
    priority = which(let == badges)
  )

sum(d2$priority) #2413
