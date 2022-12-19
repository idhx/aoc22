library(tidyverse)

d <- as_tibble(read.table("day2/input.txt"))
names(d) <- c("m1", "m2")

# add var for score
d$score <- rep(0, length(d$m1))

# transform second move to same encoding as first
d <- d %>%
  mutate(
    m2 = case_when(
      m2 == "X" ~ "A",
      m2 == "Y" ~ "B",
      m2 == "Z" ~ "C",
    )
  )

# add points for item played
d <- d %>%
  mutate(
    score = case_when(
      m2 == "A" ~ d$score + 1,
      m2 == "B" ~ d$score + 2,
      m2 == "C" ~ d$score + 3
    )
  )


# add points for outcome
d <- d %>%
  mutate(
    score = case_when(
      m1 == m2 ~ d$score + 3,
      m2 == "A" & m1 == "C" ~ d$score + 6,
      m2 == "B" & m1 == "A" ~ d$score + 6,
      m2 == "C" & m1 == "B" ~ d$score + 6,
      TRUE ~ d$score
    )
  )

sum(d$score) # 12535
