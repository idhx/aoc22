library(tidyverse)

input <- readLines("day5/input.txt")

# Separate stacks from moves
delim <- which(input == "")
stacks <- input[1:(delim - 2)]
stacks_names <- input[delim - 1]
moves <- input[(delim + 1):length(input)]

## Transform stacks into list

vec2 <- c()

# populate vector
for (i in stacks) {
  want_chars <- seq(2, nchar(i), by = 4)
  vec <- unlist(str_split(i,""))
  vec <- vec[want_chars]
  while (length(vec) < 9) {
    vec <- append(vec, " ")
  }
  vec2 <- c(vec2, vec)
}

# transform into list
m <- matrix(vec2, nrow = 9)
stacks2 <- as.list(data.frame(t(m))) # this is so ugly...
stacks2 <- lapply(stacks2, rev)

# remove whitespaces
stacks2 <- lapply(stacks2, function(x) {x[ x != " "]})
stacks2 <- unname(stacks2)
