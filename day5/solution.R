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
stacks3 <- stacks2 # copy for second exercise. should write a function, but eh

# transform input
moves <- moves%>%
  str_remove("^move ") %>%
  str_remove("from ") %>%
  str_remove("to ") %>%
  str_split(" ")

# function that acutally moves crates
do_moves <- function(state, vec) {
  n <- vec[1]
  from <- vec[2]
  to <- vec <- vec[3]
  from_stack <- state[[from]]
  to_stack <- state[[to]]
  crates <- from_stack[(length(from_stack) - n + 1):length(from_stack)]
  crates <- rev(crates)
  from_stack <- from_stack[-((length(from_stack) - n + 1):length(from_stack))]
  to_stack <- c(to_stack, crates)
  state[[from]] <- from_stack
  state[[to]] <- to_stack
  return(state)
}

# loop over input
for (i in moves[1:length(moves)]) {
  i <- as.numeric(i)
  stacks2 <- do_moves(stacks2, i)
}

# final crates at top
paste((unlist(lapply(stacks2, function(x) {tail(x, 1)}))), collapse = "")

## second part
do_moves2 <- function(state, vec) {
  n <- vec[1]
  from <- vec[2]
  to <- vec <- vec[3]
  from_stack <- state[[from]]
  to_stack <- state[[to]]
  crates <- from_stack[(length(from_stack) - n + 1):length(from_stack)]
  from_stack <- from_stack[-((length(from_stack) - n + 1):length(from_stack))]
  to_stack <- c(to_stack, crates)
  state[[from]] <- from_stack
  state[[to]] <- to_stack
  return(state)
}

# loop over input
for (i in moves[1:length(moves)]) {
  i <- as.numeric(i)
  stacks3 <- do_moves2(stacks3, i)
}

# final crates at top
paste((unlist(lapply(stacks3, function(x) {tail(x, 1)}))), collapse = "")
