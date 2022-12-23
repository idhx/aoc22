input <- readLines("day6/input.txt")
input <- unlist(strsplit(input, ""))


## part 1
## There is probably a more efficient way of doing this
sig_found <- FALSE
pos <- 4

while (!sig_found) {
  window <- seq((pos - 3), pos)
  chars <- input[window]
  if (length(unique(chars)) == length(chars)){
    sig_found <- TRUE
  } else{
  pos <- pos + 1
  }
}

# answer
pos

## part 2
## There is probably a more efficient way of doing this
sig_found <- FALSE
pos <- 14

while (!sig_found) {
  window <- seq((pos - 13), pos)
  chars <- input[window]
  if (length(unique(chars)) == length(chars)){
    sig_found <- TRUE
  } else{
  pos <- pos + 1
  }
}

# answer
pos
