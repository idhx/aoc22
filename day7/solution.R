library(tidyverse)
input <- readLines("./day7/input.txt")

# required vars
path <- c()           # current path
all_dirs_sizes <- c("/" = 0) # list of sizes


# NOTE: potential edge case if there are two directories
# with the same name on the system

# function to parse input
parse <- function(x, path, all_dirs_sizes) {
  # change to parentdir
  if (grepl("^\\$ cd \\.\\.", x)) {
    path <- path[-length(path)]
    # change to subdir
  } else if (grepl("^\\$ cd ", x)) {
    dir_to_add <- sub("^\\$ cd ", "", x)
    dir_to_add <- paste(c(path[length(path)], dir_to_add), collapse="-")
    path <- c(path, dir_to_add)
  # add if first time dir listed
  } else if (grepl("^dir", x)) {
    dir_to_add <- paste(c(path[length(path)], sub("^dir ", "", x)), collapse="-")
    if (!dir_to_add %in% names(all_dirs_sizes)) {
      all_dirs_sizes[dir_to_add] <- 0
    } else {
      stop('dir was already present in all_dirs_sizes')
    }
    # add size of file
  } else if (grepl("^\\d", x)) {
    size <- as.numeric(sub(" .*$", "", x))
    all_dirs_sizes[path] <- all_dirs_sizes[path] + size
  }
  return(
    list(
      path = path,
      all_dirs_sizes = all_dirs_sizes
    )
  )
}

for (i in input) {
  step <- parse(i, path, all_dirs_sizes)
  path <- step$path
  all_dirs_sizes <- step$all_dirs_sizes
}

# answer
all_dirs_sizes[all_dirs_sizes <= 100000] %>% sum() # 1243729
