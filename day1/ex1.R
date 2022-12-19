input <- as.numeric(paste(readLines("day1/input.txt")))

# initalise vectors
elf <- c()
list <- c()

for (i in input) {
  if (!is.na(i)) {
    elf <- c(elf, i) # add i to this elf's bag
  }
  else {
    list <- c(list, sum(elf)) # add total for this elf to list
    elf <- c() # and move to next elf
  }
}

max(list) # 65912
