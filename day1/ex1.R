input <- as.numeric(paste(readLines("day1/input.txt")))

## first star

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

## second star

sorted <- sort(list, decreasing = TRUE)
sum(sorted[1:3]) #195625
