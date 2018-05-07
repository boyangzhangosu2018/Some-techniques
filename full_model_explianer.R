## This is a protocol code to automately complete full-model selection with 4 variables in the list.
## Please notice this function doesn't contain lmer function (linear mixed model)
## Author list: Boyang Zhang (zhang.7077@osu.edu), Yu, Zhongtang, Moraes, Luis
le <- 4
p2 <- function (track) {
  if (length(track) == le) {
    
    print(track)
    

  } else {
    track <- c(track, FALSE)
    p2(track)
    length(track) <- length(track) - 1
    track <- c(track, TRUE)
    p2(track)
    length(track) <- length(track) - 1
  }
}
p2(c())
