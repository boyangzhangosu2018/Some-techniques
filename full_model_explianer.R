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
