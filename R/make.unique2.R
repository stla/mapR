dotify <- function(x, avoid){
  l <- length(x)
  if(l == 1L && !x %in% avoid){
    return(x)
  }
  numbers <- 1L:l
  out <- paste0(x, ".", numbers)
  ndots <- 1L
  while(any(out %in% avoid)){
    ndots <- ndots + 1L
    out <- paste0(x, paste0(rep(".", ndots), collapse = ""), numbers)
  }
  out
}

make.unique2 <- function(x){
  if(anyDuplicated(x)){
    splt <- split(x, x)
    u <- names(splt)
    for(i in 1L:length(splt)){
      splt_i <- splt[[i]]
      j <- match(splt_i[1L], u)
      avoid <- u[-j]
      splt_i_new <- dotify(splt_i, avoid)
      u <- c(avoid, splt_i_new)
      splt[[i]] <- splt_i_new
    }
    x <- unsplit(splt, x)
  }
  x
}