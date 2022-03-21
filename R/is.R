isString <- function(x){
  is.atomic(x) && is.character(x) && length(x) == 1L && !is.na(x)
}

isNumericVector <- function(x){
  is.atomic(x) && is.numeric(x) && !any(is.na(x))
}