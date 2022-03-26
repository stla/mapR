#' @title Unwraps a 'Just' value
#' @description Unwraps the content of a 'Just' \code{maybe} value.
#'
#' @param x a \code{maybe} value (defined in the \strong{maybe} package)
#' 
#' @return If \code{x} is a 'Just' \code{maybe} value, then the function 
#'   unwraps the value. Otherwise, an error is thrown.
#'
#' @importFrom maybe is_just
#' @export
#' @examples 
#' library(maybe)
#' from_just(just(9))
from_just <- function(x){
  if(is_just(x)){
    x[["content"]]
  }else{
    stop("`x` is not a 'Just' value.")
  }
}