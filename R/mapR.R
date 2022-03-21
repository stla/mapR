#' @useDynLib mapR, .registration=TRUE
#' @importFrom Rcpp evalCpp  
#' @importFrom methods new
NULL

Rcpp::loadModule("maprModule", what = "MAPR")
Rcpp::loadModule("maprptrModule", what = "MAPRPTR")


test <- function(keys, values){
  new(MAPR, keys, values)$mapPointer()
}

mapat <- function(map, key){
  new(MAPRPTR, map)$at(key)
}