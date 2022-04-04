#' @useDynLib mapR, .registration=TRUE
#' @importFrom Rcpp evalCpp setRcppClass
NULL

uMAPR <- setRcppClass("uMAPR")

oMAPR <- setRcppClass("oMAPR")

ErrorOrObject <- setRcppClass("ErrorOrObject")

# EitherFunc <- setRcppClass("EitherFunc")