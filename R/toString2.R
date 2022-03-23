#' @importFrom R6 is.R6 is.R6Class
#' @noRd
class2 <- function(x){
  if(is.atomic(x)){
    type <- typeof(x)
    ldim <- length(dim(x))
    if(ldim == 0L){
      if(is.null(x)){
        "NULL"
      }else{
        out <- if(is.factor(x)){
          "factor"
        }else{
          if(!inherits(x, type)){ # e.g. gmp::bigq, jsonlite::json 
            type <- class(x)[1L]
          }
          if(length(x) >= 2L){
            paste0(type, " vector")
          }else{
            type
          }
        }
        attr(out, "toString") <- TRUE
        out
      }
    }else{
      if(ldim == 2L){
        cls <- "matrix"
      }else{
        cls <- "array"
      }
      if(!inherits(x, cls)){ # e.g. bigq matrix
        type <- class(x)[1L]
      }
      paste0(type, " ", cls)
    }
  }else{
    if(is.function(x)){
      "function"
    }else if(is.data.frame(x)){
      cls <- class(x)[1L]
      if(cls == "data.frame"){
        "data.frame"
      }else{
        sprintf("data.frame (%s)", cls)
      }
    }else if(is.list(x)){
      if(is.pairlist(x)){
        "pairlist"
      }else{
        "list"
      }
    }else if(isS4(x)){
      sprintf("S4 (%s)", class(x)[1L])
    }else if(is.R6Class(x)){
      "R6 class generator"
    }else if(is.R6(x)){
      cls <- class(x)[1L]
      if(cls != "R6"){
        sprintf("R6 object (%s)", cls)
      }else{
        "R6 object"
      }
    }else{
      typeof(x)
    }
  }
}

toString2 <- function(x){
  cls <- class2(x)
  if(isTRUE(attr(cls, "toString"))){
    paste0(cls, ": ", toString(x, width = 40L))
  }else{
    cls
  }
}
