#' @useDynLib mapR, .registration=TRUE
#' @importFrom Rcpp evalCpp  
NULL

Rcpp::loadModule("maprModule", what = "MAPR")
Rcpp::loadModule("maprptrModule", what = "MAPRPTR")

#' @title R6 class representing a map
#'
#' @description A map is given by keys and values.
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom methods new
mapR <- R6Class(
  
  "mapR",
  
  private = list(
    .map = NULL,
    .items = NULL
  ),
  
  # active = list(
  #   center = function(value) {
  #     if (missing(value)) {
  #       private[[".center"]]
  #     } else {
  #       center <- as.vector(value)
  #       stopifnot(
  #         is.numeric(center),
  #         length(center) == 2L,
  #         !any(is.na(center))
  #       )
  #       private[[".center"]] <- center
  #     }
  #   }
  # ),
  
  public = list(
    
    #' @description Creates a new \code{mapR} object.
    #'
    #' @param keys keys
    #' @param values v
    #'
    #' @return A \code{mapR} object.
    #'
    #' @examples
    #' map <- mapR$new(keys = c("a", "b"), values = list(c(1,2), c(3,4,5)))
    initialize = function(keys, values) {
      keys <- as.character(keys)
      if(any(is.na(keys))){
        stop("Keys cannot contain missing values.")
      }
      stopifnot(
        is.list(values),
        length(keys) == length(values)
      )
      modes <- vapply(values, mode, character(1L))
      if(any(modes != "numeric")){
        stop("The values must be given as a list of numeric vectors.")
      }
      ptr <- new(MAPR, keys, values)$mapPointer()
      private[[".map"]] <- new(MAPRPTR, ptr)
      private[[".items"]] <- data.frame(keys = keys, values = I(values))
    },
    
    #' @description Show instance of a \code{mapR} object.
    #' @param ... ignored
    print = function(...) {
      cat("Circle:\n")
      # cat(" center: ", toString(private[[".center"]]), "\n", sep = "")
      # cat(" radius: ", toString(private[[".radius"]]), "\n", sep = "")
    },
    
    #' @description Get all keys.
    #'
    #' @return The keys.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1,2), c(3,4,5))
    #' )
    #' map$keys()
    keys = function(){
      private[[".map"]]$keys()
    },
    
    #' @description Returns the value corresponding to the given key
    #'
    #' @param key a key
    #'
    #' @return a value
    #'
    #' @examples
    #' map <- mapR$new(keys = c("a", "b"), values = list(c(1,2), c(3,4,5)))
    #' map$at("b")
    at = function(key){
      stopifnot(isString(key))
      private[[".map"]]$at(key)
    },
    
    #' @description Insert a new key-value pair.
    #'
    #' @param key a key
    #' @param value a value
    #'
    #' @return Nothing
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1,2), c(3,4,5))
    #' )
    #' map$insert("c", c(6, 7))
    #' map$keys()
    insert = function(key, value){
      stopifnot(isString(key))
      stopifnot(isNumericVector(value))
      private[[".map"]]$insert(key, value)
    },
    
    #' @description Erase an entry.
    #'
    #' @param key a key
    #'
    #' @return Nothing
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1,2), c(3,4,5))
    #' )
    #' map$erase("a")
    #' map$keys()
    erase = function(key){
      stopifnot(isString(key))
      private[[".map"]]$erase(key)
    }
    
  )
)
