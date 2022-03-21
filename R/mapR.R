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
    .ptr = NULL,
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
    #' map <- mapR$new(keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5)))
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
      private[[".ptr"]] <- ptr
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

    #' @description Size of the map.
    #'
    #' @return An integer, the number of entries.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$size()
    size = function(){
      private[[".map"]]$size()
    },
    
    #' @description Get all keys.
    #'
    #' @return The keys.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$keys()
    keys = function(){
      private[[".map"]]$keys()
    },

    #' @description Get all values.
    #'
    #' @return The values.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$values()
    values = function(){
      private[[".map"]]$values()
    },

    #' @description Get all entries.
    #'
    #' @return The entries in a dataframe.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$items()
    items = function(){
      keys <- self$keys()
      values <- self$values()
      data.frame(key = keys, value = I(values))
    },
    

    #' @description Converts the map to a list.
    #'
    #' @return A named list.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$toList()
    toList = function(){
      keys <- self$keys()
      values <- self$values()
      names(values) <- keys
      values
    },
    
    #' @description Returns the value corresponding to the given key
    #'
    #' @param key a key
    #' @param stop_if_not_found a Boolean value, whether to stopo if the key 
    #'   is not found, or to return \code{NaN}
    #'
    #' @return a value
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$at("b")
    #' map$at("x", stop_if_not_found = FALSE)
    at = function(key, stop_if_not_found = TRUE){
      stopifnot(isString(key))
      if(stop_if_not_found){
        private[[".map"]]$at(key)
      }else{
        tryCatch({
          private[[".map"]]$at(key)
        }, error = function(e){
          NaN
        })
      }
    },

    #' @description Checks if a key exists.
    #'
    #' @param key a string
    #'
    #' @return A Boolean value.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$has_key("b")
    #' map$has_key("x")
    has_key = function(key){
      stopifnot(isString(key))
      private[[".map"]]$has_key(key)
    },

    #' @description Returns the n-th entry.
    #'
    #' @param n a positive integer
    #' @param stop_if_too_large a Boolean value, whether to stop if \code{n}
    #'   is too large, or to return \code{NaN}
    #'
    #' @return A list with the key and the value at index \code{n}.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$nth(2)
    nth = function(n, stop_if_too_large = TRUE){
      stopifnot(isPositiveInteger(n))
      if(stop_if_too_large){
        private[[".map"]]$nth(as.integer(n) - 1L)
      }else{
        tryCatch({
          private[[".map"]]$nth(as.integer(n) - 1L)
        }, error = function(e){
          NaN
        })
      }
    },
    
    #' @description Insert a new entry.
    #'
    #' @param key a key
    #' @param value a value
    #'
    #' @return Nothing
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
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
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$erase("a")
    #' map$keys()
    erase = function(key){
      stopifnot(isString(key))
      private[[".map"]]$erase(key)
    },

    #' @description Merge with another map.
    #'
    #' @param map a \code{mapR} object
    #'
    #' @return Nothing.
    #'
    #' @examples
    #' map1 <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map2 <- mapR$new(
    #'   keys = c("c", "d"), values = list(c(9, 8), c(7, 6))
    #' )
    #' map1$merge(map2)
    #' map1$items()
    merge = function(map){
      stopifnot(inherits(map, "mapR"))
      map2 <- map[[".__enclos_env__"]][["private"]][[".ptr"]]
      private[[".map"]]$merge(map2)
    }
    
  )
)
