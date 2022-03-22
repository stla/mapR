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
    .map = NULL
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
    #' @param keys keys, a character vector
    #' @param values values, a list of numeric vectors; \code{keys} and 
    #'   \code{values} must have the same length
    #' @param join Boolean, whether to join the values of duplicated keys
    #' @param checks Boolean, whether to check \code{keys} and \code{values}
    #'
    #' @return A \code{mapR} object.
    #'
    #' @examples
    #' mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' mapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6))
    #' )
    #' mapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6)),
    #'   join = TRUE
    #' )
    initialize = function(keys, values, join = FALSE, checks = TRUE){
      if(checks){
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
      }
      if(join && anyDuplicated(keys)){
        splt <- split(values, keys)
        values <- lapply(splt, function(x) do.call(c, x))
        keys <- names(values)
      }
      ptr <- new(MAPR, keys, values)$ptr#mapPointer()
      private[[".ptr"]] <- ptr
      private[[".map"]] <- new(MAPRPTR, ptr)
    },
    
    #' @description Show instance of a \code{mapR} object.
    #' @param ... ignored
    print = function(...) {
      size <- self$size()
      if(size == 0L){
        cat("empty `mapR`")
      }else{
        keys <- sprintf('"%s"', self$keys())
        values <- vapply(self$values(), toString, character(1L))
        s <- ifelse(size > 1L, "s", "")
        cat(sprintf("`mapR` containing %d item%s:\n\n", size, s))
        lines <- paste0("  ", keys, " -> ", values)
        cat(lines, sep = "\n")
      }
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
    #' @return The keys, a character vector.
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
    #' @return The values, a list of numeric vectors.
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
    
    #' @description Returns the value corresponding to the given key.
    #'
    #' @param key a key (string)
    #' @param stop_if_not_found a Boolean value, whether to stop if the key 
    #'   is not found, or to return \code{NaN}
    #'
    #' @return The value corresponding to the key, a numeric vector.
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
    
    #' @description Returns the index of the given key.
    #'
    #' @param key a key (string)
    #'
    #' @return The index of the key, or \code{NA} if it is not found.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$index("b")
    #' map$index("x")
    index = function(key){
      stopifnot(isString(key))
      i <- private[[".map"]]$index(key)
      if(i == 0L){
        NA_integer_
      }else{
        i
      }
    },
    
    #' @description Extract submap.
    #'
    #' @param keys some keys, a character vector; those which do not belong to 
    #'   the keys of the reference map will be ignored
    #'
    #' @return A \code{mapR} object.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b", "c"), 
    #'   values = list(c(1, 2), c(3, 4, 5), c(6, 7))
    #' )
    #' map$submap(c("a", "c"))
    submap = function(keys){
      stopifnot(isCharacterVector(keys))
      keys <- intersect(keys, self$keys())
      lst <- self$toList()
      mapR$new(keys, lst[keys], checks = FALSE) 
    },
    
    #' @description Checks whether a key exists in a map.
    #'
    #' @param key a key (string)
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
    
    #' @description Returns the n-th entry of the map.
    #'
    #' @param n index, a positive integer
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
    #' map$nth(9, stop_if_too_large = FALSE)
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
    #' @param key a key (string)
    #' @param value a value (numeric vector)
    #' @param replace Boolean, whether to replace the value if the key is 
    #'   already present
    #'
    #' @return Nothing, this updates the map.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$insert("c", c(6, 7))
    #' map
    #' map$insert("a", c(8, 9))
    #' map
    #' map$insert("a", c(8, 9), replace = TRUE)
    #' map
    insert = function(key, value, replace = FALSE){
      stopifnot(isString(key))
      stopifnot(isNumericVector(value))
      if(replace){
        private[[".map"]]$assign(key, value)
      }else{
        private[[".map"]]$insert(key, value)
      }
    },
    
    #' @description Erase some entries.
    #'
    #' @param keys some keys, a character vector
    #'
    #' @return Nothing, this updates the map.
    #'
    #' @examples
    #' map <- mapR$new(
    #'   keys = c("a", "b", "c"), 
    #'   values = list(c(1, 2), c(3, 4, 5), c(6, 7))
    #' )
    #' map$erase("a")
    #' map
    #' map$erase(c("b", "c"))
    #' map
    erase = function(keys){
      stopifnot(isCharacterVector(keys))
      if(length(keys) == 1L){
        private[[".map"]]$erase(keys)
      }else{
        private[[".map"]]$merase(keys)
      }
    },
    
    #' @description Merge with another map.
    #'
    #' @param map a \code{mapR} object
    #' @param join Boolean, whether to join the values if the reference map 
    #'   and \code{map} have some identical keys
    #'
    #' @return Nothing, this updates the reference map.
    #'
    #' @examples
    #' map1 <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map2 <- mapR$new(
    #'   keys = c("c", "d"), values = list(c(9, 8), c(7, 6))
    #' )
    #' map1$merge(map2)
    #' map1
    #' 
    #' # `join` example ####
    #' map1 <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map2 <- mapR$new(
    #'   keys = c("a", "d"), values = list(c(9, 8), c(7, 6))
    #' )
    #' map1$merge(map2, join = FALSE)
    #' map1
    #' 
    #' map1 <- mapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map1$merge(map2, join = TRUE)
    #' map1
    merge = function(map, join = FALSE){
      stopifnot(inherits(map, "mapR"))
      if(join && anyDuplicated(keys <- c(self$keys(), map$keys()))){
        values <- c(self$values(), map$values())
        splt <- split(values, keys)
        values <- lapply(splt, function(x) do.call(c, x))
        keys <- names(values)
        ptr <- new(MAPR, keys, values)$ptr#mapPointer()
        private[[".ptr"]] <- ptr
        private[[".map"]] <- new(MAPRPTR, ptr)
      }else{
        map2 <- map[[".__enclos_env__"]][["private"]][[".ptr"]]
        private[[".map"]]$merge(map2)
      }
    }
    
  )
)
