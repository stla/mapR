# Rcpp::loadModule("maprModule", what = "MAPR")
# Rcpp::loadModule("maprptrModule", what = "MAPRPTR")


#' @title R6 class representing an ordered map
#'
#' @description A map is given by keys and values.
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom methods new
omapR <- R6Class(
  
  "omapR",
  
  lock_class = TRUE,
  
  cloneable = FALSE,
  
  private = list(
    # .ptr = NULL,
    .map = NULL
  ),
  
  public = list(
    
    #' @description Creates a new \code{omapR} object.
    #'
    #' @param keys keys, a character vector without \code{NA} value
    #' @param values values, a list of R objects; \code{keys} and 
    #'   \code{values} must have the same length
    #' @param duplicated the action to perform for duplicated keys, one of 
    #'   \code{"drop"}, \code{"join"}, or \code{"separate"}
    #' @param checks Boolean, whether to check \code{keys} and \code{values}
    #' @param ptr an external pointer; this is for internal use only
    #'
    #' @return An \code{omapR} object.
    #'
    #' @examples
    #' omapR$new(
    #'   keys = c("z", "a"), 
    #'   values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' # examples with duplicated keys:
    #' omapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6))
    #' )
    #' omapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6)),
    #'   duplicated = "join"
    #' )
    #' omapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6)),
    #'   duplicated = "separate"
    #' )
    initialize = function(keys, values, duplicated = "drop", checks = TRUE, ptr = NULL){
      if(!is.null(ptr)){
        OMAPR <- new("oMAPR", ptr)
        private[[".map"]] <- OMAPR
      }else{
        duplicated <- match.arg(duplicated, c("drop", "join", "separate"))
        if(checks){
          keys <- as.character(keys)
          if(any(is.na(keys))){
            stop("Keys cannot contain missing values.")
          }
          stopifnot(
            is.list(values),
            length(keys) == length(values)
          )
        }
        if(duplicated != "drop" && anyDuplicated(keys)){
          if(duplicated == "join"){
            splt <- split(values, keys)
            values <- lapply(splt, function(x){
              if(length(x) == 1L) x[[1L]] else x
            })
            keys <- names(values)
          }else{ # duplicated == "separate"
            keys <- make.unique2(keys)
          }
        }
        OMAPR <- new("oMAPR", keys, values)
        private[[".map"]] <- OMAPR
      }
      invisible(NULL)
    },
    
    #' @description Show instance of a \code{omapR} object.
    #' @param ... ignored
    print = function(...) {
      size <- self$size()
      if(size == 0L){
        cat("empty `omapR` object\n")
      }else{
        keys <- sprintf('"%s"', self$keys())
        values <- vapply(self$values(), toString2, character(1L))
        s <- ifelse(size > 1L, "s", "")
        cat(sprintf("`omapR` object containing %d item%s:\n\n", size, s))
        lines <- paste0("  ", keys, " -> ", values)
        cat(lines, sep = "\n")
      }
    },
    
    #' @description Size of the reference map.
    #'
    #' @return An integer, the number of entries.
    #'
    #' @examples
    #' map <- omapR$new(
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
    #' map <- omapR$new(
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
    #' map <- omapR$new(
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
    #' map <- omapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$items()
    items = function(){
      keys <- self$keys()
      values <- self$values()
      data.frame(key = keys, value = I(values))
    },
    
    #' @description Converts the map to a named list.
    #'
    #' @return A named list.
    #'
    #' @examples
    #' map <- omapR$new(
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
    #' @return The value corresponding to the key, a R object.
    #'
    #' @examples
    #' map <- omapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$at("b")
    #' map$at("x", stop_if_not_found = FALSE)
    at = function(key, stop_if_not_found = TRUE){
      stopifnot(isString(key))
      stopifnot(isBoolean(stop_if_not_found))
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
    #' map <- omapR$new(
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
    
    #' @description Extract a submap from the reference map.
    #'
    #' @param keys some keys, a character vector; those which do not belong to 
    #'   the keys of the reference map will be ignored
    #' @param inplace Boolean, whether to update the reference map or 
    #'   to return a new map
    #' @param bydeleting Boolean, whether to construct the submap by 
    #'   deleting the keys which are not in \code{keys} or by starting 
    #'   from the empty submap and adding the entries 
    #'
    #' @return An \code{omapR} object if \code{inplace=FALSE}, 
    #'   nothing otherwise.
    #'
    #' @examples
    #' map <- omapR$new(
    #'   keys = c("a", "b", "c"), 
    #'   values = list(c(1, 2), c(3, 4, 5), c(6, 7))
    #' )
    #' map_copy <- map$copy()
    #' map$extract(c("a", "c"))
    #' map
    #' map$extract(c("a", "c"), inplace = TRUE)
    #' map
    #' map_copy$extract(c("a", "c"), bydeleting = TRUE)
    #' map_copy
    #' map_copy$extract(c("a", "c"), inplace = TRUE, bydeleting = TRUE)
    #' map_copy
    extract = function(keys, inplace = FALSE, bydeleting = FALSE){
      stopifnot(isCharacterVector(keys))
      stopifnot(isBoolean(inplace))
      stopifnot(isBoolean(bydeleting))
      if(length(keys) == 0L){
        if(inplace){
          private[[".map"]] <- new("oMAPR", character(0L), list())
          invisible(NULL)
        }else{
          omapR$new(character(0L), list(), checks = FALSE)
        }
      }else{
        if(bydeleting){
          if(inplace){
            private[[".map"]]$extract_by_erasing_inplace(keys)
            invisible(NULL)
          }else{
            ptr <- private[[".map"]]$extract_by_erasing(keys)
            omapR$new(ptr = ptr)
          }
        }else{
          if(inplace){
            private[[".map"]]$extract_inplace(keys)
            invisible(NULL)
          }else{
            ptr <- private[[".map"]]$extract(keys)
            omapR$new(ptr = ptr) 
          }
        }
      }
    },
    
    #' @description Checks whether a key exists in the reference map.
    #'
    #' @param key a key (string)
    #'
    #' @return A Boolean value.
    #'
    #' @examples
    #' map <- omapR$new(
    #'   keys = c("a", "b"), 
    #'   values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$has_key("b")
    #' map$has_key("x")
    has_key = function(key){
      stopifnot(isString(key))
      private[[".map"]]$has_key(key)
    },
    
    #' @description Returns the n-th entry of the reference map.
    #'
    #' @param n index, a positive integer
    #' @param stop_if_too_large a Boolean value, whether to stop if \code{n}
    #'   is too large, or to return \code{NaN}
    #'
    #' @return A list with the key and the value at index \code{n}.
    #'
    #' @examples
    #' map <- omapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$nth(2)
    #' map$nth(9, stop_if_too_large = FALSE)
    nth = function(n, stop_if_too_large = TRUE){
      stopifnot(isPositiveInteger(n))
      stopifnot(isBoolean(stop_if_too_large))
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
    
    #' @description Insert a new entry in the reference map.
    #'
    #' @param key a key (string)
    #' @param value a value (R object)
    #' @param replace Boolean, whether to replace the value if the key is 
    #'   already present
    #'
    #' @return Nothing, this updates the map.
    #'
    #' @examples
    #' map <- omapR$new(
    #'   keys = c("a", "b"), 
    #'   values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$insert("c", c(6, 7))
    #' map
    #' map$insert("a", c(8, 9))
    #' map
    #' map$insert("a", c(8, 9), replace = TRUE)
    #' map
    insert = function(key, value, replace = FALSE){
      stopifnot(isString(key))
      stopifnot(isBoolean(replace))
      if(replace){
        private[[".map"]]$assign(key, value)
      }else{
        private[[".map"]]$insert(key, value)
      }
      invisible(NULL)
    },
    
    #' @description Erase the entries of the reference map whose keys are the 
    #'   given ones.
    #'
    #' @param keys some keys, a character vector
    #'
    #' @return Nothing, this updates the map.
    #'
    #' @examples
    #' map <- omapR$new(
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
      }else if(length(keys) >= 2L){
        private[[".map"]]$merase(keys)
      }
      invisible(NULL)
    },
    
    #' @description Merge the reference map with another map.
    #'
    #' @param map an \code{omapR} object
    #' @param duplicated the action to perform if the reference map 
    #'   and \code{map} have some identical keys, one of 
    #'   \code{"drop"}, \code{"join"}, or \code{"separate"}
    #'
    #' @return Nothing, this updates the reference map.
    #'
    #' @examples
    #' map1 <- omapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map2 <- omapR$new(
    #'   keys = c("c", "d"), values = list(c(9, 8), c(7, 6))
    #' )
    #' map1$merge(map2)
    #' map1
    #' 
    #' # `duplicated` example ####
    #' map1 <- omapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map1_copy1 <- map1$copy()
    #' map1_copy2 <- map1$copy()
    #' map1_copy3 <- map1$copy()
    #' map2 <- omapR$new(
    #'   keys = c("a", "d"), values = list(c(9, 8), c(7, 6))
    #' )
    #' map1_copy1$merge(map2)
    #' map1_copy1
    #' 
    #' map1_copy2$merge(map2, duplicated = "join")
    #' map1_copy2
    #' 
    #' map1_copy3$merge(map2, duplicated = "separate")
    #' map1_copy3
    merge = function(map, duplicated = "drop"){
      stopifnot(inherits(map, "omapR"))
      duplicated <- match.arg(duplicated, c("drop", "join", "separate"))
      if(duplicated != "drop"){
        keys1 <- self$keys()
        keys2 <- map$keys()
        if(length(intersect(keys1, keys2))){
          keys <- c(keys1, keys2)
          values <- c(self$values(), map$values())
          if(duplicated == "join"){
            splt <- split(values, keys)
            values <- lapply(splt, function(x){
              if(length(x) == 1L) x[[1L]] else x
            })
            keys <- names(values)
          }else{ # duplicated == "separate"
            keys <- make.unique2(keys)
          }
          OMAPR <- new("oMAPR", keys, values)
          private[[".map"]] <- OMAPR
        }else{
          .map2 <- map[[".__enclos_env__"]][["private"]][[".map"]]
          private[[".map"]]$merge(.map2$ptr)
        }
        invisible(NULL)
      }else{
        .map2 <- map[[".__enclos_env__"]][["private"]][[".map"]]
        private[[".map"]]$merge(.map2$ptr)
        invisible(NULL)
      }
    },
    
    #' @description Copy the reference map.
    #'
    #' @return A copy of the reference map.
    #' 
    #' @examples 
    #' map <- omapR$new(
    #'   c("a", "b"), 
    #'   list(c(1,2), c(FALSE, TRUE))
    #' )
    #' true_copy <- map$copy()
    #' true_copy$erase("a")
    #' map
    #' naive_copy <- map
    #' naive_copy$erase("a")
    #' map
    copy = function(){
      omapR$new(ptr = private[[".map"]][["ptr"]])
    }
    
  )
)
