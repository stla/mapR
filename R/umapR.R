#Rcpp::loadModule("umaprModule", what = "uMAPR")

#' @title R6 class representing an unordered map.
#'
#' @description A map is given by keys and values. The keys must be 
#'   some strings, the values can be any R objects.
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom methods new
umapR <- R6Class(
  
  "umapR",
  
  lock_class = TRUE,
  
  cloneable = FALSE,
  
  private = list(
    # .ptr = NULL,
    .map = NULL
  ),
  
  public = list(
    
    #' @description Creates a new \code{umapR} object.
    #'
    #' @param keys the keys, a character vector without \code{NA} value
    #' @param values the values, a list of R objects; \code{keys} and 
    #'   \code{values} must have the same length
    #' @param duplicated the action to perform for duplicated keys, one of 
    #'   \code{"drop"}, \code{"join"}, or \code{"separate"}
    #' @param checks Boolean, whether to check \code{keys} and \code{values}
    #' @param ptr an external pointer; this is for internal use only
    #'
    #' @return A \code{umapR} object.
    #'
    #' @examples
    #' umapR$new(
    #'   keys = c("a", "b"), 
    #'   values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' # examples with duplicated keys:
    #' umapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6))
    #' )
    #' umapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6)),
    #'   duplicated = "join"
    #' )
    #' umapR$new(
    #'   keys = c("a", "a", "b"), 
    #'   values = list(c(1, 2), c(3, 4), c(5, 6)),
    #'   duplicated = "separate"
    #' )
    initialize = function(keys, values, duplicated = "drop", checks = TRUE, ptr = NULL){
      if(!is.null(ptr)){
        UMAPR <- new("uMAPR", ptr)
        private[[".map"]] <- UMAPR
        invisible(NULL)
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
          # modes <- vapply(values, mode, character(1L))
          # if(any(modes != "numeric")){
          #   stop("The values must be given as a list of numeric vectors.")
          # }
        }
        if(duplicated != "drop" && anyDuplicated(keys)){
          if(duplicated == "join"){
            splt <- split(values, keys)
            values <- lapply(splt, function(x){
              if(length(x) == 1L) x[[1L]] else x#do.call(c, lapply(x, list))
            })
            keys <- names(values)
          }else{ # duplicated == "separate"
            keys <- make.unique2(keys)
          }
        }
        UMAPR <- new("uMAPR", keys, values)
        private[[".map"]] <- UMAPR
        invisible(NULL)
        # private[[".ptr"]] <- UMAPR$ptr
      }
    },
    
    #' @description Show instance of a \code{umapR} object.
    #' @param ... ignored
    print = function(...) {
      size <- self$size()
      if(size == 0L){
        cat("empty `umapR` object")
      }else{
        keys_values <- private[[".map"]]$toList()
        keys <- sprintf('"%s"', keys_values[["keys"]])
        values <- vapply(keys_values[["values"]], toString2, character(1L))
        s <- ifelse(size > 1L, "s", "")
        cat(sprintf("`umapR` object containing %d item%s:\n\n", size, s))
        lines <- paste0("  ", keys, " -> ", values)
        cat(lines, sep = "\n")
      }
    },
    
    #' @description Size of the map.
    #'
    #' @return An integer, the number of entries.
    #'
    #' @examples
    #' map <- umapR$new(
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
    #' map <- umapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$keys()
    keys = function(){
      private[[".map"]]$keys()
    },

    #' @description Get all values.
    #'
    #' @return The values, a list of R objects.
    #'
    #' @examples
    #' map <- umapR$new(
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
    #' map <- umapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$items()
    items = function(){
      L <- private[[".map"]]$toList()
      keys <- L[["keys"]]
      o <- order(keys)
      values <- L[["values"]]
      data.frame(key = keys, value = I(values))[o, , drop = FALSE]
    },
    
    #' @description Converts the map to a named list.
    #'
    #' @return A named list.
    #'
    #' @examples
    #' map <- umapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$toList()
    toList = function(){
      L <- private[[".map"]]$toList()
      keys <- L[["keys"]]
      o <- order(keys)
      values <- L[["values"]][o]
      names(values) <- keys[o]
      values
    },
    
    #' @description Returns the value corresponding to the given key.
    #'
    #' @param key a key (string)
    #' @param stop_if_not_found a Boolean value, whether to stop if the key 
    #'   is not found, or to return \code{NaN}
    #'
    #' @return The value corresponding to the key, an R object.
    #'
    #' @examples
    #' map <- umapR$new(
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
    
    #' @description Extract a submap.
    #'
    #' @param keys some keys, a character vector; those which do not belong to 
    #'   the keys of the reference map will be ignored
    #' @param inplace Boolean, whether to update the reference map or 
    #'   to return a new map
    #' @param bydeleting Boolean, whether to construct the submap by 
    #'   deleting the keys which are not in \code{keys} or by starting 
    #'   from the empty submap and adding the entries 
    #'
    #' @return A \code{umapR} object if \code{inplace=FALSE}, 
    #'   nothing otherwise.
    #'
    #' @examples
    #' map <- umapR$new(
    #'   keys = c("a", "b", "c"), 
    #'   values = list(c(1, 2), c(3, 4, 5), c(6, 7))
    #' )
    #' map$extract(c("a", "c"))
    #' map
    #' map$extract(c("a", "c"), inplace = TRUE)
    #' map
    extract = function(keys, inplace = FALSE, bydeleting = FALSE){
      stopifnot(isCharacterVector(keys))
      stopifnot(isBoolean(inplace))
      stopifnot(isBoolean(bydeleting))
      if(length(keys) == 0L){
        if(inplace){
          private[[".map"]] <- new("uMAPR", character(0L), list())
          invisible(NULL)
        }else{
          umapR$new(character(0L), list(), checks = FALSE)
        }
      }else{
        if(bydeleting){
          if(inplace){
            private[[".map"]]$extract_by_erasing_inplace(keys)
            invisible(NULL)
          }else{
            ptr <- private[[".map"]]$extract_by_erasing(keys)
            umapR$new(ptr = ptr)
          }
        }else{
          # ptr <- private[[".map"]]$extract(keys) 
          if(inplace){
            # UMAPR <- new("uMAPR", ptr)
            # private[[".map"]] <- UMAPR
            private[[".map"]]$extract_inplace(keys)
            invisible(NULL)
          }else{
            ptr <- private[[".map"]]$extract(keys)
            umapR$new(ptr = ptr) 
          }
        }
      }
    },

    #' @description Checks whether a key exists in a map.
    #'
    #' @param key a key (string)
    #'
    #' @return A Boolean value.
    #'
    #' @examples
    #' map <- umapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map$has_key("b")
    #' map$has_key("x")
    has_key = function(key){
      stopifnot(isString(key))
      private[[".map"]]$has_key(key)
    },
    
    #' @description Insert a new entry.
    #'
    #' @param key a key (string)
    #' @param value a value (R object)
    #' @param replace Boolean, whether to replace the value if the key is 
    #'   already present
    #'
    #' @return Nothing, this updates the map.
    #'
    #' @examples
    #' map <- umapR$new(
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
      stopifnot(isBoolean(replace))
      if(replace){
        private[[".map"]]$assign(key, value)
      }else{
        private[[".map"]]$insert(key, value)
      }
      invisible(NULL)
    },
    
    #' @description Erase some entries.
    #'
    #' @param keys some keys, a character vector
    #'
    #' @return Nothing, this updates the map.
    #'
    #' @examples
    #' map <- umapR$new(
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
    
    #' @description Merge with another map.
    #'
    #' @param map a \code{umapR} object
    #' @param duplicated the action to perform if the reference map 
    #'   and \code{map} have some identical keys, one of 
    #'   \code{"drop"}, \code{"join"}, or \code{"separate"}
    #'
    #' @return Nothing, this updates the reference map.
    #'
    #' @examples
    #' map1 <- umapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map2 <- umapR$new(
    #'   keys = c("c", "d"), values = list(c(9, 8), c(7, 6))
    #' )
    #' map1$merge(map2)
    #' map1
    #' 
    #' # `duplicated` example ####
    #' map1 <- umapR$new(
    #'   keys = c("a", "b"), values = list(c(1, 2), c(3, 4, 5))
    #' )
    #' map1_copy1 <- map1$copy()
    #' map1_copy2 <- map1$copy()
    #' map1_copy3 <- map1$copy()
    #' map2 <- umapR$new(
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
      stopifnot(inherits(map, "umapR"))
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
          UMAPR <- new("uMAPR", keys, values)
          private[[".map"]] <- UMAPR
          invisible(NULL)
        }else{
          .map2 <- map[[".__enclos_env__"]][["private"]][[".map"]]
          private[[".map"]]$merge(.map2$ptr)
          invisible(NULL)
        }
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
    #' map <- umapR$new(c("a", "b"), list(c(1,2), c(FALSE, TRUE)))
    #' true_copy <- map$copy()
    #' true_copy$erase("a")
    #' map
    #' naive_copy <- map
    #' naive_copy$erase("a")
    #' map
    copy = function(){
      umapR$new(ptr = private[[".map"]][["ptr"]])
      #umapR$new(self$keys(), self$values(), checks = FALSE)
    }
    
  )
)
