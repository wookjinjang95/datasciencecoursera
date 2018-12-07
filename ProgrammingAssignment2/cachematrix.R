## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is suppose to create a cache memory allocate for the inverse
## matrix. It also returns a list within set, get_matrix, set_cached_matrix,
## and lastly get_cached_matrix values.

makeCacheMatrix <- function(x = matrix()) {
    cached_matrix <- NULL
    set <- function(y){
        x <<- y
        cached_matrix <<- NULL
    }
    get_matrix <- function() x
    set_cached_matrix <- function(inversed) cached_matrix <<- inversed
    get_cached_matrix <- function() cached_matrix
    
    list(set = set, get_matrix = get_matrix,
         set_cached_matrix = set_cached_matrix,
         get_cached_matrix = get_cached_matrix)
}


## Write a short comment describing this function
## This function first look if there is a cache result already calculated
## from the previous calculation. If not, then we calculate and store it
## into the cache so that if it gets called again, it can find it from cache.
cacheSolve <- function(x, ...) {
    cached_matrix <- x$get_cached_matrix()
    if(!is.null(cached_matrix)){
        message("Got the inverse matrix from cache")
        return(cached_matrix)
    }
    data <- x$get_matrix()
    cached_matrix <- solve(data)
    x$set_cached_matrix(cached_matrix)
    cached_matrix
}
