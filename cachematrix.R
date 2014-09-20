# For R Programming Coursera course
# Peter J Dodge. 20/09/2014.
# Version 1.1
## Two functions to cache the inverse of a matrix.

## This function creates a special "matrix" object (list actually) that can cache its inverse.
## Create function Matrix (list) as follows (e.g.): MyMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {x <<- y; cache <<- NULL}  # set at global environment level.
    get <- function() x
    setcache <- function(mycache) cache <<- mycache  # set chache at global env. level.
    getcache <- function() cache
    return(list(set = set, get = get, setcache = setcache, getcache = getcache))
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## call cacheSolve 'after' creating function matrix above as follows:
## cachesolve(MyMatrix)
## First time through cacheSolve creates 'mymatrix' since 'cache' does not exist.
## Second (and subsequent) call(s) 'cache' exists so 

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    cache <- x$getcache()
    if(!is.null(cache)) {message("getting cached data"); return(cache)}
    mymatrix <- x$get()
    cache <- solve(mymatrix)  # we assume that mymatrix is invertable.
    x$setcache(cache) # Set cache at global environment level for next time.
    return(cache)  # I always like to use an explicit 'return' for a function.
}