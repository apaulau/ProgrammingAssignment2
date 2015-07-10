## Matrix solving with caching abillities


## Makes matrix with predefined access methods 
## with ability of caching inverse matrix
makeCacheMatrix <- function (matrix = matrix()) {
    inverse <- NULL
    
    set <- function (m) {
        matrix <<- m
        inverse <<- NULL 
    }
    get <- function () { matrix }
    
    setInverse <- function (inv) {
        inverse <<- inv 
    }
    getInverse <- function () { inverse }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Gets inverse of passed matrix from the cache if possible
## otherwise computes and caches it
cacheSolve <- function (m, ...) {
    inverse <- m$getInverse()
    if (!is.null(inverse)) {
        message("getting cache")
        return(inverse)
    }
    
    matrix <- m$get()
    inverse <- solve(matrix, ...)
    m$setInverse(inverse)
    
    inverse
}
