## These functions are designed to save computation by caching the inverse of a matrix
## rather than recalculating each time. If the inverse is cached already the function
## returns the cached version as opposed to recalculating

## makeCacheMatrix creates a special vector which stores a matrix caches the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of a matrix 
## when supplied the cache matrix provided by makeCacheMatrix as x
## if the inverse has already been created and cached then it returns the cached version 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting Cached Data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
