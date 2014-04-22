## makeCacheMatrix and cacheSolve reduce the computational cost of repeatedly calculating 
## the inverse of a matrix by caching the solution and making it available for retrieval

## makeCacheMatrix takes a matrix and creates a matrix object capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## cacheSolve takes the matrix object returned by makeCacheMatrix and returns the matrix inverse
## If the inverse has already been calculated it retrieves the cached solution
## If no cached inverse exists it calculates and caches the solution

cacheSolve <- function(x, ...) {
    ## attempt to retrieve cached inverse of matrix x
    i <- x$getInverse()
    
    if(!is.null(i)) { 
        ## if cached inverse was found inform user
        message("cached inverse retrieved")
    } else {
        ## if no cached inverse exists compute inverse and cache
        message("calculating inverse of matrix and caching")
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
    }
    
    ## return the inverse of matrix x
    i
}
