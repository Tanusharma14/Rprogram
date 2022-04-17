# Rprogram
makeCacheMatrix <- function(x = matrix()) ## creates a matrix object that can cache its inverse
{
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## return value of matrix arguement
    setinverse <- function(inverse) inv <<- inverse         ## assigns value of inv in parent environment
    getinverse <- function() inv                            ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)     ## to refer function with $ operator
    }
    
    ## This function computes the inverse of the matrix returned by makeCacheMatrix above.
    ## If the inverse has already been calculated (and the matrix has not changed), cacheSolve will retrieve the inverse from the cache
    
    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
