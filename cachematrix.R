## Caching the Inverse of a Matrix

## matrix assign to variable x, and initialize m to NULL

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set<-function(y){   ## reset matrix
        x<<-y           ## assign the matrix to x
        m<<-NULL        ## reinitialize to m
    }
    get <- function() x
    setmatrixinv <- function(matrixinv) m <<- matrixinv
    getmatrixinv <- function() m
    list(set = set, get = get,
         setmatrixinv = setmatrixinv,
         getmatrixinv = getmatrixinv)
}

## Return a matrix that is the inverse of 'x' or retrieve from cache
cacheSolve <- function(x, ...) {
    
    m <- x$getmatrixinv()              
    if(!is.null(m)) {           ## if calculation was done earlier
        message("Getting cached data")  
        return(m)               ## return old result(m) directly 
    }
    data <- x$get()             ## else get the  matrix
    m <- solve(data, ...)       ## calculate the inverse matrix
    x$setmatrixinv(m)           ## reassign inverse matrix 
    m                           ## print the inverse matrix 
}