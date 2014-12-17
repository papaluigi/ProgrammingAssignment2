## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector", which is really a 
## list containing some functions which 1. sets the value of the matrix, 2. gets the
## value of the matrix, 3. sets the value of the inverse matrix, 4. gets the value 
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Function which sets the input matrix in the cache
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## Function which returns the input matrix from the cache
        get <- function() x
        ## Function which stores the inverse matrix in the cache
        setinv <- function(solvedmat) m <<- solvedmat
        ## Function which returns the inverse matrix from the cache
        getinv <- function() m
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. It first checks to see if the inverse matrix has
## already been calculated. If so, it gets the inverse matrix from the cache and
## skips the computation. Otherwise, it calculates the inverse matrix of the data
## and sets the value of the inverse in the cache via the setinv function.
## A message is displayed when the cache is used.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## First check if an inverse matrix is cached
        m <- x$getinv()
        ## In case yes, then display confirmation message 
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        ## In case no, then get the input matrix...
        matrix <- x$get()
        ## ...calculate its inverse...
        m <- solve(matrix, ...)
        ## ...put it in the cache...
        x$setinv(m)
        ## ...And display it
        m
}
