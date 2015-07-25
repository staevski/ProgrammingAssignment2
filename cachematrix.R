## A pair of functions that illustrate how to cache the inverse of a matrix
## to speed up the computation
## Example use and output:
## > mym <- matrix(c(2,1,5,3), nrow = 2, ncol = 2)
## > source("cachematrix.R")
## > myf <- makeCacheMatrix(mym)
## > cacheSolve(myf)
##      [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
## > cacheSolve(myf)
## Just got the cashed inverted Matrix
##      [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
## > 

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		
		im <- NULL
		
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        ##get the initial matrix (data)
        get <- function() x
        
        ##computing the inverse matrix and storing the result into the variable im 
        setInverse <- function(data) im <<- solve(data)
        
        ##just getting the m variable (the inverse matrix) from the environment
        getInverse <- function() im
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated 
##, then the "cacheSolve" function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        cashedim <- x$getInverse()
        
        if(!is.null(cashedim)) {
                message("Just got the cashed inverted Matrix")
                return(cashedim)
        }
        
        ##in case there is no cashed inverted matrix, compute it!
        data <- x$get()
        im <- x$setInverse(data)
        im
}
