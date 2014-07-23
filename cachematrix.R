## Makes a matrix which can cache its inverse
## It returns a list containing four functions:
##    1. set - sets the matrix x to matrix y given as parameter
##    2. get - method to access the matrix x
##    3. setInverse - method to set the inverse of matrix x
##    4. getInverse - method get the inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## method to set the matrix x and initialize the inverse to null
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## method to get back the matrix x
    get <- function() x
    ## method to set the inverse of matrix
    setInverse <- function(inv) inverse <<- inv
    ## method to get the matrix x
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## returns the inverse of matrix
##If already calculated returns the cached inverse
cacheSolve <- function(x, ...) {
        inverse<-x$getInverse()
        ## Checks if inverse of matrix is null.
        ## If not null returns the cached inverse.
        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
        }
        ## gets the matrix 
        data<-x$get()
        ## finds the inverse of matrix
        inverse<-solve(data)
        ## caches the inverse for future use
        x$setInverse(inverse)
        ## returns the inverse
        inverse
}
