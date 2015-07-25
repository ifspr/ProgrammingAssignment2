## makeCacheMatrix takes a (square, invertible) matrix as an input
##   and creates a list containing functions to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the inverse of the matrix
##   4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve returns the inverse of a given matrix. 
## It first checks if the matrix inverse has been computed. 
## If so, it retrieves the inverse from the cache. 
## Otherwise, it computes the inverse of the matrix and 
##   stores that in the cache via the setinv function. 
## The matrix is assumed to be square and invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
