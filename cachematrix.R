## Basicaly, it is two functions that work together and as a result they will return a matrix 
#that is the inverse of 'x', which was comptued by this functions or taken from the cache.

## The first function, makeCashMatrix creates a special "matrix", 
#which is a list containing a function to:

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setsolve <- function(inverseMatrix) invm <<- inverseMatrix
        getsolve <- function() invm
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## The following function compute the inverse matrix of the special "matrix" created with 
#the above function. However,if the inverse matrix has already been calculated, it gets 
#the inversion from the cache and skips the computation. Otherwise,
#it calculates the inversion matrix of the data and sets the value  in the cache 
#via the setsolve function.


cacheSolve <- function(x, ...) {
        invm <- x$getsolve()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setsolve(invm)
        invm
}
