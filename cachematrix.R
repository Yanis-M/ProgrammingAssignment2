## The inverse of a matrix may require very intensive computation, 
## below is a creation two functions to cash the inverse of a matrix in order to get this latter be used in other computations.

## makeCacheMatrix will cach a matrix, it is a list of four functions:
## 1. set: set the matrix itself
## 2. get: get the matrix itself
## 3. setinverse: set the inverse of a matrix
## 4. getinverse: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## inv initially set to NULL
        
        set <- function(y) {
                x <-- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        List(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve: a function to compute the inverse of the matrix and cache the result
## it will :
##      1. get the inverse:
##              1.1 if it is available return its value with a message indicating what has been done
##              1.2 if the inverse is not available, it will get the matrix then calculate its inverse
##      3. get the inverse cached and return the result

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("call to the cached matrix")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- inverse(x, ...)
        
        x$setinverse(inv)
        
        inv
}
