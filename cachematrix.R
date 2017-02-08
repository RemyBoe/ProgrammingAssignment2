#In the function cachematrix & cachesolve, you basicly can create a matrix and 
#you can cache the inverse. This is usefull because if you have to calcute 
#the inverse  of a matrix in a loop without caching, it will take you way longer,
# than if you can cache it and let the cachesolve function retrieve it from the cache
# function directly if it already has been calculated.

#cachematrix basicly creates a matrix which caches the inverse.
cachematrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}

#Cachesolve Computes the inverse of the matrix made by cachematrix. 
#If it already has been computed, it just retrieves it from the cache directly. 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        matrix <- x$get()
        
        inv <- solve(matrix, ...)
        
        x$setinv(inv)
        
        inv
}
