## Given a matrix 'x', a special cache object (called m1) is calculated by the function makeCacheMatrix and stored in the makeCacheMatrix
## environment.  The function CacheSolve calculates the inverse of the special  matrix (m1) returned from makeCacheMatrix.
## If the inverse has already been calculated then the CacheSolve will return the inverse from the cache.

## creates a matrix x where it stores the matrix given to it in enironment of makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {          
        m1 <- NULL                               ##initializes the inverse of x (stored in m1) as null until inverse of x is calcd
        set <- function(y = matrix()) {  	 ##defines 4 function - 1st function 'set' (argument 'y')
                x <<- y
                m1 <<- NULL
        }
        get <- function() x                      ##2nd function (empty argument)
        setinverse <- function(solve) m1 <<- solve	##3rd function (argument 'mean')
        getinverse <- function() m1               ##4th function (empty argument)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Return a matrix (m1) that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m1 <- x$getinverse()
        if(!is.null(m1)) {                            ##checks to see if inverse is already calculated
                message("getting cached data")        ##not created so set to cache value above
                return(m1)
        }
        data <- x$get()                               ##otherwise calculate the inverse of the data
        m1 <- solve(data, ...)
        x$setinverse(m1)                              ##set the value of the inverted matrix in the cache via the setinverse function
        m1
}
