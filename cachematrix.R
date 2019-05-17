# Function group takes a matrix as input and calculates the inverse of it,
# storing the value in memory for subsequent calls

## take a matrix as input and return a list of 4 named functions.
makeCacheMatrix <- function(x = matrix()) {
        # set m to be NULL
        m <- NULL
        
        # create a function called "set" that takes an argument y, sets global
        # variables x to be y and m to be NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # creates function called "get" which returns x
        get <- function() x
        
        # creates function called setinverse which takes parameter and
        # assigns it to global variable m
        setinverse <- function(inverse) m <<- inverse
        
        # creates a function to return variable m
        getinverse <- function() m
        
        # returns a list of 4 functions, each named
        list(set = set
             , get = get
             , setinverse = setinverse
             , getinverse = getinverse)
}

# Checks to see if the inverse matrix is in memory and if so return it,
# otherwise perform the calculation
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        # check if the returned value is NULL. If not return the matrix in the
        # cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if here, the value in memory was NULL meaning we get the matrix that
        # was passed into makeCacheMatrix
        data <- x$get()
        
        #invert the matrix passed into makeCacheMatrix
        m <- solve(data, ...)
        
        # use the setinverse function in makeCacheMatrix to set the global
        # variable
        x$setinverse(m)
        
        # return the inverted matrix
        m
}
