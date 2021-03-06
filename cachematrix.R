## makeCacheMatrix(): 
# This function takes in an invertible matrix, and stores it internally.
# It also has an internal variable, m, for storing the inverse of the matrix.
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m
    
    # Calling makeCacheMatrix will return a list structure containing a list of named elements, each of which is a function.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve():
# This function takes in the result from makeCacheMatrix(), which includes a list structure that contains 4 functions.
# cacheSolve() will compute the inverse matrix, and cache it in the variable m.
# It will also return the inverse matrix. 
##
cacheSolve <- function(x, ...) {        
    # First, attempt to get the data from the cached variable "m".
    m <- x$getinverse()
    
    # If "m" exists, return it.
    if(!is.null(m)) {
        message("Getting from cached data...")
        return(m)
    }
    
    # Otherwise, if "m" does not exist, get the original matrix, compute its inverse, then cache the inverse matrix in variable "m". Lastly, return the inverse matrix.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m        
}


## -- Run the below code for testing the above functions --
# matrix2 <- matrix(c(4,3,3,2),2,2)  # Creation of a 2x2 square matrix.
# matrix3 <- matrix(c(1,1,1,3,4,3,3,3,4), 3, 3) # Creation of a 3x3 square matrix.
# listTemp <- makeCacheMatrix(matrix2)
# cacheSolve(listTemp)
# cacheSolve(listTemp)    # This 2nd call to cacheSolve() will return the cached inverse matrix, instead of recalculating it.
# listTemp$set(matrix3)   # Set the "x" variable in listTemp to the 3x3 matrix instead.
# cacheSolve(listTemp)
# cacheSolve(listTemp)    # Again, this 2nd call to cacheSolve() will return the cached value instead.
