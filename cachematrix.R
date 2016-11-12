## Functions for ProgrammingAssignment2

## set and get the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL # initialize s
    set <- function(y) {
        x <<- y # set cached x to y value
        s <<- NULL # initialize cached s
    }
    # get will execute x as function
    get <- function() x
    # setinverse will cache solve as s
    setinverse <- function(solve) s <<- solve 
    # getinverse will execute s as function
    getinverse <- function() s
    # final argument returns list of 4 potential actions defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## computes inverse of a square invertable matrix
## checks square-ness but assumes invertable, per instructions

cacheSolve <- function(x, ...) {
    # literally assign to s the getinverse function from the list x
    # effectively, this pulls pulls from cache the current inverse matrix value
    s <- x$getinverse()
    if(!is.null(s)) { #check if s is already set
        message("getting cached data") # if so notify the user
        return(s) # and return the value, and exit
    }
    data <- x$get() # get the matrix and assign it to data
    # assigns inverse of matrix data to s
    # argument b, so solve returns inverse
    s <- solve(data, ...) 
    x$setinverse(s) # execute setinverse, which caches inverse value
    s #returns s to user
    }
