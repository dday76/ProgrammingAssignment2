## Functions for ProgrammingAssignment2

## set and get the value of the matrix and its inverse
## constructor for makeCacheMatrix objects

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
    # functions above are now accessible using makeCacheMatrix$item()
    # () is required to execute function
    # x$set(), x$get(), x$setinverse(), x$getinverse()
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## computes inverse of a square invertable matrix
## checks square-ness but assumes invertable, per instructions
cacheSolve <- function(x, ...) {
    # first catch non-square matrix error
    # assignment specified we can assume invertable but not square
    # could use proper debugger, but it's simple enough for a simple if
    if(ncol(x$get())!=nrow(x$get())) { # if rows don't equal columns, return error
        message("It's too hard. Please give me a square matrix.")
        return(x$get()) # return matrix to check
        # matrix might be large so error should be below matrix so user sees it
        # but for the sanity of my peer graders, I'll just leave it as is.
    }
    # literally assign to s the getinverse function from the list x
    # effectively, this pulls pulls from cache the current inverse matrix value
    s <- x$getinverse()
    if(!is.null(s)) { #check if s is already set
        message("Cached data shown below.") # if so notify the user after matrix
        return(s) # and return the value, and exit
    }
    data <- x$get() # get the matrix and assign it to data
    # assigns inverse of matrix data to s
    # argument b left as default for solve, so solve returns inverse
    s <- solve(data, ...) 
    x$setinverse(s) # execute setinverse, which caches inverse value
    s #returns s to user
    }
