## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # assume the matrix supplied is a square (even rows and columns) and always invertible
    
    inv = NULL
    
    # Set the matrix
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #get the matrix
    get = function() x
    
    #set the inverse
    setinv = function(i) inv <<- i
    
    #get the inverse
    getinv = function() inv
    
    #return a list that will be used in the cacheSolve() function
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

    # returns the inverse of the matrix from makeCacheMatrix()
    
    inv = x$getinv()
    
    # check to see if the inverse has already been calculated previously 
    # if inverse exists, then return the inverse and avoid the calculations below in the function
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # if no inverse exists, then calculate the inverse
    data = x$get()
    inv = solve(data, ...)
    
    # set the inverse in the cache by calling the setinv function
    x$setinv(inv)
    
    return(inv)
}
