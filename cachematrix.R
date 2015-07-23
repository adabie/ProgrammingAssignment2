## There are 2 functions to set up a matrix, invert it and cache the result.
## If the inverse is requested a second time the cached data will be supllied.


## makeCacheMatrix expects a matrix to be supplied.  The function then takes the
## matrix and sets it up by adding the functions onto the object to get and set
## the inverse data

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(solve) m<<- solve
        getinverse <- function()m
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve attempts to get the inverse matrix on the matrix object
## if the inverse doesn't exist it will generate the inverse using solve and
## cache the result.  If the data has been cached it will return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m))
        {
                message("getting cached inverse data")
                return(m)        
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
