## Caching the inverse of a matrix
## The makeCacheMatrix function creates a matrix that can cache its inverse. When calling the cacheSolve function 
## on the output of makeCacheMatrix, if the inverse has already been calculated, it will return the cached result.
## Otherwise, the inverse will be cumputed.

## Create a special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) m <<- solve
        getinverse<-function() m
        list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}


## Retrieve the inverse from the cache or compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
