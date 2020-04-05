## R Programming - Peer Assignment 2
## Two functions to cache the inverse of a matrix

## Create special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m<- NULL
     set<- function(y) {
          x<<- y
          m<<- NULL
     }
     get<- function() x
     setinverse<- function(solve) m<<- solve
     getinverse<- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Compute inverse of special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m<- x$getinverse()
     if(!is.null(m)) {
          message("getting cached inverse")
          return(m)
     }
     data<- x$get()
     m<- solve(data)
     x$setinverse(m)
     m
}

