## David Greig
## Coursera R Programming
## 2018-03-17

## The following functions provide the ability to calculate the 
## inverse of a matrix and cache it for subsequent access.
## Caching the inverse of a matrix provides for an increased speed 
## versus repeatedly calculating its inverse.

## For the purpose of this exercise, we assume that the passed matrix
## is invertible

## ex: 
## a<-matrix(c(5,1,0,3,-1,2,4,0,-1), nrow=3, byrow=TRUE)
## mcm<-makeCacheMatrix(a) <-- create matrix object capable of caching its inverse
## cs<-cacheSolve(mcm) <-- initial call: inverse of a is calculated and cached
## cs<-cacheSolve(mcm) <-- second and subsequent calls: inverse of a is retrieved from cache

## fxn: makeCacheMatrix
## Given an invertible matrix x, the function will return an object capable of 
## caching its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## fxn: cacheSolve
## Given the object created by makeCacheMatrix, this function will first check
## to see if the inverse has already been calculated and if so, retrieve it
## from the 'cache'.  If it has not been calculated, it will calculate it and
## store it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
