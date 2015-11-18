## Fulfillment of R-Prog034 Assignment 2
## What follows are two functions, designed
## to create a cached matrix, and then subsequently to 
## return an inversion of the matrix from a cached version

## Basic function to create cache-able, invertable matrix:
makeCacheMatrix <- function(x = matrix()) {
    ## initializes "m' variable in alternate environment:
    m<<-NULL
    ## defines the subfunction "set"
    set<- function(y) {
        x<<-(y)
        m<<-NULL
    }
    
    get<- function() x
    setsolve<- function(solve) m<<- solve
    getsolve<- function() m
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
    
}




## Function examines matrix created above.
## If solve of matrix has already been cached, return cached solve
## If solve has not been cached, solves matrix

cacheSolve <- function(x, ...) {
        m<- x$getsolve()
        ## Checks if a solve is NOT in cache:
        if (!is.null(m)) {
            ## informs user that cached solution is being used:
            message("Getting cached data")
            ## provides the cached solution:
            return(m)
            }
        data<- x$get ()
        m<- solve(data, ...)
        x$setsolve (m)
        m
}

