## These functions have been writen as part of the R programming week 3 assignment



## The below makeCacheMatrix function creates a special "matrix" object that can cache its inverse,
## gets matrix as an input, sets the value of matrix
## gets the value of matrix, sets the inverse matrix value and gets the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y) {
                x<<- y    ##<<- used to assign value to an object in an environment that's diff from current one
                inv<<- NULL
        }
        get<- function() x         ##gets the value of the matrix
        setinverse<- function(inverse) inv<<- inverse    ##sets the value of inverse matrix
        getinverse<- function() inv                      ##gets the value of inverse matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The below cacheSolve function computes the inverse of the special "matrix" returned by the above makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache below

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinverse()
        if(!is.null(inv)) {            ##if inverse matrix is not null
                message("getting cached data")
                return(inv)            ##returns the inverse matrix
        }
          ##if the value of inverse matrix is null, then
        data<- x$get()
        inv<- solve(data, ...)
        x$setinverse(inv)
        inv
}
