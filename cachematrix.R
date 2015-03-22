## Put comments here that give an overall description of what your
## functions do
## These functions together provide a way to only have to 
## calculate the inverse of an (invertable) matrix once.
## When the inverse is needed subsequent times, the 
## cached (saved) inverse will be returned, saving 
## computation resources

## Write a short comment describing this function
## This function, makeCacheMatrix, takes in an invertible matrix
## calculates the inverse, and saves the value, which is 
## later retrievable via a function call from 
makeCacheMatrix <- function(x = numeric()) {
    ## initialize mtx
    mtx <- NULL
    ## print(environment())
    ## evn <- environment()
    ## print(parent.env(evn))
    ## general set function
    set <- function(y) {
        x <<- y
        mtx <<- NULL
    }
    ## function to return original matrix
    get <- function() x
    ## function to set the inverse matrix to cached object
    setinverse <- function(solved) mtx <<- solved
    ## function to return the inverse matrix
    getinverse <- function() mtx
    ## function to return the environment (for debugging/learning)
    getevn<- function() environment()
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse,
         getevn = getevn)
}

## This function takes a matrix (of object type makeCacheMatrix) 
## as input, checks to see if its inverse has been calculated 
## and cached already and returns the cached inverse if it was 
## found, otherwise it solves for the inverse and saves (caches) 
## it for next time.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    ## if the inverse matrix is found (value is not null) 
    ## then print the message and return the cached value
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    ## otherwise, get the original matrix
    mtx <- x$get()
    ## solve for the inverse
    inv <- solve(mtx, ...)
    ## set the inverse by calling the setinverse function on the 
    ## matrix object
    x$setinverse(inv)
    ## return the newly calculated matrix inverse
    inv
}

## testing:
## square.matrix <- matrix(c(1,2,3,0,1,4,5,6,0),ncol=3)
## print(square.matrix)
## cmtx <- makeCacheMatrix(square.matrix)
## smtx <- cacheSolve(cmtx)
## smtx <- cacheSolve(cmtx) 
## should see "getting cached data" comment


