## The makeCacheMatrix function accepts a matrix and builds four functions that allow for getting or setting either the original matrix or the inverse matrix 
## (created via the solve() function.) By assigning the results of the makeCacheMatrix() function to a new object, that object can be passed to the 
## cacheSolve() function, which will retrieve the cached inverse matrix if available. If not, a cached inverse matrix is created and returned.

## Function allows a user to pass in a matrix, then it generates four functions that can be called against the object that the function was assigned to in order
## to retrieve or reset the original or inverse matrices. For example: x <- makeCacheMatrix(m) would then allow you to call x$get() 
## or x$set(matrix(c(1,-3,2,-4), 2, 2)) to get the original matrix or rebuild the original matrix with a new matrix.

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    ## setter function for setting the original matrix
    set <- function(mtx) {
        x <<- mtx
        invM <<- NULL  ## removes the cached inverse matrix if the set() function is called, will need to be rebuilt using setInverse()
        
    }
    ## getter function for retrieving the original matrix
    get <- function( ) x
    ## setter function for setting the inverse matrix
    setInverse <- function(mtx) {
        invM <<- solve(mtx) 
    }
    ## getter function for retrieving the inverse matrix
    getInverse <- function( ) invM  
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function first tests to determine whether the inverse matrix is cached via the getInverse() function. If true, the cached inverse matrix is returned.
## If false, the original matrix is retrieved via the get() function, then passed to the setInverse() function to build the cached inserve matrix.
## Finally, the newly-cached inverse matrix is returned.

cacheSolve <- function(x, ...) {
    ## assigns the results of the getInverse() function to a variable
    test <- x$getInverse()
    ## test the variable to see if there is a cached inverse matrix and if so, returns the cached matrix
    if(!is.null(test)) {
        message("coming right up ...")    
        return(test)  
    }
    ## if there is no cached matrix, uses the get() and setInverse() functions to build the cached matrix, then the getInverse() function to return the cached matrix
    message("sorry, we're a little slow today :(")
    x$setInverse(x$get())
    return(x$getInverse())    
}
