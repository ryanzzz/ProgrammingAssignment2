## The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinvs <- function(invs) iv <<- invs
    getinvs <- function() iv
    list(set = set, get = get,
         setinvs = setinvs,
         getinvs = getinvs)
    
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse ofthe data using the solve() function, which
## requires the matrix to be square, and sets the value of the inverse in the cache via the setinvs function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iv <- x$getinvs()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinvs(iv)
    iv
}
