##The makeCacheMatrix function calculates the mean of the special "vector" created with respect to the above function. It first checks to see if the mean has already been calculated.
##If so, it gets the mean from the cache. If not, calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

## These functions written in partial fulfillment of Coursera Data Science: R Programming Week 3 Assignment


## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
  
  ## Thw makeCacheMatrix function creates a special "matrix" object that can cache its inverse
  
  makeCacheMatrix <- function(x = matrix()) { ## to define the argument with default mode of "matrix"
    inv <- NULL                             ## to initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## to define the set function to assign new 
      x <<- y                             ## to get value of matrix in parent environment
      inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## to assign value of inv in parent environment
    getinverse <- function() inv                     ## to get the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
    ## to the functions with the $ operator
  }
  
  
  
  ##The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }