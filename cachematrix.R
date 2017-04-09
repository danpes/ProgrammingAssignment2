## This function is meant to cache a matrix object and its inverse.

makeCacheMatrix <- function(x = matrix()) {        ## default matrix argument
  inv <- NULL                                      ## inv will hold the value of matrix inverse 

  set <- function(y) {                             ## assigns new value of matrix in parent environment
    x <<- y                                        ## any time the matrix is re-set, its inverse is NULL-ed
    inv <<- NULL                                   
  }

  get <- function() {                              ## returns formerly set matrix
    x
  }
  
  setinverse <- function(inverse) { 
    inv <<- inverse                                ## assigns value of inv in parent environment
  }
  
  getinverse <- function() {                       ## returns inverse of matrix (which needs to be properly properly set)
    inv
  }
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)                    ## in order to refer to the functions with the $ operator
}


## Here we get the inverse of the formerly (through makeCacheMatrix) defined matrix.
## If the inverse has formerly been calculated on a same matrix object, then it will retrieve the inverse from the cache.
## Here we assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}

## tests
#w <- matrix(nrow=2,ncol=2,rexp(4, rate=.1))
#cm <- makeCacheMatrix(w)
#cm$get()
#cacheSolve(cm)
#cm$set(cacheSolve(cm))
#cacheSolve(cm)

