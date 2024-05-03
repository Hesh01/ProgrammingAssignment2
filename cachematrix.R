## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  #initializing inverse as NULL
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #funtioning a get to matrix
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() {
    inver<-ginv(x)
    inver%*%x
  }
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


#used to get cache data

cacheSolve <- function(x, ...) {         #get cache data
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){                  #checking whether inverse is NULL
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
