

## this function just cache the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## here we solve the inverse of matrix using solve and also show cache result if it is already processed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cahced data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
