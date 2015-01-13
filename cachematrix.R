## this function just cache the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #seting values
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #getting values
  get <-function()x
  #  setting function for cache inverse
  setinverse <- function(inverse) inv <<- inverse
  # getting inverse
  getinverse <- function()inv
  
  # setings setter and getter
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## here we solve the inverse of matrix using solve method and also show cache result if it is already processed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## checking if there is already cached data available
  if(!is.null(inv)){
    message("getting cahced data")
    return (inv)
  }
  ## else finding the inverse of matrix
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
