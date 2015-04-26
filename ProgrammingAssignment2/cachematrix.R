## This function creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize matrix inverse
  inverse <- NULL
  
  ## Function to set matrix    
  set <- function(y) {
    x <<- y
    ## Reset value of the matrix inverse in case the original matrix has changed.
    inverse <<- NULL
  }
  
  ## Get the value of the original matrix
  get <- function() x
  
  ## Set the value of matrix inverse
  setinverse <- function(matrixinverse = matrix()) inverse <<- matrixinverse
  
  ## Get the value of the matrix inverse
  getinverse <- function() inverse
  
  ##Create function list to store the defined four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special matrix returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  ## Get matrix inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    ## If the inverse was already calculated, then retrieve it from cache
    message("getting cached data")
    return(i)
  }
  
  ## Get the original matrix
  data <- x$get()
  
  ## Calculate matrix inverse
  i <- solve(data)
  
  ## set value for inverse of the matrix
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}




