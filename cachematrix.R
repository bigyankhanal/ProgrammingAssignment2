makeCacheMatrix <- function(x = matrix()) {  # this takes the input as matrix
  m <- NULL    
  set <- function(y) {      ## creating matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matinv) m <<- matinv   ## sets inverse of the matrix
  getinverse <- function() m   ## returns inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {     ## check for cache solution if yes returns solution
    message("getting cached data")      
    return(m)
  }
  data <- x$get()         
  m <- solve(data, ...)  ## solve function calculates the inverse of matrix
  x$setinverse(m)
  m
}