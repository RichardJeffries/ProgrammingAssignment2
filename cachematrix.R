## Two functions to cache the inverting of matrices. One to create
## 'objects' that have cacheable inverses and one to solve them
## using the cached result where available

## Creates special cacheable matrices from regular ones

makeCacheMatrix <- function(x = matrix()) {
  # i will hold the inverse
  i <- NULL
  
  # local function to set the matrix and reset the inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # getter for the actual matrix 
  get <- function() x
  
  # setter and getter for the matrix inverse
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  # return value is a list of the local functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Caches the inverse of a 'cachematrix' as created
# by the function above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  # If the passed matrix already has return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Get the underlying matrix, solve it and set it for re-use later
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
