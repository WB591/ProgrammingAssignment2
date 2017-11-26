## The below functions are useed to create a matrix and cache its inverse.
## makeCacheMatrix creates a matrix, which does the following 1.creates the matrix
## 2. gets the value of the matrtix, 3. sets the inverse, 4. gets the inverse.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will calculate the inverse of the matrix unless it has already been 
## calculated then it will retrive the cached inverse 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting chached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
