##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

##----------------------------------------------------------------------------------------------------##

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #if the matrix is not square the function is terminated.
  if (nrow(x)!=ncol(x)) stop("The matrix is not square and it is not invertible")
    m <- NULL
  ## - set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## - get the value of the matrix
  get <- function() x
  ## - set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  ## - get the value of the inverse
  getinverse <- function() m
  ## return a list of all four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##----------------------------------------------------------------------------------------------------##

##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inversed matrix has already been calculated. 
##If so, it gets the inversed matrix from the cache and skips the computation. 
##Otherwise, it calculates the inversed matrix and sets the value of the inversed matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse matrix of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverted matrix")
    return(m)  #return inverted matrix 
  }
  # invert the matrix and pass it to m
  data <- x$get()
  m <- solve(data, ...)  
  x$setinverse(m)
  m
}