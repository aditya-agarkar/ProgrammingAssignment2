## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The following functions can be used to cache a matrix inverse and avoid computation repeatedly.
## To use the functions:
## Example
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()         # Returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## amatrix$getinverse()  # Returns matrix inverse
## cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

## The function makeCacheMatrix accepts a matrix and creates a list of functions
## to set the matrix, retireve matrix, set inverse of the matrix to a global vairable m
## and finally retrieves the matrix inverse from global vairable m

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
