# This function is used to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(calinv) inv <<- calinv
  getInv <- function() inv
  list(set = set,               # set value of matrix
       get = get,               # get value of matrix
       setInv = setInv,         # set value of inverse
       getInv = getInv)         # get value of inverse
}

# This function finds the inverse of the matrix obtained from makeCacheMatrix above
# If matrix whose inverse is calculated remains unchanged then this function will retrieve inverse from cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...) # solve() is a function which computes inverse of square invertible matrix
  x$setInv(inv)
  inv
}
