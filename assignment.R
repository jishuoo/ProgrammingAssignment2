##makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<- function(x = matrix()) {
  m<- NULL
  set<- function(y) {
    x<<- y
    m<<- NULL
  }
  get<- function() x
  setInv<- function(inverse)m <<- inverse
  getInv<- function()m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

##cacheSolve: 
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  m<- x$getInv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat<- x$get()
  m<- solve(mat, ...)
  x$setInv(m)
  inv
}