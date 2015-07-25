## MakeCacheMatrix has the following 
## functions that creates a vector of  Matrix class. 
## Sets the value of the vector and gets the same . 
## Similarly the inverse value of the matrix is also created as setinverse.
## And vector value is called back by calling getinverse.

## maieCacheMatrix creates a matrix to set, get, setinverse and getinverse 
## the matrix value facilitating to call the getinverse of a function.
makeCacheMatrix <- function(r = matrix()) 
{
      k <- NULL
    set <- function(s) {
      r <<- s
      k <<- NULL
    }
    get <- function() r
    setinverse <- function(inv) k <<- inv
    getinverse <- function() k
    list (set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}
## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available
cacheSolve <- function(r, ...) 
{
  k <- r$getinverse()
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  m <- r$get()
  k <- solve(m, ...)
  r$setinverse(k)
  k
}
## Here is an example showing us the inverse of a matrix Cached for a unit matrix.
## Unit Matrix is choosen as i = i inverse, easy to check.
u <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
u
v <- makeCacheMatrix(u)
cacheSolve(v)