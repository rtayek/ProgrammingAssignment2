## Put comments here that give an overall description of what your
## functions do

## make a special vector to cach the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            inverse <<- y
            inverse <<- NULL
      }
      get <- function()
            x
      setinverse <- function(inverse_)
            inverse <<- inverse_
      getinverse <- function()
            inverse
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
      )
}


## invert a matrix or us the cached value.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if (!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}


## sanity test case

sanityTestCase <- function() {
      matrix <- diag(3)
      cacheFunction <- makeCacheMatrix(matrix)
      inverse = cacheSolve(cacheFunction)
      inverse2 <- cacheSolve(cacheFunction)
      product <- matrix %*% inverse
      if (!all(product == diag(3)))
            return(FALSE)
      product2 <- matrix %*% inverse
      if (!all(product2 == diag(3)))
            return(FALSE)
      TRUE

}
