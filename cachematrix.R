## A pair of functions that create a special "matrix" object 
## that can cache the inverse of a matrix and retrieve it. 
## When retreaving the inverse, if the cache is empty, the
## inverse is then calculated and stored.



## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      c <- NULL
      calcCache <- function () c <<- solve(x)
      getCache <- function() c
      getMatrix <- function() x
      set <- function (y) 
            c <<- y
      list (calcCache = calcCache, getCache = getCache, getMatrix = getMatrix, set = set)      
}



## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      c <- x$getCache()
      if(!is.null(c)) {
            message("getting cached data")
            return(c)
      }
      else {
            m <- x$getMatrix()
            c <- solve(m)
            x$set(c)
            return(c)
      }
}