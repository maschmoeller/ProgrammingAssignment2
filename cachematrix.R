## The following functions are supposed to dialogue with each other; one creates
## a matrix object that is determined to cache its inverse whenever it is computed
## while the second makes use of that property to avoid recomputing.

## The makeCacheMatrix function creates a matrix object that can cache its inverse.
## First, it defines the inverse matrix object ("a") as NULL, since it has not
## been computed yet, but stores the matrix inputed ("x")
## Then, it defines that the inverse matrix of the inputed matrix, when calculated,
## by means of the function "solve", will be cached to the "a" object
makeCacheMatrix <- function(x = matrix()) {
      a <- NULL
      set <- function(y) {
            x <<- y
            a <<- NULL
      }
      get <- function() x
      setinv <- function(solve) a <<- solve # attributes the inverse matrix to "a"
      getinv <- function() a
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above, or simply returns it when it has already been computed.
## First, it checks if the inverse has not been calculated before (and, in consequence,
## cached in object "a"). If it has, it returns the message "getting cached data"
## and, subsequently, the inverse matrix
## If not, it computes and returns the inverse matrix of the object
cacheSolve <- function(x, ...) {
      a <- x$getinv()
      if(!is.null(a)) {
            message("getting cached data")
            return(a)
      }
      data <- x$get()
      a <- solve(data, ...)
      x$setinv(a)
      a
}

## If this was the first run of cacheSolve in a matrix object created with 
## makeCacheMatrix, the inverse matrix is now cached, and if cacheSolve is once
## more applied to the same object it will return the "getting cached data" message

