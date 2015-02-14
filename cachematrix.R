## The following two functions allow to create
## a cachable inverse of an existing matrix.
## Example usage:
##
## > cells <- c(1,26,24,68)
## > rnames <- c("R1", "R2")
## > cnames <- c("C1", "C2")
## > testMatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,dimnames=list(rnames, cnames))
## > x <- makeCacheMatrix(testMatrix)
## > cacheSolve(x)
##      R1           R2
## C1 -0.12230216  0.046762590
## C2  0.04316547 -0.001798561
## > cacheSolve(x)
## getting cached data
##      R1           R2
## C1 -0.12230216  0.046762590
## C2  0.04316547 -0.001798561

## Function that creates a cachable matrix.
## The returned object is a list of functions
## to get and set a source matrix as well as to 
## get and set the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns a matrix that is the inverse of x.
## In case the inverse was calculated previously
## a cached value is being returned.
cacheSolve <- function(x) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}