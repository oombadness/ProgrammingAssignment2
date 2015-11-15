## Andrew J Austin - ajausti1@gmail.com - 11/15/15
##
## R Programming - Peer Reviewed Project #1
##
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.
##

## Create the special matrix object that can cache a solved version of the matrix supplied
## during construction or on call of the 'set' function.  This method assumes that the
## supplied matrix is solvable.
makeCacheMatrix <- function(x = matrix()) {
  solvedMatrix <- NULL
  set <- function(y) {
    x <<- y
    solvedMatrix <<- NULL
  }
  get <- function() x
  setSolvedMatrix <- function(solution) solvedMatrix <<- solution
  getSolvedMatrix <- function() solvedMatrix
  list(set = set, get = get,
       setSolvedMatrix = setSolvedMatrix,
       getSolvedMatrix = getSolvedMatrix)
}

## Accepts a special matrix object created as the result of calling makeCacheMatrix and solves
## the matrix.  Returns a cached copy of the matrix solution if a cached copy exists, orhterwise
## solves, caches and returns the solution.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolvedMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolvedMatrix(m)
  m
}
