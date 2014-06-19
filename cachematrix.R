## Assignment 2 caching matrix and solving inverse


## Functions similar to an object in C based coding
## able to instantiate makeCacheMatrix with or with out 
## passing a matrix to it.  Using the set method can then pass a
## matrix.
## EXAMPLE:
## a <- makeCacheMatrix()
## x <- matrix(1:4,2,2)
## a$set(x)
## similarly could've done this:
## x <- matrix(1:4,2,2)
## a <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
      ## initialize i (inverse) to NULL
      i <- NULL
      set <- function(y) {
            ## When set is used the matrix passed in and the inverse
            ## set to NULL
            x <<- y
            i <<- NULL
      }
      get <- function() x
      ## Set and Get functions for the inverse
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      ## declaring a list and setting what each references
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## Solving for cached matrix
## Using example from above:
## cacheSolve(a)

cacheSolve <- function(x= matrix(), ...) {
      ## attempts to get inverse
      i <- x$getinverse()
      if(!is.null(i)) {
            ## inverse is not NULL returns inverse already stored
            message("getting cached data")
            return(i)
      }
      ## inverse is NULL retrieves matrix and solves for the inverse
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setinverse(i)
      i      
}
