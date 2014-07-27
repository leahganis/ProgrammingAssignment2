## Leah Ganis
## R Programming
## 7/26/14

## cachematrix.R
## These two functions together will cache a matrix and its inverse, 
## so that the inverse can be recalled later if it has been calculated
## previously. 


## makeCacheMatrix is a function which creates a special "matrix" object 
## which is really a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

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


## cacheSolve is a function which returns a matrix that is the 
## inverse of 'x'. If the inverse has already been calculated, 
## return the cached value. Else, calculate it, store it, and 
## return it. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

