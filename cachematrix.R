## makeCacheMatrix will store a matrix and inverse matrix. the function also returns a list of
## that get, set the matrice

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function will make an inverse matrix and store the inverse matrix in the enviroment using
## setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix)
  x$setinverse(i)
  i
}

