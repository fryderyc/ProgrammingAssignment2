## Way to run the cached matrix:
## A <- matrix(c(1,1,-1,2), nrow=2)
## B <- makeCacheMatrix(A)
## cacheSolve(B)
## << will output the solved result >>
## cacheSolve(B)
## << will output the solved result again, with the text "getting cached data" >>

## NOTICE: library "matlib" needed to execute successfully.


## A function to create a matrix with cache getter/setters
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Solve the matrix inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setmatrix(m)
  m
}