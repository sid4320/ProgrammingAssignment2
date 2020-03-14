## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
makeCacheMatrix <- function( mat = matrix() ) {

  ## Initialize the inverse property
  i <- NULL
  inv <- NULL

  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
    mat <<- matrix
    inv <<- NULL
  }

  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
    mat
  }

  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
    inv <<- inverse
  }

  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
    inv
  }

  ## Return a list of the methods
@@ -40,23 +40,23 @@ makeCacheMatrix <- function( m = matrix() ) {
cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  mat <- x$getInverse()

  ## Just return the inverse if its already set
  if( !is.null(m) ) {
  if( !is.null(mat) ) {
    message("getting cached data")
    return(m)
    return(mat)
  }

  ## Get the matrix from our object
  data <- x$get()

  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  mat <- solve(data) %*% data

  ## Set the inverse to the object
  x$setInverse(m)
  x$setInverse(mat)

  ## Return the matrix
  m
  mat
}
