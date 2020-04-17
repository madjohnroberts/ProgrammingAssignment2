## Make a list that stores different parts of a "matrix object"
## List stores fucntions that access the matrix and its inverse in the global env
## Assumes the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  ## Makes a wierd object for storing a matrix and it's inverse
  ## Args: 
  ##    x (matrix): An invertible matrix 
  ## Returns:
  ##    list: Contains the set(), get(), set_inv(), and get_inv() methods
  ## The methods set() and set_inv() will store to global the following variables:
  ## set() stores its argument to "x" in global
  ## set_inv() stores its argument to "inv" in global
  ## The get() and get_inv() methods return "x" and "inv" respectively
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  return(list(set=set,
              get=get,
              set_inv=set_inv,
              get_inv=get_inv
              ))
}


cacheSolve <- function(x) {
  ## Returns a matrix that is the inverse of 'x'
  ## Checks if the inverse is in global before calculating the inverse 
  ## Args:
  ##    x (list): Weird "matrix object" consructed using "makeCacheMatrix"
  ## Returns:
  ##    matrix: inverse of the matrix stored in x
  x_inv <- x$get_inv()
  if(!is.null(x_inv)){
    message("Fetching cached inverse matrix")
    return(x_inv)
  }
  m <- x$get()
  m_inv <- solve(m)
  x$set_inv(m_inv)
  return(m_inv)
}
