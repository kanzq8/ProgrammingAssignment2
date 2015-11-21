## This exercise will show how to preserve the computation result of an inverse of a matrix.
## The result will be cached, ie. the value will be saved and re-used if no change is done to the original matrix

## This function creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(matrix_original = matrix()) {

  matrix_inverse <- NULL
  
  set_original <- function(org){
    matrix_original <<- org
    matrix_inverse <<- NULL
  }
  
  get_original <- function() matrix_original
  
  set_inverse <- function(inv) matrix_inverse <<- inv
  
  get_inverse <- function() matrix_inverse
  
  list (set_original = set_original, get_original = get_original,
        set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it  gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the matrix_inverse
## in the cache via the set_inverse function.

cacheSolve <- function(m, ...) {
        
  inv <- m$get_inverse()
  
  if(!is.null(inv)){
    message("getting cashed data")
    return(inv)
  }

  data <- m$get_original()
  inv <- solve(data)
  m$set_inverse(inv)
  inv
}
