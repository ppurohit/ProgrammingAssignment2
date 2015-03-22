## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  
  set <- function(mat) {
    x <<- mat
    invmat <<- NULL
  }
  
  get <- function() x
  
  setInverseMatrix <- function(inverseMatrix) invmat <<- inverseMatrix
  
  getInverseMatrix <- function() invmat
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invMat <- x$getInverseMatrix()
  
  if(!is.null(invMat)) {
    message("getting cached inverse matrix")
    
    ## Return cached inverse matrix
    return(invMat)
  }
  
  mat <- x$get()
  
  ## Calculate inverse matrix
  invMat <- solve(mat, ...)
  
  x$setInverseMatrix(invMat)
  
  ## Return inverse matrix
  invMat
  
}
