##Matrix inversion is time consuming and costly operation if matrix is large enough.The caching of already calculated marix may give some benifit. 
##These pair of functions are used to create matrix, calculates and caches it's inverse.If inverse is already calculated returns it from cache.


##makeCacheMatrix function creates a "matrix", 
##which is a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverseMatrix
##get the value of the inverseMatix

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


##The cacheSolve function returns inverse of the matrix - x. 
#If inverse of matrix is already cached it returns cached value
#else it calculates inverse and caches it and returns and inversed matrix value.

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
