## This program contains two functions makeCacheMatrix and cacheSolve 
## to cache the inverse of a matrix

## makeCacheMatrix creates a matrix object to cache the inverse of
## matrix

makeCacheMatrix <- function(x = matrix()) {
  ## i initialised to null every time a new object is created
  i<-NULL
  
  ##methods to access the matrix and its inverse
  getMatrix<-function(){ x }
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getInverse<-function(){i}
  setInverse<-function(inverse)
  {
    i<<-inverse
  }
  list(getMatrix = getMatrix,setMatrix=setMatrix,
       getInverse = getInverse,setInverse =setInverse)

}


## This function calculates the inverse of the matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        
  i<-x$getInverse()
  
  ##return cached matrix if inverse already calculated
  if(!is.null(i)){
    message("caching")
    return(i)
  }
  
  ##calculate inverse for a new matrix
  data<-x$getMatrix()
  inverse<-solve(data,...)
  x$setInverse(inverse)
  return(inverse)
}
