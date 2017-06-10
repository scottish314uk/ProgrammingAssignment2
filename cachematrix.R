## cachematrix.R
## overall description of what the functions do, example:
##> source("cachematrix.R")
##> mySquareMatrix <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
##> mySquareMatrix$getCache() ## return NULL for the first time, inverse not computed yet.
##> cacheSolve(mySquareMatrix) ## return computed inverse
##> mySquareMatrix$getCache() ## return inverse from cache whcih is thesame as 
## the computed inverse (does not return NULL as initally did)
##---------------------------------------------------------------------------------------------------

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## 'cacheMatrix'
  ## assign NULL to cacheMatrix
  
  cacheMatrix <- NULL
  
  ## 'setMatrix'
  ## define setMatrix

  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  ## 'getMatrix'
  ## return the matrix 'x'
  
  getMatrix <- function() x
  
  ## 'setCache'
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  ## 'getCache'
  ## that will return the cached inverse of 'x'
  
  getCache <- function() cacheMatrix
  
  ## list the names of all methods that will be known to the outside world
 
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)

} 
 
##--------------------------------------------------------------------------------------------------
  
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
  ## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  
  ## 'cacheSolve'
  ## return the inverse of a given matrix utilizing the cache
  
  ## 'cachematrix'

  cacheMatrix <- x$getCache()
  
  ## if the content is not null then: return the result 
  
  if (!is.null(cacheMatrix)) {
    message("getting cache matrix...")
    return(cacheMatrix)
  }
  
  ## if the content is empty then: 
  ## get the matrix, create, set, update and return the cache matrix
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
  
}