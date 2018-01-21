## cbweaver 01.21.18

## makeCacheMatrix creates a list containing a function to
##   set the value of a matrix
##   get the value of a matrix
##   set the value of the inverse of the matrix
##   get the value of the inverse of the matrix
##   the value of the matrix passed into the function

makeCacheMatrix <- function(x = matrix()) {
  
  xmatrix = x
  i <- NULL
  
  setcacheddata <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  getcacheddata <- function() {
    x
  }
  setcachedinverse <- function(inverse = matrix()) {
    i <<- inverse
  }
  getcachedinverse <- function() {
    i
  }
  ## create a list with the new functions and value of x
  list(setcacheddata = setcacheddata, 
       getcacheddata = getcacheddata, 
       setcachedinverse = setcachedinverse,
       getcachedinverse = getcachedinverse, orig = xmatrix)

}


## cacheSolve computes the inverse of the special
## "matrix" regurned by makeCacheMatrix. If the inverse 
## already been calculated and the matrix has not changed
## then return the inverse from cache

cacheSolve <- function(x, ...) {

  i <- x$getcachedinverse()
  data1 <- x$getcacheddata()
  data2 <- x[[5]]
     if(!is.null(i)) {
      if (identical(data1,data2)) {
    message("getting cached data")
    return(i)
      }
    }
  #cacheddata <- x$getcacheddata()
  #message(cacheddata)
  i <- solve(data1)
  x$setcachedinverse(i)
  i
  
}
 

