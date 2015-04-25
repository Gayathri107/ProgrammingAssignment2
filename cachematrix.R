## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL #the cached value of the computed result
  
  # set function to set the source matrix
  #  Uses deep assignment opeator <<- to assign into the parent function's environment
  
  set  <- function(y){ 
    x <<- y 
    i <<- NULL  
  } 
  
  #get function to return the source matrix
  get  <- function() x 
  
  #setinverse function to set the cache
  setinverse  <- function(inverse) i  <<- inverse 
  
  #getinverse funtion to get the cached result
  getinverse  <- function() i 
  
  #now package up the functions as a list
  list(set= set, get = get,  
       setinverse = setinverse,  
       getinverse = getinverse) 

}

## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
##  user, who would not have to first use the function above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix mat used to create 'x' (e.g. x<-makeCacheMatrix(mat))
  
  #access the cached result
         i  <- x$getinverse() 
         
  #if the cached result is not NULL, return it
  if (!is.null(i)){ 
    message("getting cached data") 
    return(i) 
  } 
  
  #otherwise, the cache was NULL, so we need to calculate and cache the inverse before returning it
  data  <- x$get()        #fetch the original matrix
  i  <- solve(data, ...)  #invert it
  x$setinverse(i)         #cache the result for next time
  i                        #return the inverse
}
