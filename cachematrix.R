## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## A wrapper function which wraps a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      
      inverse <- NULL
      
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      
      get <- function(){
            x
      } 
      
      setinverse <- function(inv){
            inverse <<- inv }
      
      getinverse <- function(){
            inverse
      }
      list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
      
}

## cacheSolve: takes makeCacheMtrix as an input
## and returns the inverse of the matrix from Cache.
## If inverse is not in the cache, it calculates 
## the inverse and caches it in makeCacheMatrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
