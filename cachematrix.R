## Put comments here that give an overall description of what your
## functions do
##The makeCacheMatrix and cacheSolve functions are a pair of functions 
##that cache the inverse of a matrix.

## Write a short comment describing this function
##The makeCacheMatrix function creates a special "matrix" object that 
##can cache its inverse. MakeCacheMatrix contains four functions: set, 
##get, setinverse, getinverse. See each line of code below for a 
##line-by-line description.

makeCacheMatrix <- function(x = matrix()) {
#Generate a makeCacheMatrix function that takes a matrix as input.
  
  m <- NULL
  #m is assigned as NULL
  
  set <- function(y) {
    #set ia a function that changes the matrix stored in makeCacheMatrix function
    
    x <<- y
    #substitures matrix x with y in makeCacheMatrix function
    
    m <<- NULL 
    #restores values if mean m to null
  }
  get <- function() x
  #function that returns matrix x stored in makeCacheMatrix function
  
  setinverse <- function(solve) m <<- solve
  #store value of the input in variable m in makeCacheMatrix function
  
  getinverse <- function() m
  #return value of input in variable m in makeCacheMatrix function
  
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)
  #list function stores four functions in makeCacheMatrix function 
}

## Write a short comment describing this function
##The cacheSolve function will return a matrix that is the inverse of 'x'.
##It does so by computing the inverse of the "matrix" returned by the 
##makeCacheMatrix function. If the inverse was already calculated and the matrix 
##was not changed, the cacheSolve function should retrieve the inverse from the cache. 
##If the inverse was not calculated, the variable data gets the matrix stored with 
##in variable m and calculates, sets, and returns the inverse. 
##See each line of code below for a line-by-line description.


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  #create main function called cacheSolve, takes x as argument
  
  m <- x$getinverse()
  if(!is.null(m)) {
    #verifies that the value m stored with get inverse exisits and is not null
    
    message("getting cached data")
    return(m)
    #if cached value exisits in memory, returns value m  
  
  }
  
  data <- x$get()
  #if no cached value m exists, gets matrix stored with makeCacheMatrix function
  
  m <- solve(data, ...)
  #compute inverse of matrix
  
  x$setmean(m)
  #stores inverse matrix in object m
  
  m
  #returns m  
}