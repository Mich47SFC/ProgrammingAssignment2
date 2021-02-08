## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function give a matrix of specified dimensions and prepares
## it to be run through the cacheSolve function.  There are several ways to input
##the matrix into the cacheSolve function.  First set a file to make the matrix with
##the makeCacheMatrix; Second make the matrix within the cacheSolve function in
##itself; or Third male the matrix then run it through the cacheSolve function using
##makeCacheMatrix function
##
##The cachSolve function returns the inverse of the matrix that is inputted through
##the makeCacheMatrix function. The first time the matrix is run through the 
##cacheSolve function it is cached so that it doesn't have to be re-analyzed.


makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y){
           x <<- y
           m <<- NULL
      }
       get <- function()x
       setInvert <- function(invert) m <<-invert
       getInvert <- function() m 
       list(set = set, get = get, 
                      setInvert = setInvert, 
                       getInvert = getInvert)
}



cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
   m<- x$getInvert()
   if (!is.null(m)) {
     message("getting cached data")
     return(m)
  }
    w <- x$get()
    m<- solve(w, ...)
    x$setInvert(m)
    m
  }