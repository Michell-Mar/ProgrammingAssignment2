## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The function makeCacheMatrix is a especial function that returns a list of functions, on which we set the matrix that we want to
##solve the inverse of it.
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
               x <<- y
               inv <<- NULL
          }
     get <- function() x
     setinv <- function(solve) inv <<- solve ##we asign the inverse of the matrix to the variable "solve"
     getinv<-function()inv
           list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)

}



## Write a short comment describing this function
##So cacheSolve is a function that prints out the inverse of the matrix that we set up in the function above.
cacheSolve <- function(x, ...) {
     inv <- x$getinv() ##Here is where we can see this
    if(!is.null(inv)) {##there could be an option that we haven't calculated it, so we have a condicional
          message("getting the matrix inverse")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
        ## Return a matrix that is the inverse of 'x'
}
##This is an example of how to use the created functions, just eliminate the "#", to run it :)
#y<-rbind(c(7,5),c(-2,9))
#mi<-makeCacheMatrix()
#mi$set(y)
#mi$get()
#mi$setinv(solve(y))
#mi$getinv()
#cacheSolve(mi)
