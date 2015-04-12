## Caching the Inverse of a Matrix
## Cache the inverse matrix and store it in memory for fast retrival

## Read the Matrix and store the inverse in a 
## Test data: m <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2) 
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1) 
makeCacheMatrix <- function(x = matrix()) { 
     inv <- NULL   ## Initialize of inv (Inverse Matrix) = NULL
     ## Cache the input matrix y to a variable x
     ##inv: inverse of x
     set <- function(y) { 
         x <<- y              
         inv <<- NULL         
     } 
     get <- function() x 
     setinverse <- function(inverse) inv <<- inverse 
     getinverse <- function() inv 
## List of four functiona to be used     
list(set=set, 
          get=get,
          setinverse=setinverse,
          getinverse=getinverse)  
 } 
 

## cacheSolve calls makeCacheMatrix. If the inverse has already ## been calculated (and the matrix x has not changed), then ##cacheSolve retrieves the inverse from the cache inv. If the ##input is new, it calculates the inverse of the data and sets ##the inverse in the cache via the setinverse function.

 cacheSolve <- function(x, ...) { 
     inv <- x$getinverse() 
     if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
     else {
         message("Running Inverse Operation")
         data <- x$get() 
         inv <- solve(data) 
         x$setinverse(inv)  
     }
     inv 
 } 

