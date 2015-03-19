## Put comments here that give an overall description of what your
## functions do
## The main purpose of the functions, presented below, is to take a square matrix A
## and deliver its inverse. Yet as opposed to the traditional way of finding inverse,
## the functions check whether a matrix has already been processed and if it has its inverse
## is brought back from the memory. Accordingly if the inverse of the matrix 
## is not yet done the appropriate computation is made and the inverse gets cached.

## Write a short comment describing this function
##The function reads an arbitrary square matrix A, computes its inverse,
##caches the inverse and return a special "matrix" object. In fact, this special matrix is a list of functions. 
##Two of them deal with the  matrix and the other two  manage inverse issues.
##Here Inv is an inverse matrix being cached. Essential part of the function is 
##the operator <<- . Using this operator allows to keep the  invesre beyond 
##the scope of the internal function where it's computed. Thus it's getting cached and doesn't
##disapper after the function stops working.

makeCacheMatrix <- function(x = matrix()) {
  
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
    
  }
  get <- function() x
  setInv <- function(Inv) Inv <<- solve(x)
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)  
}


## Write a short comment describing this function
##The following function calculates the inverse of the special "matrix" created above. 
##Its argument is the the special "matrix".
##As stated above if a matrix has already been inversed the inverse is retreived from the memory. 
##The esiest way to comprehend idea beyond the caching issue is to substitute <<- operator with
## <- operator in the first function. Then we rerun both the functions,firstly passing a matrix to
##the new version of makeCacheMatrix. Secondly, we pass the result to the cacheSolve and repeat it at 
##at least two times (so the same matrix is passed). Instead of retreiving the cached inverse
##the function recomputes it every time (since the inverse has not been saved previuosly). 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if (!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
  
}
