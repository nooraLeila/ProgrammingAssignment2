## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #set up function and empty vector 
  m <- NULL #important if there is no cache
  set <- function(y) {
    x <<- y # putting x into set function
    m <<- NULL 
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve 
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() #check if there is a value for m
  if(!is.null(m)) { #if there is then return m
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) #create the inverted matrix
  x$setsolve(m)
  m
}
