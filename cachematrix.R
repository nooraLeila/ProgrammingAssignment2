## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #set up function and empty vector 
  m <- NULL #important if there is no cache
  set <- function(y) { #set the value of the vector
    x <<- y # putting x into set function
    m <<- NULL 
  }
  get <- function() x # get the value of the vector 
  setsolve <- function(solve) m <<- solve # set the value of the matrix
  getsolve <- function() m # get the value of the matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

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
