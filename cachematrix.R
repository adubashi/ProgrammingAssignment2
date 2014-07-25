
#Creates a matrix that can cache it's matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
  i <- NULL
  
  #Setter
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Getter
  get <- function() x
  
  #Setter - Inverse
  set_inverse <- function(inv_input) i <<- inv_input
  #Getter - Inverse
  get_inverse <- function() inv
  
  # returns methods
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}

#Computes the Inverse, unless it's stored in Cache, then simply returns
#what's in cache
cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$set_inverse(i)
  
  #returns result
  i
  
}
