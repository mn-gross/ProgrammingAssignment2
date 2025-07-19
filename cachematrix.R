##--THIS FUNCTION CHANGES, READS, STORES, and GETS INVERSE OF MATRIX--##
  #set function: replaces the matrix stored with a new matrix (y) 
    #also resets the cached inverse because the matrix changed
  #get function: returns stored matrix
  #setinverse function: caches inverse
  #getinverse function returns cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
      # NULL because no inverse right now
  set <- function(y) {
    x <<- y
    inv <<- NULL # matrix will change
  }
  get <- function() x # hands back x
  setinverse <- function(inverse) inv <<- inverse # store inverse of matrix (caches)
  getinverse <- function() inv # returns cached inverse  
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse) # returns list of four functions previously defined
}

##--THIS FUNCTION CHECKS IF INVERSE IS CACHED (RETURNS IT) or CALCULATES,CACHES, & RETURNS IT--##
cacheSolve <- function(x, ...) {
     # Return a matrix that is the inverse of 'x'
     # takes special matrix object x. "..." gives extra arguments to solve()
  inv <- x$getinverse() 
    # previous function from makeCachematrix, to check if theres an inverse
    # stores it in inv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
     # checks if inv exists and returns cached inverse and exits function early
  }
  data <- x$get()
  inv <- solve(data, ...) #computes inverse
  x$setinverse(inv) #caches it
     # calculates inverse if not yet cached
  inv
}
