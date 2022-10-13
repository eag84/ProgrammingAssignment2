## Finds and stores inverse of matrices

#Takes matrix x and initiates i to store inverse. Sets x to what is in parent
#set i as null. Gets calls local x. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#If i not null, but has been defined, return and exit
#if it has not, solves for inverse and feeds it into list

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
 if(!is.null(i)) {
   message("getting cached data")
   return(i)
  }
 data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
