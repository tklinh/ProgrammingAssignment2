## These two function show how to use Lexical Scoping
## These function compute the inverse of a matrix,
## If the inverse is in the cache, cached value will be returned
## If the inverse isn't in the cache, solve function
## will be used to compute the inverse and store result in the cache

## makeCacheMatrix function has one input is a matrix
## it returns a list contains following functions
## 1. set: set a new matrix and clear cache
## 2. get: get the current matrix
## 3. setinverse: set new inverse matrix
## 4. getinverse: get cached inverse

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


## cacheSolve
## input: a list returned from makeCacheMatrix function
## it get the inverse value from cache, if inverse is not in cache
## it computes the inverse using solve function
## and store the result in the cache
## if inverse is in the cache, the cached value will be returned

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("cacheSolve -> getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
