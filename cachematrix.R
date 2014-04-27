## Matrix inversion can be a costly computation, especially for large matrices.
## Because of this, it can be beneficial to cache the inverse of a matrix 
## rather than recomputing it each time. Taken together, these functions 
## allow for calculating, caching and retrieving a matrix' inverse.
## (The inverse of a given matrix is the corresponding matrix whose dot product 
## with the provided matrix will yield the identify matrix.)

## For further reading on matrix inversion, see:
## [Wolfram Alpha](http://mathworld.wolfram.com/MatrixInverse.html)
## [Wikipedia](http://en.wikipedia.org/wiki/Invertible_matrix)

## We simplify our calculations by assuming that the given matrix is 
## square and invertible - which is to say that there is a unique matrix 
## which is its inverse.



###
### makeCacheMatrix
### - x (a matrix)
###
### creates a wrapper for the provided matrix which consists of a series of 
### functions, which allow for:
### - getting/setting the matrix
### - getting/setting the inverse of the matrix
### returns this list to be used as an object elsewhere.
###

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


###
### cacheSolve
### - x (a "makeCacheMatrix" object)
###
### A solve() equivalent for "makeCacheMatrix" objects. Computes the inverse 
### of the matrix object returned by "makeCacheMatrix". If the inverse 
### is stored in the cache and the matrix is unchanged, then the cached 
### inverse is returned. Otherwise, the inverse is calculated and stored 
### in the cache for later reference.
###

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}
