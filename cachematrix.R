
## Create a special "cached-matrix" object that can cache the inverse of the matrix value.
## You can get the R matrix using $get() function.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns an inverse of cached-matrix x, with cashing the result in x.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m))
    return (m)
  data <- x$get()
  i <- diag(1, dim(data)[1], dim(data)[2])
  m <- solve(data, i)
  x$setinverse(m)
  m
}
