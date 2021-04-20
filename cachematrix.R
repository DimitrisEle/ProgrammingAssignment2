## The first function in the file creates an R object that stores a matrix and its inverse. The second function requires an argument that is returned by the primer in order to retrieve the inverse from the cached value. 

## Creates an R object that stores a matrix and its inverse

makeCacheMatrix <-  function(x = matrix()){
                fs <- NULL
                set <- function(y) {
                        x <<- y
                        fs <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) fs <<- inverse
                getinverse <- function() fs
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Take the cached value that is stored in the makeCacheMatrix() environment

cacheSolve <- function(x, ...) {
         fs <- x$getinverse()
         if(!is.null(fs)) {
              message("getting cached data")
              return(fs)
         }
         data <- x$get()
         fs <- solve(data, ...)
         x$setinverse(fs)
         fs
}

