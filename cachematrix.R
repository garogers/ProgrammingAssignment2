## These two functions cache the inverse of a matrix
## the first function creates an object that can cache its inverse
## the second computes the inverse after first checking to see if the 
## matrix has already had its inverse calculated

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m_inv <- NULL
     set <- function(y) {
       x <<- y
       m_inv <<- NULL
     }
     get <- function() x
     set_inverse <- function(inverse) m_inv <<- inverse
     get_inverse <- function() m_inv
     list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m_inv <- x$get_inverse()
     if(!is.null(m_inv)) {
       message("getting cached inverse")
       return(m_inv)
     }
     my_data <- x$get()
     m_inv <- solve(my_data)
     x$set_inverse(m_inv)
     m_inv
}
