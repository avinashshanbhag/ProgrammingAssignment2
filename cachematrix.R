## Put comments here that give an overall description of what your
## functions do

## Oct 25, 2014


## This function creates a special matrix, that also has its inverse available for use, if 
## the matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
        
        inv_matrix <- NULL
        
        set <- function(y) 
        {
                x <<- y
                inv_matrix <<- NULL
        }
        
        get <- function() x
        
        ## Sets the inverse matrix variabl in the calling environment
        setinverse <- function(inverse) inv_matrix <<- inverse
        
        getinverse <- function() inv_matrix
        
        ## returns list of available functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
        
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function retrieves the inverse from the cache.

## Input: instance of makeCacheMatrix, 
## Output: inverse matrix of the input matrix

cacheSolve <- function(x, ...) {
        
        ## Get the cached inverse matrix for the given matrix
        inv_matrix <- x$getinverse()
        
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        
        ## The inverse matrix has not been computed. So, get the matrix data
        data <- x$get()
        
        ## This function computes the inverse of the given matrix
        inv_matrix <- solve(data, ...)
        
        ## This function sets the computed inverse matrix into the cache
        x$setinverse(inv_matrix)
        
        ## returns the inverse matrix
        inv_matrix
        
}
